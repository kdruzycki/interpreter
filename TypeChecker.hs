module TypeChecker where

import Control.Monad.Except
import Control.Monad.State.Strict
import qualified Data.Map as Map
import Data.Maybe
import qualified AbsLatteMalinowe as LM
import Utils

data Type = VType LM.Type | FnType LM.Type [LM.Type]
  deriving Show
type RetType = Maybe LM.Type
type ScopeLevel = Int
type TypeHoverings = [(ScopeLevel, Type)]
type IdentSpace = Map.Map LM.Ident TypeHoverings
type TCState = (RetType, ScopeLevel, IdentSpace)
type TypeChecker = ExceptT String (State TCState)

typeCheck :: LM.Program -> Either String ()
typeCheck p = evalState (runExceptT $ typeCheckM p) (Nothing, 0, Map.empty)  

typeCheckM :: LM.Program -> TypeChecker ()
typeCheckM (LM.Program _ fnDefs) = do
  declFns fnDefs
  checkFns fnDefs
  return ()

declFns :: [LM.TopDef] -> TypeChecker ()
declFns [] = return ()
declFns ((LM.FnDef _ type_ ident args block):fns) = do
  unavailable <- gets $ currLvlOccupied ident
  if unavailable
    then throwError $ "Function already declared: " ++ show ident
    else do
      modify $ withIdent ident (FnType type_ (map (\(LM.Arg _ type_ _) -> type_) args))
      declFns fns

checkFns :: [LM.TopDef] -> TypeChecker ()
checkFns [] = return ()
checkFns ((LM.FnDef _ type_ ident args block):fns) = do
  modify $ lvlUp
  declArgs ident args
  modify $ setRetType (Just type_)
  ret <- checkBlock block
  if isNothing ret && not (isVoid type_)
    then throwError $ "Function " ++ (show ident) ++ " of type " ++ (show type_) ++ " lacks a return statement"
    else do
      modify $ setRetType Nothing
      modify $ lvlDown
      checkFns fns

declArgs :: LM.Ident -> [LM.Arg] -> TypeChecker ()
declArgs fnIdent [] = return ()
declArgs fnIdent ((LM.Arg _ type_ ident):args) = do
  unavailable <- gets $ currLvlOccupied ident
  if unavailable
    then throwError $ "Function " ++ (show fnIdent) ++ " has multiple arguments with the same name " ++ (show ident)
    else do
      modify $ withIdent ident (VType type_)
      declArgs fnIdent args

checkBlock :: LM.Block -> TypeChecker (RetType)
checkBlock (LM.Block _ stmts) = do
  modify $ lvlUp
  ret <- untilJust $ map checkStmt stmts
  modify $ lvlDown
  return ret

checkStmt :: LM.Stmt -> TypeChecker (RetType)
checkStmt s = case s of
  LM.BStmt _ b -> checkBlock b
  LM.Cond _ e b -> (exprType e) >>= (assertBool "if") >> checkBlock b
  LM.CondElse _ e b1 b2 -> (exprType e) >>= (assertBool "if") >> checkBlock b1 >> checkBlock b2
  LM.While _ e b -> (exprType e) >>= (assertBool "while") >> checkBlock b
  LM.For _ ident e1 e2 b -> do
    t1 <- exprType e1
    if not $ sameTypes t1 (LM.Int Nothing)
      then throwError $ "For loop pointer " ++ (show ident) ++ " cannot be initialized with a value of type " ++ (show t1)
      else do
        t2 <- exprType e2
        if not $ sameTypes t2 (LM.Int Nothing)
          then throwError $ "For loop pointer " ++ (show ident) ++ " cannot be bound with a finishing value of type " ++ (show t2)
          else do
            modify $ lvlUp
            modify $ withIdent ident (VType $ LM.Int Nothing)
            ret <- checkBlock b
            modify $ lvlDown
            return ret
  LM.Ret _ e -> do
    rt <- gets $ retType
    t <- exprType e
    if sameTypes t $ fromJust rt
      then return rt
      else throwError $ "Function of type " ++ (show (fromJust rt)) ++ " cannot return a value of type " ++ (show t)
  LM.VRet _ ->do
    rt <- gets $ retType
    if isVoid (fromJust rt)
      then return rt
      else throwError $ "Function of type " ++ (show (fromJust rt)) ++ " cannot return with no value"
  _ -> do
    case s of
      LM.SExp _ e -> (exprType e) >> (return ())
      LM.Decl _ type_ items -> processSeq (declVar type_) items
      LM.Print _ e -> (exprType e) >> (return ())
      LM.Ass _ ident e -> do
        it <- gets $ identType ident
        if isNothing it
          then throwError $ "Variable " ++ (show ident) ++ " cannot be implicitly declared"
          else do
            vt <- exprType e
            it' <- fromVType ident $ fromJust it
            if sameTypes it' vt
              then return ()
              else throwError $ "Variable " ++ (show ident) ++ " assignment expected a value of type " ++ (show it) ++ ", got " ++ (show vt)
      LM.Incr _ ident -> do
        it <- gets $ identType ident
        if isNothing it
          then throwError $ "Variable " ++ (show ident) ++ " cannot be implicitly declared"
          else do
            it' <- fromVType ident $ fromJust it
            if sameTypes it' (LM.Int Nothing)
              then return ()
              else throwError $ "Variable " ++ (show ident) ++ " of type " ++ (show it') ++ " cannot be incremented"
      LM.Decr _ ident -> do
        it <- gets $ identType ident
        if isNothing it
          then throwError $ "Variable " ++ (show ident) ++ " cannot be implicitly declared"
          else do
            it' <- fromVType ident $ fromJust it
            if sameTypes it' (LM.Int Nothing)
              then return ()
              else throwError $ "Variable " ++ (show ident) ++ " of type " ++ (show it') ++ " cannot be decremented"
      _ -> return ()
    return Nothing

exprType :: LM.Expr -> TypeChecker (LM.Type)
exprType e = case e of
  LM.LitInt _ _ -> return $ LM.Int Nothing
  LM.LitTrue _ -> return $ LM.Bool Nothing
  LM.LitFalse _ -> return $ LM.Bool Nothing
  LM.LitString _ _ -> return $ LM.Str Nothing
  LM.Var _ ident -> assertVar ident 
  LM.App _ ident es -> do
    args <- mapM exprType es
    assertApp ident args
  LM.Neg _ e -> (exprType e) >>= (assertInt "-")
  LM.Not _ e -> (exprType e) >>= (assertBool "!")
  LM.And _ e1 e2 -> (exprType e1) >>= (assertBool "&&") >> (exprType e2) >>= (assertBool "&&")
  LM.Or  _ e1 e2 -> (exprType e1) >>= (assertBool "||") >> (exprType e2) >>= (assertBool "||")
  LM.Rel _ e1 op e2 -> do
    t1 <- exprType e1
    t2 <- exprType e2
    assertNotVoid op t1
    assertNotVoid op t2
    assert op t1 t2
    return $ LM.Bool Nothing
  LM.Mul _ e1 op e2 -> do
    t1 <- exprType e1
    t2 <- exprType e2
    assertMul t1 t2
  LM.Add _ e1 op e2 -> do
    t1 <- exprType e1
    t2 <- exprType e2
    assertNotBoolVoid op t1
    assertNotBoolVoid op t2
    assert op t1 t2

assertVar :: LM.Ident -> TypeChecker (LM.Type)
assertVar ident = do
  t <- gets $ identType ident
  if isNothing t
    then throwError $ "Variable " ++ (show ident) ++ " does not exist"
    else case fromJust t of
      FnType _ _ -> throwError $ (show ident) ++ " is a function, not a variable"
      VType vt -> return vt

assertApp :: LM.Ident -> [LM.Type] -> TypeChecker (LM.Type)
assertApp ident args = do
  t <- gets $ identType ident
  if isNothing t
    then throwError $ "Function " ++ (show ident) ++ " does not exist"
    else case fromJust t of
      VType _ -> throwError $ (show ident) ++ " is a variable, not a function"
      FnType fnType argTypes -> if length args /= length argTypes
        then throwError $ "Function " ++ (show ident) ++ " expected " ++ (show $ length argTypes) ++ " arguments, got " ++ (show $ length args)
        else do
          mapM (\(t1, t2) -> assertArgType ident t1 t2) (zip argTypes args)
          return fnType

assert :: Show a => a -> LM.Type -> LM.Type -> TypeChecker (LM.Type)
assert op texp tact = if sameTypes texp tact
  then return tact
  else throwError $ "Operator/statement " ++ (show op) ++ " expected type " ++ (show texp) ++ " but got " ++ (show tact)
  
assertArgType :: LM.Ident -> LM.Type -> LM.Type -> TypeChecker (LM.Type)
assertArgType ident texp tact = if sameTypes texp tact
  then return tact
  else throwError $ "Function " ++ (show ident) ++ " expected an argument of type " ++ (show texp) ++ " but got " ++ (show tact)

assertInt op = assert op (LM.Int Nothing)
assertBool op = assert op (LM.Bool Nothing)

assertNotVoid :: Show a => a -> LM.Type -> TypeChecker (LM.Type)
assertNotVoid op t = case t of
  LM.Void _ -> throwError $ "Operator " ++ (show op) ++ " cannot be applied to Void"
  _ -> return t

assertNotBoolVoid :: Show a => a -> LM.Type -> TypeChecker (LM.Type)
assertNotBoolVoid op t = case t of
  LM.Bool _ -> throwError $ "Operator " ++ (show op) ++ " cannot be applied to Bool"
  _ -> assertNotVoid op t

assertMul :: LM.Type -> LM.Type -> TypeChecker (LM.Type)
assertMul t1 t2 = case t1 of
  LM.Str _ -> case t2 of
    LM.Int _ -> return t1
    _ -> err
  LM.Int _ -> case t2 of 
    LM.Str _ -> return t2
    LM.Int _ -> return t1
    _ -> err
  where
    err :: TypeChecker (LM.Type)
    err = throwError $ "Operator * cannot be applied to " ++ (show t1) ++ " and " ++ (show t2)

declVar :: LM.Type -> LM.Item -> TypeChecker ()
declVar type_ item = do
  case item of
    LM.NoInit _ ident -> decl ident type_
    LM.Init _ ident e -> do
      vtype <- exprType e
      if sameTypes type_ vtype
        then decl ident type_
        else throwError $ "Variable " ++ (show ident) ++ " initialization required a value of type " ++ (show type_) ++ ", got " ++ (show vtype)
  where
    decl :: LM.Ident -> LM.Type -> TypeChecker ()
    decl ident type_ = do
      unavailable <- gets $ currLvlOccupied ident
      if unavailable
        then throwError $ "Variable " ++ (show ident) ++ " cannot be redeclared in the same scope"
        else modify $ withIdent ident (VType type_)

fromVType :: LM.Ident -> Type -> TypeChecker (LM.Type)
fromVType ident t = case t of
  VType vt -> return vt
  _ -> throwError $ (show ident) ++ " is a function and cannot be assigned a value"

-- Helper Functions --

scopeLvl (_, lvl, _) = lvl

mapScopeLvl f (x, lvl, y) = (x, f lvl, y)

retType (rt, _, _) = rt

setRetType rt (_, x, y) = (rt, x, y)

currLvlOccupied ident (_, lvl, idSpace) =
  case Map.lookup ident idSpace of
    Nothing -> False
    Just [] -> False
    Just ((currLvl, _):_) -> currLvl == lvl

withIdent ident type_ (x, lvl, idSpace) =
  (x, lvl, Map.alter (decl lvl type_) ident idSpace) where
    decl lvl type_ types = Just $ (lvl, type_) : (fromMaybe [] types)

identType ident (_, _, idSpace) = case Map.findWithDefault [] ident idSpace of
    [] -> Nothing
    (_, type_):_ -> Just type_

lvlUp = mapScopeLvl (+1)

lvlDown (x, lvl, idSpace) =
  (x, lvl - 1, Map.map (rmLvl lvl) idSpace) where
    rmLvl _ [] = []
    rmLvl lvl xs@((currLvl, _):ys) = if currLvl == lvl then rmLvl lvl ys else xs

sameTypes (LM.Int _) (LM.Int _) = True
sameTypes (LM.Str _) (LM.Str _) = True
sameTypes (LM.Bool _) (LM.Bool _) = True
sameTypes _ _ = False

isVoid (LM.Void _) = True
isVoid _ = False