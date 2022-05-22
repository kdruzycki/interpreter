module TypeChecker where

import Control.Monad.Except
import Control.Monad.State.Strict
import qualified Data.Map as Map
import Data.Maybe
import qualified AbsLatteMalinowe as LM

-- TODO
-- remove parametrization of types from the rest of files
-- dodać location
-- read-only bedzie w typeCheckerze
-- funkcje zwracające wartość
-- sprawdzanie, czy coś jest zmienną, czy funkcją też
-- zgodność typów argumentów funkcji też
-- redeklaracja zmiennych też (w obrębie jednego bloku)
-- i redeklaracja zmiennej read-only
-- deklarację funkcji można przesłonić deklaracją zmiennej (sic!)
-- w szczególności jeśli deklaracja funkcji jest przesłonięta deklaracją zmiennej, to nie można wykonać App nazwa_funkcji
-- czy można przesłaniać argumenty funkcji??
-- próba wypisania voida / funkcji void
-- próba wykonania operacji na voidzie
-- "redeclaration of xxx in the same scope"
-- czy argumenty funkcji nie nazywają się tak samo?

-- koncepcja:
-- korzystamy z LM, ale przez BNFCPosition nie używamy Eq, tylko piszemy własne coś a'la eq dla tych typów

data Type = VType LM.Type | FnType LM.Type [LM.Type]
  deriving Show
-- TODO instance Eq dla Type ^
type RetType = Maybe LM.Type
type ScopeLevel = Int
type TypeHoverings = [(ScopeLevel, Type)]
type IdentSpace = Map.Map LM.Ident TypeHoverings
type TCState = (RetType, ScopeLevel, IdentSpace)
type TypeChecker = ExceptT String (State TCState) ()

typeCheck :: LM.Program -> Either String ()
typeCheck p = evalState (runExceptT $ typeCheckM p) (Nothing, 0, Map.empty)  

typeCheckM :: LM.Program -> TypeChecker
typeCheckM (LM.Program _ fnDefs) = do
  declFns fnDefs
  checkFns fnDefs
  -- po czym w pętli znów przechodzimy przez każdą funkcję i wtedy deklarujemy jej argumenty
    -- sprawdzamy, czy argumenty mają unikalne nazwy
  return ()

declFns :: [LM.TopDef] -> TypeChecker
declFns [] = return ()
declFns ((LM.FnDef _ type_ ident args block):fns) = do
  unavailable <- gets $ currLvlOccupied ident
  if unavailable
    then throwError $ "Function already declared: " ++ show ident
    else do
      modify $ withIdent ident (FnType type_ (map (\(LM.Arg _ type_ _) -> type_) args))
      declFns fns

-- funkcja to dodatkowy level, a blok funkcji to już kolejny - todo w Functions.hs
checkFns :: [LM.TopDef] -> TypeChecker
checkFns [] = return ()
checkFns ((LM.FnDef _ type_ ident args block):fns) = do
  modify $ lvlUp
  declArgs ident args
  -- checkBlock
  modify $ lvlDown
  checkFns fns

declArgs :: LM.Ident -> [LM.Arg] -> TypeChecker
declArgs fnIdent [] = return ()
declArgs fnIdent ((LM.Arg _ type_ ident):args) = do
  unavailable <- gets $ currLvlOccupied ident
  if unavailable
    then throwError $ "Function " ++ (show fnIdent) ++ " has multiple arguments with the same name " ++ (show ident)
    else do
      modify $ withIdent ident (VType type_)
      declArgs fnIdent args

-- Utils --

scopeLvl (_, lvl, _) = lvl
mapScopeLvl f (x, lvl, y) = (x, f lvl, y)
retType (rt, _, _) = rt
setRetType rt (_, x, y) = (rt, x, y)
currLvlOccupied ident (_, lvl, idSpace) =
  case Map.lookup ident idSpace of
    Nothing -> False
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