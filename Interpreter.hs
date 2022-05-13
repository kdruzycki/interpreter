{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Interpreter where

import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Monad.Trans.Writer.Lazy

import Utils
import Globals
import Evaluator (eval)
import AbsLatteMalinowe

interpret :: Show a => Program' a -> Result
interpret = transProgram

transProgram :: Show a => Program' a -> Result
transProgram x = case x of
  AbsLatteMalinowe.Program _ topdefs -> Right VVoid

-- transTopDef :: Show a => AbsLatteMalinowe.TopDef' a -> Result
-- transTopDef x = case x of
--   AbsLatteMalinowe.FnDef _ type_ ident args block -> failure x

-- transArg :: Show a => AbsLatteMalinowe.Arg' a -> Result
-- transArg x = case x of
--   AbsLatteMalinowe.Arg _ type_ ident -> failure x

-- transBlock :: Show a => AbsLatteMalinowe.Block' a -> Result
-- transBlock x = case x of
--   AbsLatteMalinowe.Block _ stmts -> failure x

execBlock :: Block' a -> IO ()
execBlock b = putStr $ (evalState (execWriterT (execBlockM b)) Map.empty) "\n"

execBlockM :: Block' a -> WriterT ShowS (State IdentEnv) ()
execBlockM (Block _ stmts) = processSeq execStmtM stmts

execStmtM :: Stmt' a -> WriterT ShowS (State IdentEnv) ()
execStmtM s = case s of
  BStmt _ block -> execBlockM block
  -- Cond _ expr block -> if (fromVBool $ eval expr) then execBlockM block else return ()
  -- AbsLatteMalinowe.CondElse _ expr block1 block2 -> failure x
  OrdStmt _ os -> case os of
    Decl _ type_ items -> processSeq (declVarM type_) items
    Print _ expr -> printM expr
    _ -> return ()
  _ -> return ()
  
printM :: Expr' a -> WriterT ShowS (State IdentEnv) ()
printM expr = do
  v <- gets $ runReader (eval expr)
  tell $ case v of
    VBool _ -> shows $ fromVBool v
    VInt _ -> shows $ fromVInt v
    VStr _ -> showString $ fromVStr v

declVarM :: Type' a -> Item' a -> WriterT ShowS (State IdentEnv) ()
declVarM type_ item = case item of
  NoInit _ ident    -> (return $ defaultVal type_)    >>= (decl ident)
  Init _ ident expr -> (gets $ runReader (eval expr)) >>= (decl ident)
  where
    decl :: Ident -> Val -> WriterT ShowS (State IdentEnv) ()
    decl i v = modify $ Map.insert i v

-- declVarM :: Type' a -> Item' a -> WriterT ShowS (State IdentEnv) ()
-- declVarM type_ item = case item of 
--   NoInit _ ident    -> decl (return $ defaultVal type_) ident
--   Init _ ident expr -> decl (gets $ runReader (eval expr)) ident
--   where
--     decl :: WriterT ShowS (State IdentEnv) () -> Ident -> WriterT ShowS (State IdentEnv) ()
--     decl m i = do
--       v <- m
--       modify $ Map.insert i v

-- transStmt :: Show a => AbsLatteMalinowe.Stmt' a -> Result
-- transStmt x = case x of
--   AbsLatteMalinowe.BStmt _ block -> failure x
--   AbsLatteMalinowe.Cond _ expr block -> failure x
--   AbsLatteMalinowe.CondElse _ expr block1 block2 -> failure x
--   AbsLatteMalinowe.OrdStmt _ ordstmt -> failure x

-- transOrdStmt :: Show a => AbsLatteMalinowe.OrdStmt' a -> Result
-- transOrdStmt x = case x of
--   AbsLatteMalinowe.While _ expr lblock -> failure x
--   AbsLatteMalinowe.For _ ident expr1 expr2 lblock -> failure x
--   AbsLatteMalinowe.Empty _ -> failure x
--   AbsLatteMalinowe.Decl _ type_ items -> failure x
--   AbsLatteMalinowe.Ass _ ident expr -> failure x
--   AbsLatteMalinowe.Incr _ ident -> failure x
--   AbsLatteMalinowe.Decr _ ident -> failure x
--   AbsLatteMalinowe.Ret _ expr -> failure x
--   AbsLatteMalinowe.VRet _ -> failure x
--   AbsLatteMalinowe.Print _ expr -> failure x
--   AbsLatteMalinowe.SExp _ expr -> failure x

-- transItem :: Show a => AbsLatteMalinowe.Item' a -> Result
-- transItem x = case x of
--   AbsLatteMalinowe.NoInit _ ident -> failure x
--   AbsLatteMalinowe.Init _ ident expr -> failure x

-- transLBlock :: Show a => AbsLatteMalinowe.LBlock' a -> Result
-- transLBlock x = case x of
--   AbsLatteMalinowe.LBlock _ lstmts -> failure x

-- transLStmt :: Show a => AbsLatteMalinowe.LStmt' a -> Result
-- transLStmt x = case x of
--   AbsLatteMalinowe.LOrdStmt _ ordstmt -> failure x
--   AbsLatteMalinowe.LBStmt _ lblock -> failure x
--   AbsLatteMalinowe.LCond _ expr lblock -> failure x
--   AbsLatteMalinowe.LCondElse _ expr lblock1 lblock2 -> failure x
--   AbsLatteMalinowe.LBreak _ -> failure x
--   AbsLatteMalinowe.LContinue _ -> failure x

-- transType :: Show a => AbsLatteMalinowe.Type' a -> Result
-- transType x = case x of
--   AbsLatteMalinowe.Int _ -> failure x
--   AbsLatteMalinowe.Str _ -> failure x
--   AbsLatteMalinowe.Bool _ -> failure x
--   AbsLatteMalinowe.Void _ -> failure x

