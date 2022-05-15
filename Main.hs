-- File generated by the BNF Converter (bnfc 2.9.4).

-- | Program to test parser.

module Main where

import Prelude
import Data.Either ( isLeft, fromLeft )
import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import Control.Monad      ( when, return )
import Control.Monad.Writer.Lazy

import LexLatteMalinowe   ( Token, mkPosToken )
import ParLatteMalinowe   ( pProgram, myLexer )
import PrintLatteMalinowe ( Print, printTree )
import Globals
import Functions -- ( interpret )

-- type Err        = Either String
type ParseFun a = [Token] -> Err a
type Verbosity  = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: Verbosity -> FilePath -> IO ()
runFile v f = putStrLn f >> readFile f >>= run v

run :: Verbosity -> String -> IO ()
run v s =
  case pProgram ts of
    Left err -> do
      putStrLn "\nParse              Failed...\n"
      putStrV v "Tokens:"
      mapM_ (putStrV v . showPosToken . mkPosToken) ts
      putStrLn err
      exitFailure
    Right tree -> do
      putStrLn "\nParse Successful!"
      showTree v tree
      -- typeCheck tree
      let program = execProgram tree
      putStr $ snd program "\n"
      putStrLn $ showString "Program resulted with: " $ show $ fst program
  where
  ts = myLexer s
  showPosToken ((l,c),t) = concat [ show l, ":", show c, "\t", show t ]

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree = do
  putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
  putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    , "  -s (files)      Silent mode. Parse content of files silently."
    ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    []         -> getContents >>= run 2
    "-s":fs    -> mapM_ (runFile 0) fs
    fs         -> mapM_ (runFile 2) fs
