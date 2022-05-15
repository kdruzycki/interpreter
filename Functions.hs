{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Functions where

import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Writer.Strict

import Utils
import Globals
import AbsLatteMalinowe
import {-# SOURCE #-} Statements (execFnBlock)

-- TODO
-- pętla for bez read only
-- przesłanianie
-- oto pomysł na scoping:
--   StateT jest typu (<poziom_zagnieżdżenia>::Int, Mapa<Ident, [(<poziom_zagn>::Int, Val)]>)
--   jak wchodzimy do bloku, to zwiększamy poziom zagnieżdżenia
--   jak koniec bloku, to ze wszystkich map.values usuwamy head wtw poziom zagnieżdżenia head'a wynosi poziom zagnieżdżenia bloku
--     oraz na koniec przywracamy poziom zagnieżdżenia sprzed bloku, czyli zmniejszamy o 1
--     *ewentualnie* można nie usuwać od razu, tylko potem sprawdzać, czy aktualne zagnieżdżenie nie jest mniejsze niż head i wtedy usunąć

-- read only bedzie w typeCheckerze
-- funkcje zwracające wartość
-- sprawdzanie, czy coś jest zmienną, czy funkcją też
-- zgodność typów argumentów funkcji też
-- redeklaracja zmiennych też (w obrębie jednego bloku)
-- i redeklaracja zmiennej read-only
-- deklarację funkcji można przesłonić deklaracją zmiennej (sic!)
-- w szczególności jeśli deklaracja funkcji jest przesłonięta deklaracją zmiennej, to nie można wykonać App nazwa_funkcji
-- czy można przesłaniać argumenty funkcji??

programOutput :: Program' a -> Either String String
programOutput p = return $ (execWriter $ execProgram p) "\n"

execProgram :: Program' a -> OutputWriter ()
execProgram p = runReaderT execProgramM (createFnEnv p)

execProgramM :: ReaderT (FnEnv a) OutputWriter ()
execProgramM = do
  main <- asks $ Map.lookup (Ident "main")
  mapM_ (\fnSgn -> runFnM fnSgn []) main

runFnM :: FnSgn a -> [Val] -> ReaderT (FnEnv a) OutputWriter ()
runFnM (argIdents, b) argVals = execFnBlock b $ zip argIdents argVals

createFnEnv :: Program' a -> FnEnv a
createFnEnv (Program _ fnDefs) = Map.fromList $ map fnEnvEntry fnDefs
  where
    fnEnvEntry :: TopDef' a -> (Ident, (FnSgn a))
    fnEnvEntry (FnDef _ _ ident args block) = 
      (ident, (map argIdent args, block))
      where
        argIdent :: Arg' a -> Ident
        argIdent (Arg _ _ ident) = ident
