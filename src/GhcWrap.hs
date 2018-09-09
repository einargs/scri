module GhcWrap (launch) where
import GHC
import GHC.Paths
import Control.Monad.IO.Class (liftIO)
import Data.Dynamic



initSession :: IO HscEnv
initSession = runGhc (Just libdir) $ do
  dflags <- getSessionDynFlags
  setSessionDynFlags $ dflags { hscTarget = HscInterpreted
                              , ghcLink   = LinkInMemory }
  target <- guessTarget "Example.hs" Nothing
  setContext [IIDecl $ simpleImportDecl (mkModuleName "Prelude")]
  env <- getSession
  return env

session :: HscEnv -> Ghc a -> IO HscEnv
session env m = runGhc (Just libdir) $ do
  setSession env
  m
  env <- getSession
  return env

evalIO :: String -> Ghc ()
evalIO inp = do
  (Just act) <- fromDynamic <$> dynCompileExpr inp
  liftIO act

launch :: IO ()
launch = do
  env <- initSession
  act <- session env $ evalIO "putStrLn \"Hey\""
  return ()
