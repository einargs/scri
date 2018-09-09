module GhcWrap (launch) where
import GHC
import GHC.Paths
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Dynamic



initSession :: IO HscEnv
initSession = runGhc (Just libdir) $ do
  -- Setup the session dynamic flags
  dflags <- getSessionDynFlags
  setSessionDynFlags $ dflags { hscTarget = HscInterpreted
                              , ghcLink   = LinkInMemory }
  
  -- Load the Prelude into the context. I don't know if there's anything
  -- in the context before this.
  setContext [IIDecl $ simpleImportDecl (mkModuleName "Prelude")]

  -- Load Example.hs from the root of the project
  target <- guessTarget "Example.hs" Nothing
  addTarget target
  load LoadAllTargets

  -- Get the module graph, which now contains MainTest, the module
  -- that Example.hs contains
  modGraph <- getModuleGraph
  -- Pull the individual module summaries out of the graph
  let modSums = mgModSummaries modGraph
  
  -- You can print all of the file names of the module summaries
  -- (Or whatever other property you want to print)
  liftIO $ mapM_ print (map ms_hspp_file modSums)

  -- The module name for the example module 
  let exampleModSum:_ = modSums
      exampleModName = ms_mod_name modSum
  
  -- You can print the module name with
  -- @liftIO $ print $ moduleNameString modName@
  -- The output should be @"MainTest"@ (quotes included because of 'print')
  
  -- Append the module name of Example.hs to the imports into the interactive
  -- context.
  ctx <- getContext
  setContext ((IIModule modName):ctx)

  -- Return the environment
  env <- getSession
  return env


session :: HscEnv -> Ghc a -> IO HscEnv
session env m = runGhc (Just libdir) $ do
  setSession env
  m
  env <- getSession
  return env

-- | Takes a string, compiles it, and forces it to be an IO action (I think).
-- Then uses 'liftIO' to lift it up and run it as part of the GHC monad.
evalIO :: String -> Ghc ()
evalIO inp = do
  (Just act) <- fromDynamic <$> dynCompileExpr inp
  liftIO act

-- | Launch a session. Temporary/placeholder.
-- TODO: Expand.
launch :: IO ()
launch = do
  env <- initSession
  act <- session env $ evalIO "locCommand"
  return ()
