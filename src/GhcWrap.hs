module GhcWrap (launch) where
import GHC
import GHC.Paths
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Dynamic
import System.FilePath


-- | Import a file and shepard it through the compilation process.
-- This is to allow easy access to the module name without having
-- to mess around with the depedency graph in possibly breakable
-- ways.
--
-- There is a very good chance that this is a fairly bad way of doing
-- this, but it will have to suffice for the time being.
--
-- Note that @fileName@ is only read in for the purposes of being passed
-- to the parser interface. 
-- 
-- NOTE: It may not be possible to shepard compilation without invoking
-- the load target stuff and potentially duplicating effort. It seems as
-- though this really comes down to whether I can generate a 'ModSummary'
-- on my own or not, seeing as 'parseModule', 'typecheckModule', and
-- 'loadModule' seem to allow me to do so on my own.
--
-- To find an answer to this question I should continue reading through
-- that "Dive into GHC" series of blog posts.
--
-- I should also consider whether or not it might be better to just
-- use the load target stuff for now instead of being a perfectionist.
-- Now that I can properly access the module name, I'm in a much better
-- position (though honestly, there is a small chance none of this was
-- necessary. It helped me learn though, so it's not terrible).
compileSrc :: String -> String -> Ghc ()
compileSrc fileSrc fileName = do
  dflags <- getSessionDynFlags
  let (warningMessages, Right locWrapper) = parser fileSrc dflags fileName 
      hsModule = unLoc locWrapper
      (Just locModName) = hsmodName hsModule
      modName = unLoc locModName
  return ()


importFile :: String -> Ghc ()
importFile path = do
  fileSrc <- liftIO $ readFile path
  let fileName = takeFileName path
  result <- compileSrc fileSrc fileName
  return ()


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
      exampleModName = ms_mod_name exampleModSum
  
  -- You can print the module name with
  -- @liftIO $ print $ moduleNameString modName@
  -- The output should be @"MainTest"@ (quotes included because of 'print')
  
  -- Append the module name of Example.hs to the imports into the interactive
  -- context.
  ctx <- getContext
  setContext ((IIModule exampleModName):ctx)

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
