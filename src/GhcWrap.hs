{-# LANGUAGE ScopedTypeVariables #-}

module GhcWrap (launch) where
import GHC
import GHC.Paths
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Dynamic
import System.FilePath


-- | Get a module name from the source.
getModNameFromSrc :: String -> String -> Ghc ModuleName
getModNameFromSrc fileSrc fileName = do
  dflags <- getSessionDynFlags
  let (warningMessages, Right locWrapper) = parser fileSrc dflags fileName 
      hsModule = unLoc locWrapper
      (Just locModName) = hsmodName hsModule
      modName = unLoc locModName
  return modName


-- | Add an interactive import to the context.
addToContext :: InteractiveImport -> Ghc ()
addToContext x = do
  ctx <- getContext
  setContext (x:ctx)

-- | Load and import a module specified by an import statement.
runImportStmt :: String -> Ghc ()
runImportStmt stmt = do
  -- Parse the import statement
  importDecl <- parseImportDecl stmt

  -- Get the module name and create the target
  let modName = unLoc . ideclName $ importDecl
      modTargetId = TargetModule modName
      modTarget = Target modTargetId True Nothing

  -- Add the target and load it
  addTarget modTarget
  load $ LoadUpTo modName

  -- Add the import to the interactive context
  addToContext $ IIDecl importDecl


initSession :: IO HscEnv
initSession = runGhc (Just libdir) $ do
  -- Setup the session dynamic flags
  dflags <- getSessionDynFlags
  setSessionDynFlags $ dflags { hscTarget = HscInterpreted
                              , ghcLink   = LinkInMemory }
  
  -- Load the Prelude into the context. I don't know if there's anything
  -- in the context before this.
  setContext [IIDecl $ simpleImportDecl (mkModuleName "Prelude")]

  -- Run the import statement
  runImportStmt "import Example (locCommand)"
  
  -- Get the module graph, which now contains MainTest, the module
  -- that Example.hs contains
  modGraph <- getModuleGraph
  -- Pull the individual module summaries out of the graph
  let modSums = mgModSummaries modGraph
  
  -- You can print all of the file names of the module summaries
  -- (Or whatever other property you want to print)
  liftIO $ mapM_ print (map ms_hspp_file modSums)

  -- Return the environment
  env <- getSession
  return env


-- | Run something inside the environment. Return the updated enviornment
-- and the result of what was run.
session :: HscEnv -> Ghc a -> IO (HscEnv, a)
session env m = runGhc (Just libdir) $ do
  setSession env
  a <- m
  env <- getSession
  return (env,a)

-- | Evaluate a string as an expression. Creates a monad that returns the
-- expression's value or fails if the expression was not of the expected
-- type.
eval :: Typeable a => String -> Ghc a
eval inp = do
  (Just (exp::a)) <- fromDynamic <$> dynCompileExpr inp
  return exp

-- | Takes a string, compiles it, and forces it to be an IO action (I think).
-- Then uses 'liftIO' to lift it up and run it as part of the GHC monad.
evalIO :: Typeable a => String -> Ghc a
evalIO inp = do
  act :: IO a <- eval inp
  liftIO act

-- | Launch a session. Temporary/placeholder.
-- TODO: Expand.
launch :: IO ()
launch = do
  env <- initSession
  (env', act::Int) <- session env $ evalIO "locCommand"
  print act
  return ()
