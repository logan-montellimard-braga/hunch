module Hunch.Runner (
    runSimulation
  , runCreation
  , printVersion
  ) where

import Hunch.Constants
import qualified Paths_hunch as Meta
import Hunch.Options.Data
import Hunch.Language.Syntax
import Hunch.Language.Parser
import Hunch.Language.PrettyPrinter

import Data.Version     (showVersion)
import Control.Monad    (when, unless)
import System.Exit      (exitSuccess, exitFailure)
import System.IO        (hPutStrLn, stderr)
import System.FilePath  (combine, joinPath, isValid)
import System.Directory (doesFileExist, doesDirectoryExist, createDirectory, copyFile)

-- Successfully terminate the program by printing a given message
terminate :: String -> IO ()
terminate str = putStrLn str >> exitSuccess

-- Forcefully exit the program by printing a given error message
fatal :: String -> IO ()
fatal str = hPutStrLn stderr ("(FATAL) " ++ str) >> exitFailure

-- Execute a given IO action and logs the given message, if wanted.
withLog :: Bool -> String -> IO () -> IO ()
withLog logB msg f = f >> when logB (putStrLn msg)

-- Execute a given IO action with the AST corresponding to the given opts.
-- Die with the returned error message if any.
withAST :: Options -> (FileSystem -> IO ()) -> IO ()
withAST opts successFn =
  case parseExp expr srcs sep token start of
    Right tree  -> do
      errors <- checkIntegrity (templates opts) tree
      if null errors
         then successFn tree
         else fatal $ formatErrors errors
    Left errMsg -> fatal errMsg
    where
      (Just expr) = input opts
      srcs        = sources opts
      sep         = delimiter opts
      token       = sigil opts
      start       = startAt opts

-- Write an empty file in target, or a file copied from the specified entry
-- template if present.
writeFileOrTpl :: FsEntry -> FilePath -> FilePath -> IO ()
writeFileOrTpl (FsEntry _ _ Default) target _        = writeFile target ""
writeFileOrTpl (FsEntry _ _ (Source tpl)) target dir = copyFile tpl' target
  where
    tpl' = dir `combine` tpl

-- Generic function to derive makeFile and makeDir functions.
genericMake :: (FilePath -> IO Bool)
            -> (FsEntry -> FilePath -> FilePath -> IO ())
            -> Options -> [FilePath] -> FsEntry -> IO ()
genericMake checkFn writeFn opts base entry =
  unless (entry == currentDir) $ do
    let path       = joinPath base `combine` (_entryNameName . _entryName) entry
        kind       = show . _entryKind $ entry
        indent     = (++) $ replicate (length base * 3) ' '
        createdMsg = kind ++ " created: " ++ path
        existsMsg  = kind ++ " already exists: " ++ path
        existsOMsg = createdMsg ++ " (overriden)"

        -- Directories are not overriden ; there's no point.
        overrideB  = (_entryKind entry == File) && override opts
        logB       = verbose opts
        tplDir     = templates opts
    exists <- checkFn path
    if not exists || overrideB
       then let msg = if exists
                         then "+= " ++ indent existsOMsg
                         else "++ " ++ indent createdMsg
            in withLog logB msg $ writeFn entry path tplDir
       else withLog logB ("== " ++ indent existsMsg)  $ return ()

-- Create a file from a given entry.
makeFile :: Options -> [FilePath] -> FsEntry -> IO ()
makeFile = genericMake doesFileExist writeFileOrTpl

-- Create a directory from a given entry.
makeDir :: Options -> [FilePath] -> FsEntry -> IO ()
makeDir = genericMake doesDirectoryExist $ \_ p _ -> createDirectory p

-- Create the given tree in the filesystem by recursively walking over its
-- children.
createTree :: Options -> [FilePath] -> FileSystem -> IO ()
createTree opts base (Node entry children) = do
  let newBasePath = base ++ currentPath
      currentPath = if entry == currentDir
                       then []
                       else [_entryNameName . _entryName $ entry]
  case _entryKind entry of
    File      -> makeFile opts base entry
    Directory -> makeDir  opts base entry
  mapM_ (createTree opts newBasePath) children

-- Check if the given template exists, from base templaate dir tplDir.
tplExists :: FilePath -> FileSystem -> IO String
tplExists tplDir node =
  if extractTpl node /= Default
     then do
       let path = tplDir `combine` (_entryTmplSource . extractTpl) node
           name = _entryNameName . _entryName . rootLabel $ node
           err  = "Template file '" ++ path ++ "' not found for file '" ++ name ++ "'"
       exists <- doesFileExist path
       return $ if exists then "" else err
     else return ""
  where
    extractTpl = _entryTmpl . rootLabel

-- Return errors if name is invalid
getNameErrors :: FileSystem -> String
getNameErrors node = if isValid filePath
                        then ""
                        else "Invalid name: '" ++ filePath ++ "'"
  where
    filePath = _entryNameName . _entryName . rootLabel $ node

-- Check the integrity of a given file system, collecting errors recursively.
checkIntegrity :: FilePath -> FileSystem -> IO [String]
checkIntegrity tplDir fs = do
  tplCheck <- tplExists tplDir fs
  childrenErrors <- mapM (checkIntegrity tplDir) $ subForest fs
  let nameCheck = getNameErrors fs
      errors    = [nameCheck, tplCheck] ++ concat childrenErrors
      errors'   = filter (not . null) errors
  return errors'

-- Format list of errors to a string with a visual list of errors.
formatErrors :: [String] -> String
formatErrors errs = "The following errors were encountered:\n" ++ errorList
  where
    errorList = unlines $ map ("   - " ++) errs


---
--- Runnners
---

runSimulation :: Options -> IO ()
runSimulation opts = withAST opts $ terminate . showTree

runCreation :: Options -> IO ()
runCreation opts = withAST opts $ \ast -> do
  let cr = if verbose opts then "\n" else ""
  createTree opts [] ast
  terminate (cr ++ "Done creating entries.")

printVersion :: Options -> IO ()
printVersion opts = do
  let base          = projectName ++ " v" ++ showVersion Meta.version
      isVerbose     = verbose opts
      repository    = projectRepo ++ " (" ++ projectLicense ++ " License)"
      info          = show projectYear ++ ", " ++ maintainerInfo
      complementary = if isVerbose
                         then "\n\n" ++ repository ++ "\n" ++ info
                         else ""
  terminate $ base ++ complementary
