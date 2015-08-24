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

import Data.Version      (showVersion)
import Data.List         (intercalate)
import Control.Monad     (when, unless, forM_)
import Control.Exception (catch)
import System.Exit       (exitSuccess, exitFailure)
import System.IO         (hPutStrLn, stderr)
import System.FilePath   (combine, joinPath, isValid, isAbsolute)
import System.Directory

-- Successfully terminate the program by printing a given message
terminate :: String -> IO ()
terminate str = putStrLn str >> exitSuccess

-- Forcefully exit the program by printing a given error message
fatal :: String -> IO ()
fatal str = hPutStrLn stderr ("(FATAL) " ++ str ++ helpText) >> exitFailure
  where
    helpText = "\n\nType `hunch --help` for more information on Hunch usage."

-- Catch exception and print it as other errors
withCatch :: IO () -> IO ()
withCatch todo = catch todo recover
  where
    recover :: IOError -> IO ()
    recover e = fatal $ intercalate "\n" ["Runtime error:", "  " ++ show e]

-- Execute a given IO action and logs the given message, if wanted.
withLog :: Bool -> String -> IO () -> IO ()
withLog logB msg f = f >> when logB (putStrLn msg)

-- Execute a given IO action with the AST corresponding to the given opts.
-- Die with the returned error message if any.
withAST :: Options -> (FileSystem -> IO ()) -> IO ()
withAST opts successFn =
  case parseExp expr srcs root sep token start of
    Right tree  -> do
      errors <- if shouldCheck
                   then checkIntegrity True (templates opts) tree
                   else return []
      if null errors
         then successFn tree
         else fatal $ formatErrors errors
    Left errMsg -> fatal errMsg
  where
    (Just expr) = input opts
    srcs        = sources opts
    root        = rootDir opts
    sep         = delimiter opts
    token       = sigil opts
    start       = startAt opts
    shouldCheck = not . noCheck $ opts

-- Recursively copy a directory.
-- No existence checks are done.
copyDir ::  FilePath -> FilePath -> IO ()
copyDir from to = do
  createDirectoryIfMissing False to
  entries <- getDirectoryContents from
  let entries' = filter (`notElem` [".", ".."]) entries
  forM_ entries' $ \name -> do
    let from' = from `combine` name
        to'   = to `combine` name
    isDirectory <- doesDirectoryExist from'
    if isDirectory then copyDir from' to' else copyFile from' to'

-- Write an empty file in target, or a file copied from the specified entry
-- template if present.
writeFileOrTpl :: FTemplate -> FilePath -> FilePath -> IO ()
writeFileOrTpl Default      target _   = writeFile target ""
writeFileOrTpl (Source tpl) target dir = copyFile (dir `combine` tpl) target

-- Create an empty dir in target, or a directory copied from the specified
-- template dir if present.
makeDirOrTpl :: FTemplate -> FilePath -> FilePath -> IO ()
makeDirOrTpl Default      target _   = createDirectory target
makeDirOrTpl (Source tpl) target dir = copyDir (dir `combine` tpl) target

-- Generic function to derive makeFile and makeDir functions.
genericMake :: (FilePath -> IO Bool)
            -> (FsEntry -> FilePath -> FilePath -> IO ())
            -> Options -> [FilePath] -> FsEntry -> IO ()
genericMake checkFn writeFn opts base entry =
  unless (entry == currentDir) $ do
    let path       = joinPath base `combine` (_entryNameName . _entryName) entry
        kindS      = show . _entryKind $ entry
        indent     = (++) $ replicate (length base * 3) ' '
        createdMsg = kindS ++ " created: " ++ path
        existsMsg  = kindS ++ " already exists: " ++ path
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
makeFile = genericMake doesFileExist (writeFileOrTpl . _entryTmpl)

-- Create a directory from a given entry.
makeDir :: Options -> [FilePath] -> FsEntry -> IO ()
makeDir = genericMake doesDirectoryExist (makeDirOrTpl . _entryTmpl)

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
getTplErrors :: FilePath -> FsEntry -> IO [String]
getTplErrors tplDir e
  | _entryTmpl e == Default   = return []
  | otherwise = do
    fileExists <- doesFileExist path
    dirExists  <- doesDirectoryExist path
    return $ if _entryKind e == File
                then if not fileExists
                        then if dirExists then [errFDir] else [errF]
                        else []
                else if not dirExists
                        then if fileExists then [errDFile] else [errD]
                        else []
  where
    path     = tplDir `combine` (_entryTmplSource . _entryTmpl) e
    errFDir  = "Template '" ++ path ++ "' is a directory, expected a file"
    errF     = "Template file '" ++ path ++ "' not found"
    errDFile = "Template '" ++ path ++ "' is a file, expected a directory"
    errD     = "Template directory '" ++ path ++ "' not found"

-- Return errors if name is invalid or path is absolute
getNameErrors :: Bool -> FsEntry -> [String]
getNameErrors isRoot node = validMsg ++ absoluteMsg
  where
    validMsg    = [validErr | not . isValid $ filePath]
    absoluteMsg = [absErr   | not isRoot && isAbsolute filePath]
    filePath    = _entryNameName . _entryName $ node
    validErr    = "Invalid name: '" ++ filePath ++ "'"
    absErr      = "Absolute path: '" ++ filePath ++ "'"

-- Check the integrity of a given file system, collecting errors recursively.
checkIntegrity :: Bool -> FilePath -> FileSystem -> IO [String]
checkIntegrity isRoot tplDir fs = do
  tplCheck <- getTplErrors tplDir $ rootLabel fs
  childrenErrors <- mapM (checkIntegrity False tplDir) $ subForest fs
  let nameCheck = getNameErrors isRoot $ rootLabel fs
      errors    = concat [nameCheck, tplCheck, concat childrenErrors]
  return errors

-- Format list of errors to a string with a visual list of errors.
formatErrors :: [String] -> String
formatErrors errs = introText ++ "\n" ++ errorList
  where
    introText        = "The following " ++ singularOrPlural ++ " encountered:"
    singularOrPlural = if length errs == 1 then "error was" else "errors were"
    errorList        = intercalate "\n" $ map ("   - " ++) errs


---
--- Runnners
---

runSimulation :: Options -> IO ()
runSimulation opts = withAST opts $ terminate . showTree

runCreation :: Options -> IO ()
runCreation opts = withAST opts $ \ast -> do
  let cr = if verbose opts then "\n" else ""
  withCatch $ createTree opts [] ast
  terminate $ cr ++ "Done creating entries."

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
