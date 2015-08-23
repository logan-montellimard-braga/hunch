module Hunch.Options.CommandLine (
    withOptions
  , showHelpText
  ) where

import Options.Applicative
import Data.Monoid (mempty)

import Hunch.Constants
import Hunch.Options.Data


singleChar :: String -> ReadM Char
singleChar s | null s       = readerError "empty token"
             | length s > 1 = readerError $
                "token is too long (max 1 char.): '" ++ s ++ "'"
             | otherwise    = return $ head s

inputOpt :: Parser (Maybe String)
inputOpt = optional . argument str $ metavar "INPUT"

sourcesOpt :: Parser [String]
sourcesOpt = many . argument str $ metavar "SOURCES"

rootDirOpt :: Parser String
rootDirOpt = strOption $
     long    "root-dir"
  <> short   'r'
  <> metavar "DIR"
  <> value   ""
  <> help    "Base directory for operations"

templatesOpt :: Parser String
templatesOpt = strOption $
     long    "templates-path"
  <> short   'p'
  <> metavar "DIR"
  <> value   ""
  <> help    "Base directory for templates files lookup"

delimOpt :: Parser String
delimOpt = strOption $
     long    "delimiter"
  <> short   'd'
  <> metavar "STRING"
  <> value   ","
  <> help    "Delimiter between names in sources"
  <> showDefault

sigilOpt :: Parser Char
sigilOpt = option (str >>= singleChar) $
     long    "number-token"
  <> short   't'
  <> metavar "CHAR"
  <> value   '$'
  <> help    "Token that will be replaced in duplicate names by an incrementing counter"
  <> showDefault

startAtOpt :: Parser Int
startAtOpt = option auto $
     long    "start-at"
  <> short   'n'
  <> metavar "INT"
  <> value   0
  <> help    "First number used in the incrementing counter (see --number-token)"
  <> showDefault

overrideOpt :: Parser Bool
overrideOpt = switch $
     long    "override"
  <> short   'o'
  <> help    "Override existing files"

noChecksOpt :: Parser Bool
noChecksOpt = switch $
     long    "no-check"
  <> short   'c'
  <> help    "Do not check input for invalid names or not found templates"

verboseOpt :: Parser Bool
verboseOpt = switch $
     long    "verbose"
  <> short   'v'
  <> help    "Print informations on actions"

simulateOpt :: Parser Bool
simulateOpt = switch $
     long    "simulate"
  <> short   's'
  <> help    "Only print the visual representation of the input"

versionOpt :: Parser Bool
versionOpt = switch $
     long    "version"
  <> help    "Print version information. Can be coupled with --verbose"

parseOptions :: Parser Options
parseOptions = Options <$>
         inputOpt
     <*> sourcesOpt
     <*> rootDirOpt
     <*> templatesOpt
     <*> delimOpt
     <*> sigilOpt
     <*> startAtOpt
     <*> overrideOpt
     <*> noChecksOpt
     <*> verboseOpt
     <*> simulateOpt
     <*> versionOpt


withOptions :: (Options -> IO ()) -> IO ()
withOptions f = f =<< execParser (withInfo parseOptions)

withInfo :: Parser Options -> ParserInfo Options
withInfo opts = info (helper <*> opts) $
     fullDesc
  <> header      (projectName ++ " - " ++ projectDesc)
  <> progDesc    ""
  <> footer      ("Maintainer: " ++ maintainerInfo)
  <> failureCode 128

showHelpText :: IO ()
showHelpText = handleParseResult $ Failure failure
  where
    failure = parserFailure pprefs pinfo ShowHelpText mempty
    pprefs  = prefs mempty
    pinfo   = withInfo parseOptions
