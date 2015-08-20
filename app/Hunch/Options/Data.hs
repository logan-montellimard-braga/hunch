module Hunch.Options.Data where

data Options = Options
  { input     :: Maybe String
  , sources   :: [String]
  , rootDir   :: String
  , templates :: String
  , delimiter :: String
  , sigil     :: Char
  , startAt   :: Int
  , override  :: Bool
  , verbose   :: Bool
  , simulate  :: Bool
  , version   :: Bool }
  deriving (Show, Read)
