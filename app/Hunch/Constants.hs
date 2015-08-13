module Hunch.Constants where

maintainerName :: String
maintainerName = "Logan Braga"

maintainerMail :: String
maintainerMail = "<" ++ email ++ ">"
  where
    email   = address ++ "@" ++ host
    address = "braga.logan"
    host    = "gmail.com"

maintainerInfo :: String
maintainerInfo = maintainerName ++ " " ++ maintainerMail

projectName :: String
projectName = "Hunch"

projectDesc :: String
projectDesc = "Terse syntax for file system manipulation"

projectYear :: Integer
projectYear = 2015

projectRepo :: String
projectRepo = "github.com/loganbraga/hunch"

projectLicense :: String
projectLicense = "MIT"
