module Hunch.Language.Syntax (
    FileSystem
  , FsForest
  , currentDir
  , Tree(..)
  , FsEntry(..)
  , FKind(..)
  , FName(..)
  , FTemplate(..)
  , Op(..)
  ) where

import Data.Tree


-- FileSystem tree.
-- A fs tree comprises of a FsEntry metadata object and a list of trees.
-- If the entry is a file, the list is empty.
type FileSystem = Tree FsEntry
type FsForest   = [FileSystem]

-- Represents a file system entry (eg: a file or a directory).
-- Files can have a 'source' template keeping track of the base content they
-- should be created with.
data FsEntry
    = FsEntry { _entryName :: FName
              , _entryKind :: FKind
              , _entryTmpl :: FTemplate
              }
    deriving (Eq)

-- Represents a file system entry name.
-- ExtNames are names not yet determined that will be drawn from external
-- sources.
data FName
    = Name    { _entryNameName :: String }
    | ExtName { _entryNameName :: String }
    deriving (Eq)

-- Represents a file system entry type.
data FKind
    = File
    | Directory
    deriving (Eq)

-- Represents a file system entry base template.
-- For now, same as a Maybe ADT, but will be used to pass more information in
-- the future.
data FTemplate
    = Default
    | Source { _entryTmplSource :: String }
    deriving (Eq)


-- Internal: current directory ("./" on unix systems) representation
currentDir :: FsEntry
currentDir = FsEntry (Name ".") Directory Default


-- Represents a tree-like operation for manipulating the file system.
data Op
    = Child   Op Op
    | Sibling Op Op
    | Repeat  Op Op
    | Times   Integer
    | Item    FsEntry


---
--- Instances declaration ---
---
instance Show FsEntry   where show = showEntry
instance Show FName     where show = showName
instance Show FKind     where show = showKind
instance Show FTemplate where show = showTemplate
instance Show Op        where show = showOp

showName :: FName -> String
showName (Name str)    = str
showName (ExtName str) = "(" ++ str ++ ")"

showTemplate :: FTemplate -> String
showTemplate (Source src) = " <- " ++ src
showTemplate Default      = ""

showKind :: FKind -> String
showKind Directory = "Dir."
showKind File      = "File"

showEntry :: FsEntry -> String
showEntry (FsEntry str k tpl) = show str ++ kindSuffix k ++ show tpl
  where
    kindSuffix Directory = "/"
    kindSuffix File      = ""

showOp :: Op -> String
showOp = concat . lines . strOp
  where
    strOp (Child op1 op2)        = show op1 ++ " > " ++ show op2
    strOp (Sibling op1 op2)      = show op1 ++ " + " ++ show op2
    strOp (Repeat x y)           = show x ++ " * " ++ show y
    strOp (Item item)            = show item
    strOp (Times n)              = show n
