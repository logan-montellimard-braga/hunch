module Hunch.Language.Parser (
    parseExp
  ) where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String  (Parser)
import Text.Parsec.Error   (showErrorMessages, errorMessages)
import Data.Tree           (unfoldTreeM)
import Data.Map            (Map, fromList, findWithDefault)
import Data.List           (genericReplicate, group, groupBy, sortBy, nubBy, partition)
import Data.List.Split     (splitOn)
import Data.Function       (on)
import Text.Printf         (printf)
import System.FilePath     (combine)
import Control.Applicative ((<$>), (<*), (*>))

import Hunch.Language.Syntax
import Hunch.Language.Lexer
import Hunch.Language.Error


---
--- PARSER ---
--- Parse user input
---

-- Decompose a file name into a tuple containing its base name
-- and wether it is the name of a directory.
decomposeName :: String -> (String, Bool)
decomposeName str = (if str == "/" then str else entryName, isDir)
  where
    isDir      = not (null str) && (last str == '/')
    entryName  = if isDir then init str else str

-- Parses an identifier followed by a slash.
-- Used to parse explicitely declared directories.
identifierDir :: Parser String
identifierDir = identifier >> string "/"

-- Parses a non-deterministic name that will be drawn from external sources.
-- Exemple: _?source
extName :: Parser (FName, Bool)
extName = do
  skip $ string "_?"
  idn <- identifier
     <|> identifierDir
  let (idn', isDir) = decomposeName idn
  return (ExtName idn', isDir)

-- Parses a standard, given name, inside single or double quotes.
-- If the name is not comprised of reserved characters, it can be expressed
-- without quotes.
standardName :: Parser (FName, Bool)
standardName = do
  idn <- identifier
     <|> identifierDir
     <|> doubleQString
     <|> singleQString
  let (idn', isDir) = decomposeName idn
  return (Name idn', isDir)

-- Parses a name for an item: a not-yet-determined name or a regular one.
fname :: Parser (FName, Bool)
fname = extName <|> standardName

-- Parses a template declaration from an '=' sign and a valid standard name.
-- Exemple: ='/home/foo/bar.txt'
template :: Parser FTemplate
template = do
  skip $ betweenDouble spaces $ char '='
  (Name source, _) <- standardName <?> "standard name"
  return $ Source source

-- Parses a template declaration or its absence.
ftemplate :: Parser FTemplate
ftemplate = template <|> return Default

-- An entry is composed of a name and optional source file. Type (file or
-- directory) is explicit or infered later.
fsentry :: Parser FsEntry
fsentry = do
  (entryName, isDir) <- fname <?> "entry name"
  tpl <- ftemplate <?> "entry template source"
  let t = if isDir then Directory else File
  return $ FsEntry entryName t tpl


-- Parses top-level expressions connected with operators.
-- Example: F my_file + D my_dir > F files * 3
expressions :: Parser Op
expressions = buildExpressionParser operationsTable expr
  where
    binary operator f = Infix $ reservedOp operator >> return f
    operationsTable   = [ [ binary "*" Repeat AssocLeft ]
                        , [ binary ">" Child AssocRight
                          , binary "+" Sibling AssocRight
                          ]
                        ]
    expr = parens expressions
       <|> Item  <$> fsentry
       <|> Times <$> repeatNumber
       <?> "file system entry or composed operation"
    repeatNumber = betweenDouble spaces decimal



---
--- TRANSFORMER ---
--- Transform parsed AST into a more-suitable tree for further processing
---

-- Unfolding function used to get a tree from an Op AST
-- Monadic version, suitable for unfoldTreeM
unfoldFnM :: Op -> Either String (FsEntry, [Op])
unfoldFnM op = case op of
  (Child _ (Times _))          -> Left $ logicalError numChildErr
  (Child (Times _) _)          -> Left $ logicalError numParentErr
  (Child (Item item) child)    -> Right (toDir item, [child])
  (Child _ _)                  -> Left $ logicalError nestedErr

  (Sibling _ (Times _))        -> Left $ logicalError numSiblingErr
  (Sibling (Times _) _)        -> Left $ logicalError numSiblingErr
  (Sibling exp1 exp2)          -> Right (currentDir, [exp1, exp2])

  (Repeat (Times _) (Times _)) -> Left $ logicalError twoNumsErr
  (Repeat expr (Times n))      -> Right (currentDir, genericReplicate n expr)
  (Repeat (Times n) expr)      -> Right (currentDir, genericReplicate n expr)
  (Repeat _ _)                 -> Left $ logicalError twoExpsErr

  (Item item)                  -> Right (item, [])
  (Times _)                    -> Left $ logicalError loneNumberErr
  where
    toDir (FsEntry str _ tpl) = FsEntry str Directory tpl

-- Transform an OP AST into a suitable Tree
toTree :: Op -> Either String FileSystem
toTree = unfoldTreeM unfoldFnM

-- Get rid of ParseError type for a simple String in Either monad
-- ParseError was needed for Parsec error details (line, col, expectations),
-- but won't be useful for further processing.
convert :: Either ParseError Op -> Either String Op
convert (Right op) = Right op
convert (Left err) = Left $ parseError col msg
  where
    f <#> (a,b,c,d,e) = f a b c d e
    msg = showErrorMessages <#> errorMessageComponents $ errorMessages err
    col = sourceColumn . errorPos $ err

-- Wrap root node into a "currentDir" (./) node if it's not already the case
wrapRoot :: FilePath -> FileSystem -> FileSystem
wrapRoot root fs@(Node e c)
  | e == currentDir' = fs
  | e == currentDir  = Node currentDir' c
  | otherwise        = Node currentDir' [fs]
  where
    currentDir' = currentDir { _entryName = Name newName }
    newName     = (_entryNameName . _entryName $ currentDir) `combine` root

-- Clean the given tree by flattening trees whose rootLabel is the currentDir
-- This effectively removes any unneeded nesting
clean :: FileSystem -> [FileSystem]
clean t@(Node entry children) = if entry == currentDir
                                   then children
                                   else [t]

-- Recursively clean a tree by mapping over its children
-- This ensures that all unneeded nesting are removed on every levels
cleanse :: FileSystem -> FileSystem
cleanse (Node entry children) = Node entry children'
  where
    children'  = concat $ fmap clean children''
    children'' = fmap cleanse children


--
-- Duplicates entries numbering
--

-- Extract the string name of a file system entry.
extractName :: FileSystem -> String
extractName = _entryNameName . _entryName . rootLabel

-- Update the name of a filesystem entry.
updateName :: FileSystem -> String -> FileSystem
updateName (Node e c) name = Node (e { _entryName = Name name }) c

-- Sort and group a list of file systems based on their names
dups :: FsForest -> [FsForest]
dups xs = groupBy ((==) `on` extractName) . sortBy (compare `on` extractName) $ xs

-- Add formatted number n on string s, replacing every consecutive occurence
-- of sigil with n padded with leading 0s corresponding to the number of
-- consecutive sigils used.
-- Example: addNumbers '$' "foo$$$" 1 => "foo001"
addNumbers :: Char -> String -> Int -> String
addNumbers sigil s n = concat . fmap numbering $ group s
  where
    numbering str = if isNumbering str then padZeros (length str) n else str
    isNumbering   = all (== sigil)
    padZeros i    = printf $ "%0" ++ show i ++ "d"

-- Add incrementing, formatted numbering to groups of duplicates FileSystem
-- entries in place of given sigil.
numberForest :: Char -> Int -> FsForest -> FsForest
numberForest sigil n ts = nub' . concat . fmap addNumbering $ dups ts
  where
    nub'              = nubBy ((==) `on` extractName)
    addNumbering fss  = zipWith (numberName sigil) fss [n..]
    numberName c fs i = updateName fs $ addNumbers c (extractName fs) i

-- Apply numbering to a given FileSystem starting at start, effectively
-- removing duplicates by replacing given sigil with incrementing numbers on
-- duplicate elements in the same file system location.
applyNumbering :: Char -> Int -> FileSystem -> FileSystem
applyNumbering sigil start (Node entry children) = Node entry children'
  where
    children'  = concat $ fmap (numberForest sigil start) [children'']
    children'' = fmap (applyNumbering sigil start) children


--
-- External sources replacement
--

-- SourcesDict are maps indexing external source names with their identifier.
type SourcesDict = Map String [String]

-- Takes a list of strings and separates them into a map two by two where the
-- first string is the key for the external sources and the second contains
-- all this source values separated by a given separator.
sourcesToMap :: [String] -> String -> SourcesDict
sourcesToMap sources sep = fromList $ extract sources
  where
    extract :: [String] -> [(String, [String])]
    extract (idn:src:xs) = (idn, splitOn sep src) : extract xs
    extract _            = []

-- Separates a list of FileSystem on the type of their name (fixed name or
-- name to be drawn from an external source).
sepByNameType :: FsForest -> (FsForest, FsForest)
sepByNameType = partition $ isExternalName . _entryName . rootLabel
  where
    isExternalName (ExtName _) = True
    isExternalName _           = False

-- Replace all occurences of an external source name on the same file system
-- level with the corresponding names provided by srcs.
replaceSources :: SourcesDict -> FsForest -> FsForest
replaceSources srcs ts = zipWith updateName ts names
  where
    names = findWithDefault [] idx srcs
    idx   = extractName $ head ts

-- Merge entries having a fixed names with entries having an external source
-- name, replacing those if needed.
mergeEntries :: SourcesDict -> FsForest -> FsForest
mergeEntries srcs ts = fixed ++ replacedSrcs
  where
    replacedSrcs  = concat . fmap (replaceSources srcs) $ dups exts
    (exts, fixed) = sepByNameType ts

-- Effectively replace external names in a given filesystem.
replaceExtNames :: SourcesDict -> FileSystem -> FileSystem
replaceExtNames sources (Node entry children) = Node entry children'
  where
    children'  = concat $ fmap (mergeEntries sources) [children'']
    children'' = fmap (replaceExtNames sources) children



-- Public interface
-- Parses a given string with above rules and transforms it
parseExp :: String -> [String] -> FilePath -> String -> Char -> Int
         -> Either String FileSystem
parseExp input srcs root sep sigil startAt = transform <$> ast
  where
    transform   = rename . cleanup
    rename      = applyNumbering sigil startAt . replaceExtNames dict
    dict        = sourcesToMap srcs sep
    cleanup     = wrapRoot root . cleanse
    ast         = toTree =<< convert parseResult
    parseResult = parse parser parserName input
    parser      = spaces *> expressions <* eof
    parserName  = "<stdin>"
