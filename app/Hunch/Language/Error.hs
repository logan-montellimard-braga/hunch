module Hunch.Language.Error (
    logicalError, parseError
  , errorMessageComponents
  , numChildErr, numParentErr, numSiblingErr
  , nestedErr, twoExpsErr, twoNumsErr, loneNumberErr
  ) where

import Data.List (intercalate)


indent :: String -> String
indent str = intercalate "\n" . map ("  " ++) $ lines str

logicalError :: String -> String
logicalError msg = intercalate "\n" [title, indent msg]
  where
    title = "Logical error:"

parseError :: Int -> String -> String
parseError col msg = intercalate "\n" [title, indent msg']
  where
    title    = "Parse error at column " ++ show col ++ ":"
    msg'     = intercalate "\n" . map addDot $ filter (not . null) $ lines msg
    addDot s = if last s == '.' then s else s ++ "."

-- Components used by Parsec showErrorMessages to generate error messages
errorMessageComponents :: (String, String, String, String, String)
errorMessageComponents = ( "or"
                         , "Unknown parse error"
                         , "Expected"
                         , "Unexpected"
                         , "end of input" )

---
--- Logical error messages ---
---
numberErr :: String -> String
numberErr msg = intercalate "\n" [numErrHeader, msg, numErrFooter]
  where
    numErrHeader = "Number literals can only be used with the repeat operator."
    numErrFooter = "Put the number between quotes if it should be an entry name."

numChildErr :: String
numChildErr = numberErr "They can't be children of a directory."

numParentErr :: String
numParentErr = numberErr "They can't be parents of other file system entries."

numSiblingErr :: String
numSiblingErr = numberErr "They can't be siblings of other file system entries."

nestedErr :: String
nestedErr = "File system entries can't have more than one parent."

twoExpsErr :: String
twoExpsErr = "Two expressions can't be repeated together."

twoNumsErr :: String
twoNumsErr = "Two numbers can't be repeated together."

loneNumberErr :: String
loneNumberErr = "Standalone number litteral."
