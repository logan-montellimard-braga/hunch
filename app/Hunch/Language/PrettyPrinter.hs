module Hunch.Language.PrettyPrinter (
    showTree
  ) where

import Hunch.Language.Syntax


showTree :: FileSystem -> String
showTree = unlines . draw

-- Data.Tree.draw stylistic modification for cleaner output
draw :: FileSystem -> [String]
draw (Node entry children) = show entry : drawSubTrees children
  where
    drawSubTrees []     = []
    drawSubTrees [t]    = shift "└── " "    " (draw t)
    drawSubTrees (t:ts) = shift "├── " "│   " (draw t) ++ drawSubTrees ts
    shift first other   = zipWith (++) (first : repeat other)
