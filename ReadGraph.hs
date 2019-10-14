{-# language ViewPatterns #-}

module ReadGraph (readGraph) where

import qualified Data.ByteString.Lazy.Char8 as L
import Data.Char
import Data.List
import Control.Monad

intsOfLine = unfoldr (L.readInt . L.dropWhile isSpace)

edgeOfLine (intsOfLine -> [x,y]) = [(x,y)]
edgeOfLine _ = []

readGraph :: FilePath -> IO [(Int,Int)]
readGraph file = (edgeOfLine <=< L.lines) <$> L.readFile file


