{-# language ViewPatterns #-}

module ReadGraph
  ( read_graph
  ) where

import qualified Data.ByteString.Lazy.Char8 as L
import Data.Char
import Data.List
import Control.Monad

intsOfLine = unfoldr (L.readInt . L.dropWhile isSpace)

-- this ignores lines with one int, eg in haskperf networks
edgeOfLine (intsOfLine -> [x,y]) = [(x,y)]
edgeOfLine _ = []

read_graph :: FilePath -> IO [(Int,Int)]
read_graph file = (edgeOfLine <=< L.lines) <$> L.readFile file
