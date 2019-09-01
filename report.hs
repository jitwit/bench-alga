{-# language TupleSections, LambdaCase, BangPatterns #-}

module Main where

import Criterion.Main
import System.Environment
import qualified Data.Graph.Inductive.Query as FGL
import qualified Data.Graph.Inductive.PatriciaTree as FGL
import qualified Data.Graph.Inductive.Graph as FGL
import qualified Data.Graph as LG
import qualified Algebra.Graph.AdjacencyMap.Algorithm as AM
import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Algebra.Graph.AdjacencyIntMap.Algorithm as AIM
import qualified Algebra.Graph.AdjacencyIntMap as AIM
import Data.Bifunctor
import qualified Data.Graph.Typed as KL
import Data.List hiding (transpose)
import Data.Tree
import Data.Foldable

hamming x y = length $ filter not $ zipWith (==) x y

fgl_of_alga :: AIM.AdjacencyIntMap -> FGL.UGr
fgl_of_alga g = FGL.mkUGraph vs es where
  vs = AIM.vertexList g
  es = AIM.edgeList g

kl_of_alga :: AIM.AdjacencyIntMap -> LG.Graph
kl_of_alga g = LG.buildG (minimum vs, maximum vs) es where
  vs = AIM.vertexList g
  es = AIM.edgeList g

-- build equivalent string and int versions of the word graph
sgb wds = (word_graph, int_graph) where
  int_graph = AIM.edges $ map (bimap fst fst) es
  word_graph = AM.edges $ map (bimap snd snd) es
  es = [ (iu,jv) | iu@(i,u) <- ws, jv@(j,v) <- ws, hamming u v == 1 ]
  ws = zip [0..] wds

read_real_world :: String -> AIM.AdjacencyIntMap
read_real_world = AIM.edges . map edge_of_line . lines where
  edge_of_line line = case map read (words line) of
                        [s,t] -> (s,t)

mkMesh = (filter (\(x,y) -> x < n' && y < n') $ concatMap
         (\x -> let first = if (x+1) `mod` sq == 0 then [] else [(x,x+1)]
                    second = if x+sq >= sq^(2 :: Int) then [] else [(x,x+sq)]
                 in first ++ second)
              [0..(sq^(2::Int))])
  where
    sq = 1 + sq'
    sq' = round (sqrt $ fromRational $ toRational n' :: Double)
    n = 4
    n' = 10^n :: Int

-- old alga definitions
kldfs g = KL.dfsForest (KL.fromAdjacencyMap g)
kldfs' g = KL.dfsForest (KL.fromAdjacencyIntMap g)

words_graph = do
  wds <- lines <$> readFile "sgb-words.txt"
  let (!gw,!gi) = sgb wds
      !es = AIM.edgeList gi
      !kl = LG.buildG (0,length wds - 1) es
      !sgv = AIM.vertexList gi
      !sge = AIM.edgeList gi
      !lggb = LG.buildG (minimum sgv,maximum sgv) sge
      !fglgb = FGL.mkUGraph sgv sge :: FGL.UGr
  withArgs ["-o","words-graph.html","--csv","words-graph.csv"] $ defaultMain
    [ bgroup "String"
      [ bench "new alga bfsForest" $ nf AM.bfsForest gw
      , bench "new alga dfsForest" $ nf AM.dfsForest gw
      , bench "old alga dfsForest" $ nf kldfs gw
      ]
    , bgroup "Int"
      [ bench "new alga bfs" $ nf (AIM.bfs [1]) gi
      , bench "fgl bfs" $ nf (FGL.bfs 1) fglgb
      , bench "new alga dfsForest" $ nf AIM.dfsForest gi
      , bench "old alga dfsForest" $ nf kldfs' gi
      , bench "Data.Graph dff" $ nf LG.dff lggb
      , bench "fgl dff'" $ nf FGL.dff' fglgb ] ]

misc_graphs = do
  rwaim <- read_real_world <$> readFile "real_world.txt"
  let !circuit = AIM.circuit [1..10000]
      !clique = AIM.clique [1..500]
      !circuit_fgl = fgl_of_alga circuit
      !clique_fgl = fgl_of_alga clique
      !circuit_kl = kl_of_alga circuit
      !clique_kl = kl_of_alga clique
      !rw_fgl = fgl_of_alga rwaim
      !rw_kl = kl_of_alga rwaim
      !mesh = AIM.edges mkMesh
      !mesh_fgl = fgl_of_alga mesh
      !mesh_kl = kl_of_alga mesh
  withArgs ["-o", "misc-graphs.html","--csv","misc-graphs.csv"] $ defaultMain
    [ bgroup "circuit 10000"
      [ bench "new alga dfsForest" $ nf AIM.dfsForest circuit
      , bench "old alga dfsForest" $ nf kldfs' circuit
      , bench "fgl dff'" $ nf FGL.dff' circuit_fgl
      , bench "containers dff" $ nf LG.dff circuit_kl ]
    , bgroup "mesh 10000"
      [ bench "new alga dfsForest" $ nf AIM.dfsForest mesh
      , bench "old alga dfsForest" $ nf kldfs' mesh
      , bench "fgl dff'" $ nf FGL.dff' mesh_fgl
      , bench "containers dff" $ nf LG.dff mesh_kl ]
    , bgroup "clique 500"
      [ bench "new alga dfsForest" $ nf AIM.dfsForest clique
      , bench "old alga dfsForest" $ nf kldfs' clique
      , bench "fgl dff'" $ nf FGL.dff' clique_fgl
      , bench "containers dff" $ nf LG.dff clique_kl ]
    , bgroup "'real world'"
      [ bench "new alga dfsForest" $ nf AIM.dfsForest rwaim
      , bench "old alga dfsForest" $ nf kldfs' rwaim
      , bench "fgl dff'" $ nf FGL.dff' rw_fgl
      , bench "containers dff" $ nf LG.dff rw_kl ] ]
  
main = do
  words_graph
  misc_graphs
