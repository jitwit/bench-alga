{-# language ViewPatterns, TupleSections, LambdaCase, BangPatterns #-}

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
import System.Directory
import System.FilePath
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

-- drop 2 because graphs start by giving # of vertices and edges
read_real_world :: String -> AIM.AdjacencyIntMap
read_real_world = AIM.edges . map edge_of_line . drop 2 . lines where
  edge_of_line (map read . words -> [s,t]) = (s,t)

graphs_from_file file = do
  let file_path = "asp/Networks/" <> file
  aim <- read_real_world <$> readFile file_path
  return (file, aim, fgl_of_alga aim, kl_of_alga aim)

real_world_networks = do
  listDirectory "asp/Networks"

-- old alga definitions
kldfs g = KL.dfsForest (KL.fromAdjacencyMap g)
kldfs' g = KL.dfsForest (KL.fromAdjacencyIntMap g)
kltop g = case KL.topSort (KL.fromAdjacencyIntMap g) of
  vs -> if AIM.isTopSortOf vs g then Just vs else Nothing 
make_acyclic g = AIM.edges [ (min x y, max x y) | (x,y) <- AIM.edgeList g, x /= y ]

dfsgroup_of_real_world_network file = do
  (!nm,!alga,!fgl,!kl) <- graphs_from_file file
  return $ bgroup nm [ bench "new-alga" $ nf AIM.dfsForest alga
                     , bench "old-alga" $ nf kldfs' alga
                     , bench "fgl" $ nf FGL.dff' fgl
                     , bench "kl" $ nf LG.dff kl ]

bfsgroup_of_real_world_network file = do
  (!nm,!alga,!fgl,!kl) <- graphs_from_file file
  return $ bgroup nm [ bench "alga" $ nf (AIM.bfsForestFrom [0]) alga
                     , bench "fgl" $ nf (FGL.bft 0) fgl ]

topgroup_of_real_world_network file = do
  (!nm,!alga,!fgl,!kl) <- graphs_from_file file
  return $ bgroup nm [ bench "new-alga" $ nf AIM.topSort alga
                     , bench "old-alga" $ nf kltop alga
                     , bench "kl" $ nf LG.topSort kl
                     , bench "fgl" $ nf FGL.topsort fgl
                     ]

daggroup_of_real_world_network file = do
  (!nm,!alga,_,_) <- graphs_from_file file
  let !dalga = make_acyclic alga
      !fgl = fgl_of_alga dalga
      !kl = kl_of_alga dalga
  return $ bgroup nm [ bench "new-alga" $ nf AIM.topSort dalga
                     , bench "old-alga" $ nf kltop dalga
                     , bench "kl" $ nf LG.topSort kl
                     , bench "fgl" $ nf FGL.topsort fgl ]

depth_first_bench = do
  groups <- mapM dfsgroup_of_real_world_network =<< real_world_networks
  withArgs ["-o", "depth-first-bench.html","--json","depth-first-bench.json"] $
    defaultMain $ groups

breadth_first_bench = do
  groups <- mapM bfsgroup_of_real_world_network =<< real_world_networks
  withArgs ["-o", "breadth-first-bench.html","--json","breadth-first-bench.json"] $
    defaultMain $ groups

top_sort_bench = do
  groups <- mapM topgroup_of_real_world_network =<< real_world_networks
  withArgs ["-o", "topological-bench.html","--json","topological-bench.json"] $
    defaultMain $ groups

top_sort_dag_bench = do
  groups <- mapM daggroup_of_real_world_network =<< real_world_networks
  withArgs ["-o", "dag-topological-bench.html","--json","dag-topological-bench.json"] $
    defaultMain $ groups
  
main = do
--  depth_first_bench
--  breadth_first_bench
--  top_sort_bench
  top_sort_dag_bench
