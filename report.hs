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
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Bifunctor
import qualified Data.Graph.Typed as KL
import Data.List hiding (transpose)
import System.Directory
import System.FilePath
import System.Random
import System.Random.Shuffle
import Text.Printf
import qualified Data.Array.Unboxed as A
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
  aim <- read_real_world <$> readFile file
  return (aim, fgl_of_alga aim, kl_of_alga aim)

networks_from_file = graphs_from_file . ("asp/Networks/"++)

real_world_networks = listDirectory "asp/Networks"

-- old alga definitions
kldfs g = KL.dfsForest (KL.fromAdjacencyMap g)
kldfs' g = KL.dfsForest (KL.fromAdjacencyIntMap g)
kltop g = case KL.topSort (KL.fromAdjacencyIntMap g) of
  vs -> if AIM.isTopSortOf vs g then Just vs else Nothing

klscc :: Ord a => AM.AdjacencyMap a -> AM.AdjacencyMap (AM.AdjacencyMap a)
klscc m = AM.gmap (component Map.!) $ removeSelfLoops $ AM.gmap (leader Map.!) m
  where
    KL.GraphKL g decode _ = KL.fromAdjacencyMap m
    sccs      = map toList (LG.scc g)
    leader    = Map.fromList [ (decode y, x)      | x:xs <- sccs, y <- x:xs ]
    component = Map.fromList [ (x, expand (x:xs)) | x:xs <- sccs ]
    expand xs = AM.induce (`Set.member` s) m
      where
        s = Set.fromList (map decode xs)

-- Remove all self loops from a graph.
removeSelfLoops :: Ord a => AM.AdjacencyMap a -> AM.AdjacencyMap a
removeSelfLoops m = foldr (\x -> AM.removeEdge x x) m (AM.vertexList m)
  
graph_stats :: AIM.AdjacencyIntMap -> (Int,Int,Double)
graph_stats g = (e,v,d) where
  e = length $ AIM.edgeList g
  v = length $ AIM.vertexList g
  v' = fromIntegral v
  d = fromIntegral e / v' / (v'-1)

summarize_graph file = do
  (g,_,_) <- networks_from_file file
  let (e,v,d) = graph_stats g
  return $ unwords [ file,show e, show v, printf "%.3f" d ]

summarize_graphs = mapM summarize_graph =<< real_world_networks

-- permutes vertices since alga's topSort considers them in sorted order
make_acyclic g = do
  let vs = AIM.vertexList g
  ps <- shuffleM vs
  let arr = A.listArray (minimum vs, maximum vs) ps :: A.UArray Int Int
  return $ AIM.edges [ (arr A.! min x y, arr A.! max x y) | (x,y) <- AIM.edgeList g, x /= y ]

dfsgroup_of_real_world_network file = do
  (!alga,!fgl,!kl) <- networks_from_file file
  return $ bgroup file [ bench "new-alga" $ nf AIM.dfsForest alga
                       , bench "old-alga" $ nf kldfs' alga
                       , bench "fgl" $ nf FGL.dff' fgl
                       , bench "kl" $ nf LG.dff kl ]

--bfsgroup_of_real_world_network file = do
--  (!alga,!fgl,!kl) <- networks_from_file file
--  return $ bgroup file [ bench "new-alga" $ nf (AIM.bfsForest [0]) alga
--                       , bench "fgl" $ nf (FGL.bft 0) fgl ]

topgroup_of_real_world_network file = do
  (!alga,!fgl,!kl) <- networks_from_file file
  return $ bgroup file [ bench "new-alga" $ nf AIM.topSort alga
                       , bench "old-alga" $ nf kltop alga
                       , bench "kl" $ nf LG.topSort kl
                       , bench "fgl" $ nf FGL.topsort' fgl ]

scc_of_fb = do
  (!alga,!fgl,!kl) <- graphs_from_file "facebook_combined.txt"
  let !am_alga = AM.edges $ AIM.edgeList alga
  return $ bgroup "fb" [ bench "old-alga" $ nf klscc am_alga
                       , bench "ord-alga" $ nf AM.scc am_alga
                       , bench "new-alga" $ nf AIM.scc alga
                       ]

sccgroup_of_real_world_network file = do
  (!alga,!fgl,!kl) <- networks_from_file file
  let !am_alga = AM.edges $ AIM.edgeList alga
  return $ bgroup file [ bench "old-alga" $ nf klscc am_alga
                       , bench "ord-alga" $ nf AM.scc am_alga
                       , bench "new-alga" $ nf AIM.scc alga
                       ]

daggroup_of_real_world_network file = do
  (!alga,_,_) <- networks_from_file file
  dalga <- make_acyclic alga
  let !fgl = fgl_of_alga dalga
      !kl = kl_of_alga dalga
  return $ bgroup file [ bench "new-alga" $ nf AIM.topSort dalga
                       , bench "old-alga" $ nf kltop dalga
                       , bench "kl" $ nf LG.topSort kl
                       , bench "fgl" $ nf FGL.topsort' fgl ]

depth_first_bench = do
  groups <- mapM dfsgroup_of_real_world_network =<< real_world_networks
  withArgs ["-o", "depth-first-bench.html","--json","depth-first-bench.json"] $
    defaultMain groups

--breadth_first_bench = do
--  groups <- mapM bfsgroup_of_real_world_network =<< real_world_networks
--  withArgs ["-o", "breadth-first-bench.html","--json","breadth-first-bench.json"] $
--    defaultMain groups

top_sort_bench = do
  groups <- mapM topgroup_of_real_world_network =<< real_world_networks
  withArgs ["-o", "topological-bench.html","--json","topological-bench.json"] $
    defaultMain groups

top_sort_dag_bench = do
  groups <- mapM daggroup_of_real_world_network =<< real_world_networks
  withArgs ["-o", "dag-topological-bench.html","--json","dag-topological-bench.json"] $
    defaultMain groups

scc_bench = do
--  groups <- mapM sccgroup_of_real_world_network =<< real_world_networks
  fb_group <- scc_of_fb
  withArgs ["-o", "scc-bench.html","--json","scc-bench.json"] $
    defaultMain [fb_group]

write_summary = writeFile "graphs_summary.txt" . unlines =<< summarize_graphs

sgb_bench = do
  (am,aim) <- sgb . words <$> readFile "sgb-words.txt"
  return (aim)
  
main = do
--  write_summary
  scc_bench
--  depth_first_bench
--  breadth_first_bench
--  top_sort_bench
--  top_sort_dag_bench

