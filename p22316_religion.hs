-- http://www.spoj.com/problems/RELIGION/
-- http://cs.stackexchange.com/questions/44577/proving-algorithm-for-removing-nodes-from-a-complete-graph-with-two-kinds-of-edg
-- WA as of 21.07.2015, because description is extremely vague and leaves a lot of interpetation choices (todo: contact author)

{-# LANGUAGE RecordWildCards #-}

import Data.List as L
import Data.Array as A
import Data.IntSet as S
import Data.IntMap.Strict as M
import Data.Ord (comparing)
import Data.Function (on)
import Control.Monad
import Control.Applicative

data NodeConnectivity = NodeConnectivity
    { ncAdjCount :: Int
    , ncAdj      :: IntSet
    , ncNonAdj   :: IntSet }

data Graph = Graph
    { grEdgeCount    :: Int
    , grConnectivity :: Array Int NodeConnectivity }

vertexCoverCliqueExists :: Graph -> Bool
vertexCoverCliqueExists Graph{..} = go vertices M.empty S.empty grEdgeCount
  where
    vertices = L.map fst
        $ L.sortBy (comparing (negate . ncAdjCount . snd))
        $ A.assocs grConnectivity
    go _ _ _ 0 = True
    go (v : vs) adjcc forbidden n
        | S.member v forbidden = False
        | otherwise = go vs adjcc' forbidden' n'
      where
        NodeConnectivity{..} = grConnectivity A.! v
        adjcc' = S.foldr' (\k m -> M.insertWith (+) k 1 m) adjcc ncAdj
        forbidden' = S.union ncNonAdj forbidden
        n' = n - ncAdjCount + maybe 0 id (M.lookup v adjcc)

ixfill :: Ord i => (a -> i) -> (i -> i) -> (i -> a) -> (i, i) -> [a] -> [a]
ixfill getix nextix fillix (start, end) xs = go start xs
  where
    go i []
        | i <= end  = fillix i : go (nextix i) []
        | otherwise = []
    go i (x : xs)
        | getix x > i = fillix i : go (nextix i) (x : xs)
        | otherwise   = x : go (nextix i) xs

data Actor = Tapaswi | Pk

pk :: Int -> Int -> [(Int, Int)] -> (Actor, Actor)
pk 2 _ _ = (Pk, Tapaswi)
pk cityCount passageCount passages
    | vertexCoverCliqueExists graph = (Tapaswi, Pk)
    | otherwise = (Tapaswi, Tapaswi)
  where
    graph = Graph passageCount $ A.array bounds
        $ ixfill fst succ (\i -> (i, isolatedNC)) bounds
        $ L.map (\(v, vs) -> (v, mkNC vs)) neighbourhood
    neighbourhood = L.map mergeEdges
        $ L.groupBy ((==) `on` fst) $ L.sortBy (comparing fst)
        $ L.concatMap (\(x, y) -> [(x, y), (y, x)]) passages
    mergeEdges ((s, t) : edges) = (s, t : L.map snd edges)
    isolatedNC = NodeConnectivity 0 S.empty cities
    cities = S.fromList [0 .. cityCount - 1]
    bounds = (0, cityCount - 1)
    mkNC vs = NodeConnectivity (S.size s) s s'
      where
        s  = S.fromList vs
        s' = S.difference cities s

lpair :: [t] -> (t, t)
lpair [x, y] = (x, y)

readPair :: Read a => IO (a, a)
readPair = lpair . L.map read . words <$> getLine

main :: IO ()
main = do
    t <- readLn
    replicateM_ t $ do
        (cityCount, passageCount) <- readPair
        passages <- replicateM passageCount readPair
        let (winner, controlOwner) = pk cityCount passageCount passages
        putStrLn $ case winner of
            Tapaswi -> "Tapaswi Maharaj triumphs"
            Pk -> "pK triumphs"
        putStrLn $ case controlOwner of
            Tapaswi -> "Remote control lies with Tapaswi"
            Pk -> "pK gets back his Remote control"
