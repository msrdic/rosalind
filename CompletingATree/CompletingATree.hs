module CompletingATree where

import           Data.List   (nub, partition)
import           Debug.Trace

-- this should be easy: count the number of isolated graphs and number of
-- edges is that number minus 1

type Node = Int
type Edge = (Node, Node)

realData :: String
realData = "rosalind_tree.txt"

sampleData :: String
sampleData = "sample.data"

toPair :: [String] -> Edge
toPair [f, s] = (read f :: Int, read s :: Int)
toPair _ = (-1, -1)

countGraphs :: [Edge] -> Int
countGraphs = countGraphs' 0

countGraphs' :: Int -> [Edge] -> Int
countGraphs' currentCount [] = currentCount
countGraphs' currentCount edges = traverseEdges currentCount edges

traverseEdges :: Int -> [Edge] -> Int
traverseEdges c e@((n1, _):_) = traverseEdges' c e [n1]

traverseEdges' :: Int -> [Edge] -> [Node] -> Int
traverseEdges' c edges [] = countGraphs' (c + 1) edges
traverseEdges' c edges (n:nodes) = traverseEdges' c newEdges newNodes'
  where (newEdges, newNodes) = removeEdgesFromNode edges n
        newNodes' = filter (/= n) $ nub (newNodes ++ nodes)

removeEdgesFromNode :: [Edge] -> Node -> ([Edge], [Node])
removeEdgesFromNode edges node = (newEdges, newNodes)
  where (newEdges, removedEdges) = partition (not . containsNode node) edges
        newNodes = uniqueNodesFromEdges removedEdges

containsNode :: Node -> Edge -> Bool
containsNode n (n1, n2) = n1 == n || n2 == n

uniqueNodesFromEdges :: [Edge] -> [Node]
uniqueNodesFromEdges = concatMap fromPair

fromPair (a, b) = [a, b]

main = do
  graph <- readFile realData
  let ls = lines graph
  let nodesCount = read $ head ls :: Int
  let edges = map (toPair . words) $ tail ls
  -- add self-referencing edges; these will be isolated single-node graphs, if any
  let completeSet = edges ++ map (\i -> (i, i)) [1..nodesCount]
  let graphsCount = countGraphs completeSet
  print (graphsCount - 1)
