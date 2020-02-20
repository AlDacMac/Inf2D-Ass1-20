-- Inf2d Assignment 1 2019-2020
-- Matriculation number: S1837803
-- {-# OPTIONS -Wall #-}


module Inf2d1 where

import Data.List (sortBy, elemIndices, elemIndex)
import ConnectFourWithTwist

import Data.List (delete)

{- NOTES:

-- DO NOT CHANGE THE NAMES OR TYPE DEFINITIONS OF THE FUNCTIONS!
You can write new auxillary functions, but don't change the names or type definitions
of the functions which you are asked to implement.

-- Comment your code.

-- You should submit this file when you have finished the assignment.

-- The deadline is the  10th March 2020 at 3pm.

-- See the assignment sheet and document files for more information on the predefined game functions.

-- See the README for description of a user interface to test your code.

-- See www.haskell.org for haskell revision.

-- Useful haskell topics, which you should revise:
-- Recursion
-- The Maybe monad
-- Higher-order functions
-- List processing functions: map, fold, filter, sortBy ...

-- See Russell and Norvig Chapters 3 for search algorithms,
-- and Chapter 5 for game search algorithms.

-}

-- Section 1: Uniform Search



-- The Node type defines the position of the agent on the graph.
-- The Branch type synonym defines the branch of search through the graph.
type Node = Int
type Branch = [Node]
type Graph= [Node]

numNodes::Int
numNodes = 4

testGraph::Graph
testGraph=[0,1,2,0,0,0,0,1,0,0,0,0,0,0,0,0]

testHeuristicTable::[Int]
testHeuristicTable=[30,9,4,0]

-- 


-- The next function should return all the possible continuations of input search branch through the graph.
-- Your function should return an empty list if the input search branch is empty.
-- This implementation of next function does not backtrace branches.
next::Branch -> Graph ->  [Branch]
next branch g = [y:branch | y <- [0..numNodes-1], g!!((head(branch) * numNodes) + y) > 0]

-- |The checkArrival function should return true if the current location of the robot is the destination, and false otherwise.
checkArrival::Node -> Node -> Bool
checkArrival destination curNode = destination == curNode

explored::Node-> [Node] ->Bool
explored point exploredList = undefined

-- Section 3 Uniformed Search
-- | Breadth-First Search
-- The breadthFirstSearch function should use the next function to expand a node,
-- and the checkArrival function to check whether a node is a destination position.
-- The function should search nodes using a breadth first search order.

breadthFirstSearch::Graph -> Node->(Branch ->Graph -> [Branch])->[Branch]->[Node]->Maybe Branch
breadthFirstSearch [] destination next branches exploredList = Nothing
breadthFirstSearch g destination next [] exploredList = Nothing
breadthFirstSearch g destination next branches exploredList = 
    if (head (head branches)) `elem` exploredList
        then (breadthFirstSearch g destination next (tail branches) exploredList)
        else if ((head (head branches)) == destination)
            then Just (head branches)
            else breadthFirstSearch g destination (next) (tail branches ++ (next (head branches) g)) ((head (head branches):exploredList))
   

-- | Depth-Limited Search
-- The depthLimitedSearch function is similiar to the depthFirstSearch function,
-- except its search is limited to a pre-determined depth, d, in the search tree.
depthLimitedSearch::Graph ->Node->(Branch ->Graph-> [Branch])->[Branch]-> Int-> [Node] -> Maybe Branch
depthLimitedSearch g destination next branches d exploredList = 
    if (head (head branches)) `elem` exploredList || length (head branches) > d 
        then if (tail branches) == []
            then Nothing
            else (depthLimitedSearch g destination next (tail branches) d exploredList)
        else if ((head (head branches)) == destination)
            then Just (head branches)
            else depthLimitedSearch g destination (next) ((next (head branches) g) ++ tail branches) d ((head (head branches):exploredList))
  

{-depthLimitedSearch2::Graph ->Node->(Branch ->Graph-> [Branch])->[Branch]-> Int->[Node]-> Maybe Branch
depthLimitedSearch2 g destination next [] d exploredList = Nothing
depthLimitedSearch2 g destination next branches d exploredList =
    if (head (head branches)) `elem` exploredList || length (head branches) > d
        then depthLimitedSearch g destination next (tail branches) d exploredList
        else if 
-}

-- | Section 4: Informed search


-- | AStar Helper Functions

-- | The cost function calculates the current cost of a trace. The cost for a single transition is given in the adjacency matrix.
-- The cost of a whole trace is the sum of all relevant transition costs.
cost :: Graph -> Branch -> Int
cost gr [node] = 0
cost gr (node:branch) = gr!!((node * numNodes) + head(branch)) + cost gr branch


    
-- | The getHr function reads the heuristic for a node from a given heuristic table.
-- The heuristic table gives the heuristic (in this case straight line distance) and has one entry per node. It is ordered by node (e.g. the heuristic for node 0 can be found at index 0 ..)  
getHr:: [Int]->Node->Int
getHr hrTable node = hrTable!!node


-- | A* Search
-- The aStarSearch function uses the checkArrival function to check whether a node is a destination position,
---- and a combination of the cost and heuristic functions to determine the order in which nodes are searched.
---- Nodes with a lower heuristic value should be searched before nodes with a higher heuristic value.

estimateMinPath:: Graph -> (Graph -> Branch -> Int) -> ([Int]->Node->Int) -> [Int] -> Branch -> Branch -> Branch
estimateMinPath g cost heuristic hrTable branch1 branch2 = 
    if ((heuristic hrTable (head branch1)) + (cost g branch1)) <= ((heuristic hrTable (head branch2)) + (cost g branch2))
        then branch1
        else branch2

aStarSearch::Graph->Node->(Branch->Graph -> [Branch])->([Int]->Node->Int)->[Int]->(Graph->Branch->Int)->[Branch]-> [Node]-> Maybe Branch
aStarSearch g destination next getHr hrTable cost [] exploredList = Nothing
aStarSearch [] destination next getHr hrTable cost branches exploredList = Nothing
aStarSearch g destination next getHr hrTable cost branches exploredList = 
    if head(foldl1 (estimateMinPath g cost getHr hrTable) branches) `elem` exploredList
        then aStarSearch g destination next getHr hrTable cost (delete (foldl1 (estimateMinPath g cost getHr hrTable) branches) branches) exploredList
        else if head(foldl1 (estimateMinPath g cost getHr hrTable) branches) == destination
            then Just (foldl1 (estimateMinPath g cost getHr hrTable) branches)
            else aStarSearch g destination next getHr hrTable cost (branches ++ next (foldl1 (estimateMinPath g cost getHr hrTable) branches) g) (head(foldl1 (estimateMinPath g cost getHr hrTable) branches): exploredList)

-- | Section 5: Games
-- See ConnectFourWithTwist.hs for more detail on  functions that might be helpful for your implementation. 



-- | Section 5.1 Connect Four with a Twist

 

-- The function determines the score of a terminal state, assigning it a value of +1, -1 or 0:
-- The human player, with role value 1, is max
eval :: Game -> Int
eval game = 
    if checkWin game 1
        then 1
        else if checkWin game 0
            then (-1)
            else 0
                    
                
                    

-- | The alphabeta function should return the minimax value using alphabeta pruning.
--  The eval function should be used to get the value of a terminal state. 
alphabeta:: Role -> Game -> Int
alphabeta player game = 
    if player == maxPlayer
        then maxValue (movesAndTurns game maxPlayer) (-2) 2 (-2)
        else minValue (movesAndTurns game minPlayer) (-2) 2 2 

-- v is given as an input here, as we can't do loops
maxValue:: [Game] -> Int -> Int -> Int -> Int
maxValue [] alpha beta v = v
maxValue (game:games) alpha beta v = 
    if terminal game
        then let newV = (max v (eval game)) in
            if newV >= beta 
                then newV
                else maxValue games (max alpha newV) beta newV
        else let newV = (max v (minValue (movesAndTurns game minPlayer) alpha beta 2)) in
            if newV >= beta 
                then newV
                else maxValue games (max alpha newV) beta newV


minValue:: [Game] -> Int -> Int -> Int -> Int
minValue [] alpha beta v = v
minValue (game:games) alpha beta v = 
    if terminal game
        then let newV = (min v (eval game)) in
            if newV <= alpha
                then newV
                else minValue games alpha (min beta newV) newV
        else let newV = (min v (maxValue (movesAndTurns game maxPlayer) alpha beta (-2))) in
            if newV <= alpha
                then newV
                else minValue games alpha (min beta newV) newV

-- | OPTIONAL!
-- You can try implementing this as a test for yourself or if you find alphabeta pruning too hard.
-- If you implement minimax instead of alphabeta, the maximum points you can get is 10% instead of 15%.
-- Note, we will only grade this function IF YOUR ALPHABETA FUNCTION IS EMPTY.
-- The minimax function should return the minimax value of the state (without alphabeta pruning).
-- The eval function should be used to get the value of a terminal state.
minimax:: Role -> Game -> Int
minimax player game=undefined
{- Auxiliary Functions
-- Include any auxiliary functions you need for your algorithms below.
-- For each function, state its purpose and comment adequately.
-- Functions which increase the complexity of the algorithm will not get additional scores
-}
