module TreeSearch (searchActions,searchStates) where

import SearchProblemDefinition 

data Node = Node {
            state :: State,
            parentNode :: Node,
            action :: Action,
            pathCost :: Double,
            depth :: Int
            }
            | EmptyNode
            deriving (Show)

searchActions :: [Action]
searchActions = tail (map action searchNodes)

searchStates :: [State]
searchStates = map state searchNodes

searchNodes :: [Node]
searchNodes = search [initNode] where
        initNode = Node initState EmptyNode noAction 0.0 0

search :: [Node] -> [Node]
search [] = []
search fringe = let (n,f) = chooseNode fringe in
        if goalTest (state n)
        then solution n
        else search (f ++ (expand n))

chooseNode :: [Node] -> (Node,[Node])
chooseNode fringe = case method of
        "BreadthFirst" -> ((head fringe),(tail fringe))
        "DepthFirst" -> ((last fringe),(init fringe))
        _ -> ((last fringe),(init fringe))

expand :: Node -> [Node]
expand node = map createSuccessorNode successorActStates
              where
              successorActStates = successorFunction (state node)
              createSuccessorNode successorActState =
                        Node {
                                state = snd successorActState,
                                parentNode = node,
                                action = fst successorActState,
                                pathCost = (pathCost node) + stepCost (state node) (fst successorActState) (snd successorActState),
                                depth = (depth node) + 1
                        }

solution :: Node -> [Node]
solution node = (ancestors node) ++ [node]
        where
        ancestors (Node _ EmptyNode _ _ _) = []
        ancestors n = let par = parentNode n in (ancestors par) ++ [par]

