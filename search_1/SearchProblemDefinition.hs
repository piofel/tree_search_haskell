module SearchProblemDefinition where

-- Below, problem definition provided by the User

data State = State Char
     deriving (Eq, Show)

data Action = Action Char
     deriving (Show)

method = "DepthFirst" -- options: {BreadthFirst,DepthFirst}

initState :: State
initState = State 'a'

finalStates :: [State]
finalStates = [(State 'e'), (State 'm')]

successorFunction :: State -> [(Action,State)]
successorFunction (State 'a') = [((Action 'l'),(State 'b')), ((Action 'r'),(State 'c'))]
successorFunction (State 'b') = [((Action 'l'),(State 'd')), ((Action 'r'),(State 'e'))]
successorFunction (State 'c') = [((Action 'l'),(State 'f')), ((Action 'r'),(State 'g'))]
successorFunction (State 'd') = [((Action 'l'),(State 'h')), ((Action 'r'),(State 'i'))]
successorFunction (State 'e') = [((Action 'l'),(State 'j')), ((Action 'r'),(State 'k'))]
successorFunction (State 'f') = [((Action 'l'),(State 'l')), ((Action 'r'),(State 'm'))]
successorFunction (State 'g') = [((Action 'l'),(State 'n')), ((Action 'r'),(State 'o'))]
successorFunction (State c)   = []

goalTest :: State -> Bool
goalTest s = if s `elem` finalStates
             then True
             else False

stepCost :: State -> Action -> State -> Double
stepCost state action result = 1.0

noAction :: Action
noAction = Action 'x'
