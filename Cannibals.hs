	module AIMAMissionaries where
	import Data.List(sort, nub, (\\))
	 
	 
	data Person = Missionary | Cannibal
	 deriving (Ord, Eq, Show)
	 
	data Position = LeftSide | RightSide
	 deriving (Eq, Show)
	 
	data PState = PState {left :: [Person], right :: [Person], boat :: Position}
	 deriving (Eq, Show)
	 
	beginState = PState {left = [Missionary, Missionary, Missionary, Cannibal, Cannibal, Cannibal], right = [], boat = LeftSide}
	goalState =  PState {left = [], right = [Missionary, Missionary, Missionary, Cannibal, Cannibal, Cannibal], boat = RightSide}
	 
	almostGoalState = PState {left = [Cannibal], right = [Missionary, Missionary, Missionary, Cannibal, Cannibal], boat = LeftSide}
	almostGoalState2 = PState {left = [Cannibal, Missionary, Missionary], right = [Missionary, Cannibal, Cannibal], boat = LeftSide}
	 
	 
	 
	solution :: [PState]
	solution = idfs beginState 0
	 
	idfs :: PState -> Int -> [PState]
	idfs s n = case idfs' 0 n False s of
	           [] -> idfs s (n+1)
	           other -> other
	where
	 idfs' :: Int -> Int -> Bool -> PState -> [PState]
	  idfs' m n True s = [s]
	 idfs' m n False s
	   | isGoalState s  = idfs' m n True s
	  | m==n           = []
	  | otherwise      = case dropWhile (==[]) $ map (idfs' (m+1) n False) (successors s) of
	                       []     -> []
	                       (x:xs) -> s : x
	 
	-- check if the state is a goal state
	isGoalState :: PState -> Bool
	isGoalState = (== goalState)
	 
	-- filter legal states
	successors :: PState -> [PState]
	successors = filter isLegalState . allSucc
	 
	-- generate all states after applying all possible combinations
	allSucc :: PState -> [PState]
	allSucc s
	 | boat s == LeftSide = map (updatePStateLeft s) (genCombs (left s))
	 | otherwise          = map (updatePStateRight s) (genCombs (right s))
	 
	-- move a number of cannibals and missonaries to the right side
	updatePStateLeft s p = let oldLeft = left s
	                           oldRight = right s
	                       in s {left = sort $ oldLeft \\ p,
	                             right = sort $ oldRight ++ p,
	                             boat = RightSide
	                            }
	 
	-- move a number of cannibals and missonaries to the left side
	updatePStateRight s p = let oldLeft = left s
	                            oldRight = right s
	                        in s {left = sort $ oldLeft ++ p,
	                              right = sort $ oldRight \\ p,
	                              boat = LeftSide
	                             }
	 
	-- unique combinations
	genCombs :: Ord a => [a] -> [[a]]
	genCombs = nub . map sort . genPerms
	 
	-- permutations of length 1 and 2
	genPerms :: Eq a => [a] -> [[a]]
	genPerms [] = []
	genPerms (x:xs) = [x] : (map (: [x]) xs) ++ genPerms xs
	-- legal states are states with the number of cannibals equal or less
	-- to the number of missionaries on one riverside (or sides with no missionaries)
	isLegalState :: PState -> Bool
	isLegalState s = hasNoMoreCannibals (left s) && hasNoMoreCannibals (right s)
	 where hasNoMoreCannibals lst = let lenMiss = length ( filter (== Missionary) lst)
	                                    lenCann = length ( filter (== Cannibal) lst)
	                                in lenMiss == 0 || lenMiss >= lenCann