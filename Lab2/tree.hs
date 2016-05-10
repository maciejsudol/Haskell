import Data.Char

data Tree generic_type = Empty | Node generic_type (Tree generic_type) (Tree generic_type) deriving (Eq, Ord, Read, Show)
data ETree generic_type = EEmpty | ENode generic_type generic_type (ETree generic_type) (ETree generic_type) deriving (Eq, Ord, Read, Show)
notBalancedTree = Node 1 Empty (Node 2 Empty (Node 3 Empty (Node 4 Empty Empty)))

toString Empty =
	putStr "Empty"
toString (Node value left right) =
	do
	putStr (show value ++ " Left:(")
	toString left
	putStr ("), Right:(")
	toString right
	putStr(")")

maxValue (Node value left right) = max value (max value2 value3)
	where
	value2 = 
		if (left == Empty)
			then value
		else maxValue left
	value3 =
		if (right == Empty)	
			then value
		else maxValue right

minValue (Node value left right) = min value (min value2 value3)
	where
	value2 = 
		if (left == Empty)
			then value
		else minValue left
	value3 =
		if (right == Empty)	
			then value
		else minValue right

height Empty = 0
height (Node value left right) =
	1 + (max (height left) (height right))

isLeave Empty = False
isLeave (Node value left right) =
	if (left == Empty && right == Empty)
		then True
	else False

swap :: (Ord t) => Tree t -> t
--swap Empty = Empty
swap (Node value Empty right) =
	value
swap (Node value left right) =
	swap left

removeVertex :: (Ord t) => Tree t -> Tree t
removeVertex Empty = Empty
removeVertex (Node value Empty right) =
	right
removeVertex (Node value left Empty) =
	left
removeVertex (Node value left right) =
	(Node newValue left right)
	where
		newValue = swap right

------------------------------------------------------------

insert element Empty =
	Node element Empty Empty
insert element (Node value left right)
	|value < element
		= (Node value left (insert element right))
	|value >= element
		= (Node value (insert element left) right)

empty :: (Ord t) => Tree t -> Bool
empty Empty = True
empty _ = False

isBinary Empty = True
isBinary (Node value left right) =
	isBinary left &&
	isBinary right &&
	(left == Empty || value > (maxValue left)) &&
	(right == Empty || value <= (minValue right))

search element Empty = False
search element (Node value left right)
	|value == element = True
	|value > element = search element left
	|value < element = search element right

isBalanced Empty = True
isBalanced (Node value left right)
	|isBalanced left && isBalanced right && (abs (height1 - height2)) <= 1
		= True
	|otherwise
		= False
	where
		height1 = height left
		height2 = height right

vlr Empty = []
vlr (Node value left right) =
	[value] ++ vlr left ++ vlr right

lvr Empty = []
lvr (Node value left right) =
	lvr left ++ [value] ++ lvr right

lrv Empty = []
lrv (Node value left right) =
	lrv left ++ lrv right ++ [value]

vrl Empty = []
vrl (Node value left right) =
	[value] ++ vrl right ++ vrl left

rvl Empty = []
rvl (Node value left right) =
	rvl right ++ [value] ++ rvl left

rlv Empty = []
rlv (Node value left right) =
	rlv right ++ rlv left ++ [value]

leaves Empty = []
leaves (Node value left right) =
	leaves left ++ leaves right ++
	(if (isLeave (Node value left right) == True)
		then [value]
	else [])

nnodes Empty = 0
nnodes (Node value left right) =
	1 + nnodes left + nnodes right

nsum Empty = 0
nsum (Node value left right) =
	value + nsum left + nsum right

tmap function Empty = Empty
tmap function (Node value left right) =
	Node (function value) (tmap function left) (tmap function right)

remove :: (Ord t) => t -> Tree t -> Tree t
remove _ Empty = Empty
remove element (Node value left right)
	|value == element
		= removeVertex (Node value left right)
	|value > element
		= Node value (remove element left) right
	|value < element
		= Node value left (remove element right)

------------------------------------------------------------

getLevel _ Empty = []
getLevel level (Node value left right)
	|level == 1
		= [value]
	|level > 1
		= getLevel (level - 1) left ++ getLevel (value - 1) right
	|otherwise
		= []

getOrder Empty = []
getOrder (Node value left right) =
	zip (lvr (Node value left right)) [1..(nnodes (Node value left right))]

absoluteHeight Empty _ = 0
absoluteHeight (Node value left right) (Node rootValue rootLeft rootRight) =
	height (Node rootValue rootLeft rootRight) - height (Node value left right) + 1

makeLayoutHelper Empty _ _ = []
makeLayoutHelper (Node value left right) root count =
	--[(x,y)| y <- [1..(nnodes (Node value left right))], x <- getLevel y (Node value left right)]
	[(x,y)] ++ (makeLayoutHelper left root (count+1)) ++ (makeLayoutHelper right root (count+1))
	where
		x = count
		y = absoluteHeight (Node value left right) root

makeLayout root =
	makeLayoutHelper root root 1

enumerateLevelHelper Empty _ =
	EEmpty
enumerateLevelHelper (Node value left right) root =	
	(ENode value level (enumerateLevelHelper left root) (enumerateLevelHelper right root))
	where
		level = absoluteHeight (Node value left right) root

enumerateLevel tree =
	enumerateLevelHelper tree tree


dumpNode Empty =
	""
dumpNode (Node value _ _) =
	show value
dumpNodes (Node v Empty Empty) =
	""
dumpNodes (Node value left Empty) =
	(show value) ++ "->" ++ (dumpNode left) ++ "\n" ++ (dumpNodes left)
dumpNodes (Node value Empty right) =
	(show value) ++ "->" ++ (dumpNode right) ++ "\n" ++ (dumpNodes right)
dumpNodes (Node value left right) =
	(show value) ++ "->" ++ (dumpNode left) ++ "\n" ++ (show value) ++ "->" ++ (dumpNode right) ++ "\n" ++ (dumpNodes left) ++ (dumpNodes right)

dumpDOT (Node value left right) =
	"digraph G {\n"++ dumpNodes (Node value left right) ++"}\n"
