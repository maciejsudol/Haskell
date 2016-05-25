import Debug.Trace                                                    -- importuje wszystkie funkcje z modułu Debug.Trace

_head (x:xs) = x
_length [] = 0
_length (x:xs) = 1 + _length xs

_take n _
    | n <= 0 = []
_take _ [] = []
_take n (x:xs) = x : _take (n-1) xs

_map _ [] = []
_map o (x:xs) = o x : _map o xs

dodajTrzyLiczby :: Double -> Double -> Double -> Double
dodajTrzyLiczby a b c = a+b+c

quicksort [] = []                                                     -- definiuje zachowanie na pustej liście
quicksort xx@(x:xs)                                                   -- @ inforumuje żeby zmienna xx była widoczna jako całość i jako poszczególne części x i xs
		| trace ("qs: "++ show x ++ " " ++show xx) False = [] -- zastosowano tu składnie | case = execute | case2 = execute2, ponieważ trace zwróci False ta gałąź się nigdy nie wykona, ale zostanie wypisana wiadomość na stderr
                | otherwise = quicksort low ++ x : quicksort high     -- otherwise to nic innego jak True, można nawet sprawdzić True == otherwise w GHCi ta gałąź się wykona w pozostałych przypadkach (w tym wypadku zawsze), następuje konkatenacja listy posortowanej quicksortem z listą składającą się z elementu rodzielającego i listy posortowanej quicksortem
                              where low = [e | e <- xs, e < x]        -- w bloku where następują definicje zmiennych lokalnych low to podlista xs z wartościami mniejszymi od strażnika x
                                    high = [e | e <- xs, e >= x]      -- natomiast high to lista w z wartościami większymi lub równymi strażnikowi


sign x | x > 0 = 1
       | x == 0 = 0
       | x < 0 = -1

tr :: Integer -> Integer -> Integer -> Bool

tr x y z | (x > 3 && x < 17) &&
           (y > 3 && y < 17) &&
           (z > 3 && z < 17) &&
           (x < y+z)         &&
           (y < x+z)         &&
           (z < x+y) = True
         | otherwise = False

tr_print x y z | ((tr x y z) == True) = do
                                        putStr("(")
                                        putStr . show $ (x)
                                        putStr(",")
                                        putStr . show $ (y)
                                        putStr(",")
                                        putStr . show $ (z)
                                        putStrLn(") jest trojkatem")
               | ((tr x y z) == False) = do
                                        putStr("(")
                                        putStr . show $ (x)
                                        putStr(",")
                                        putStr . show $ (y)
                                        putStr(",")
                                        putStr . show $ (z)
                                        putStrLn(") nie jest trojkatem")

data STree a = Nil | Node (STree a) a (STree a) deriving (Eq, Ord, Show)

makeTree :: Ord a => [a] -> STree a
makeTree = foldl insert Nil

insert :: Ord a => STree a -> a -> STree a
insert Nil v = Node Nil v Nil
insert (Node t1 x t2) v

    | x == v = Node t1 x t2
    | v < x = Node (insert t1 v) x t2
    | otherwise = Node t1 x (insert t2 v)

empty :: Ord a => STree a -> Bool
empty n
    | n == Nil = True
    | otherwise = False



isBinary :: Ord a => STree a -> Bool
isBinary Nil = True
isBinary(Node t1 x t2) = isBinary t1 && isBinary t2 &&
                       (t1 == Nil || x > maxt t1) &&
                       (t2 == Nil || x < mint t2)



search :: Ord a => STree a -> a -> Bool
search Nil v = False
search (Node t1 x t2) v
    | x == v = True
    | v < x = search t1 v
    | v > x = search t2 v

isBalanced :: Ord a => STree a -> Bool
isBalanced Nil = True
isBalanced (Node t1 x t2)
        |   ((isBalanced t1 == True) &&
            (isBalanced t2 == True) &&
            (abs(h1 - h2) <= 1)) = True
        |   otherwise = False
        where
            h1 = height t1
            h2 = height t2

vlr :: Ord a => STree a -> [a]
vlr Nil = []
vlr (Node t1 x t2) = [x] ++ vlr t1 ++ vlr t2

lvr :: Ord a => STree a -> [a]
lvr Nil = []
lvr (Node t1 x t2) = lvr t1 ++ [x] ++ lvr t2

lrv :: Ord a => STree a -> [a]
lrv Nil = []
lrv (Node t1 x t2) = lrv t1 ++ lrv t2 ++ [x]

vrl :: Ord a => STree a -> [a]
vrl Nil = []
vrl (Node t1 x t2) = [x] ++ vrl t2 ++ vrl t1

rvl :: Ord a => STree a -> [a]
rvl Nil = []
rvl (Node t1 x t2) = rvl t1 ++ [x] ++ rvl t2

rlv :: Ord a => STree a -> [a]
rlv Nil = []
rlv (Node t1 x t2) = rvl t2 ++ rvl t1 ++ [x]

leaves :: Ord a => STree a -> [a]
leaves Nil = []
leaves (Node t1 x t2) = leaves t1 ++ leaves t2 ++ (if (isleave (Node t1 x t2) == True) then [x] else [])

nnodes :: Ord a => STree a -> Integer
nnodes Nil = 0
nnodes (Node t1 x t2) = 1 + nnodes t1 + nnodes t2

-- a must be the type that implements the Integral typeclass
nsum :: Num a => STree a -> a
nsum Nil = 0
nsum (Node t1 x t2) = x + nsum t1 + nsum t2

tmap :: Ord a => (a -> a) -> STree a -> STree a
tmap _ Nil = Nil
tmap f a = makeTree (map f (vlr a))

tmap2 :: Ord a => (a -> a) -> STree a -> STree a
tmap2 _ Nil = Nil
tmap2 f (Node t1 x t2) = Node (tmap2 f t1) (f x) (tmap2 f t2)

isleave :: Ord a => STree a -> Bool
isleave Nil = False
isleave (Node t1 x t2) = if (t1 == Nil && t2 == Nil) then True else False


height :: Ord a => STree a -> Integer
height Nil = 0
height (Node t1 x t2) = 1 + (max (height t1) (height t2))

maxt :: Ord a => STree a -> a
maxt (Node t1 x t2) = max x (max y z)
    where
        y = if (t1 == Nil) then x
            else maxt t1
        z = if (t2 == Nil) then x
            else maxt t2

mint :: Ord a => STree a -> a
mint (Node t1 x t2) = min x (min y z)
    where
        y = if (t1 == Nil) then x
            else mint t1
        z = if (t2 == Nil) then x
            else mint t2


tree = Node Nil 1 (Node Nil 2 Nil)
emptyTree = Nil
notBalancedTree = Node Nil 1 (Node Nil 2 (Node Nil 3 (Node Nil 4 Nil)))
sTree = makeTree [10,5,7,6,4,3,13,12,11,17]
leaveExample = Node Nil 1 Nil


