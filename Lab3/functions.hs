lista = [1,2,3,4,5,6,7,8,9,10]

sum' list =
	foldl (+) 0 list
sum'' list =
	foldr (+) 0 list

product' list =
	foldl (*) 1 list
product'' list =
	foldr (*) 1 list

reverse' list =
	foldl (flip (:)) [] list
reverse'' list =
	foldr (\element list -> list ++ [element]) [] list	-- Funkcja anonimowa

and' list =
	foldl (&&) True list
and'' list =
	foldr (&&) True list

or' list =
	foldl (||) False list
or'' list =
	foldr (||) False list

head' (head:tail) =
	foldl (\first rest -> first) head (head:tail)
head'' list =
	foldr (\first _ -> first) 0 list

last' list =
	foldl (\_ rest -> rest) 0 list
