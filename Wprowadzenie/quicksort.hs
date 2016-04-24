import Debug.Trace
quicksort [] = []
quicksort xx@(x:xs)
	| trace ("qs : " ++ show x ++ " " ++ show xx) False = []
	| otherwise = quicksort low ++ x : quicksort high
		where
			low = [e| e<-xs, e<x]
			high = [e| e<-xs, e>=x]
