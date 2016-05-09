import Data.Char

helper = zip (['a'..'z']++['A'..'Z']++[' ']) [0..]
--helper = zip ['a'..'z'] [0..]

letter2number::Char -> Int	-- header do "parametryzowania, nie jest konieczny - compilator zaweza poszukiwania typu
letter2number c = head [b| (a,b) <- helper, a==c]
number2letter n = head [a| (a,b) <- helper, b==(n `mod` (length helper))]

--shiftPos n c =
	--if (isLower c) == False
		--then c		-- jezeli litera jest duza
	--else
		--number2letter ((letter2number c) + n)
shiftPos n c =
	number2letter ((letter2number c) + n)

encode::Int -> String -> String
encode n s = map (shiftPos n) s	-- map aplikuje podana funkcje do kazdego elementu listy
decode s n = map (shiftPos ((length helper) - n)) s 

https://gist.github.com/lashleigh/796430

