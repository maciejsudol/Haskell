-- pierwsze 17 nieparzystych
	-- take 17 ([1,3..19]++[23..])
-- trojkaty
	-- [(x,y,z)| x<-[3..17], y<-[3..17], z<-[3..17], x<(y+z) || y<(x+z) || z<(x+y)]
-- trojkaty prostokatne
	-- [(x,y,z)| x<-[3..17], y<-[3..17], z<-[3..17], x^2 == (y^2 + z^2) || y^2 == (x^2 + z^2) || z^2 = (x^2 y^2)]

head' (x:_) = x

length' list = sum([1| _<-list])

take' x list =
	if length list == x
		then list
	else
		take' x (init list)

map' fun arg = [fun x| x<-arg]

plusy x y =
	if length x == 0
		then y
	else
		plusy (init x) ((last x) : y)
