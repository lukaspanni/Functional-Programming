listSum::[Int]->Int
listSum [] = 0
listSum (x:xs) = x + listSum xs

listSquare::[Int]->[Int]
listSquare [] = []
listSquare (x:xs) = (x*x):listSquare xs

listLast::[Int]->Int
listLast (x:xs) | xs == [] = x
                | otherwise = listLast xs

listInit::[Int]->[Int]
listInit [] = []
listInit (x:xs) | xs == [] = []
                | otherwise = x:listInit xs

listReverse::[Int]->[Int]
listReverse [] = []
listReverse (x:xs) = listReverse xs ++[x]
