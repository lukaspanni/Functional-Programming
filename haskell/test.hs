multTable = [[x*y | y <- [1..10]] | x <- [1..10]]

fac n
    | n == 0 = 1
    | otherwise = fac(n-1)*n

sumN n = sum [1..n]
sumEN n = sum [2,4..n]
sumON n = sum [1,3..n]

isOdd n
    |n `mod` 2 == 0 = False
    |otherwise = True

sum_prod (x,y) = (x+y, x*y)

sum_list :: [Int] -> Int
sum_list[] = 0
sum_list (x:xs) = x + sum(xs)

sqr_list::[Int] -> [Int]
sqr_list [] = []
sqr_list (x:xs) = (x*x):sqr_list xs

sum_EList :: [Int] -> Int
sum_EList [] = 0
sum_EList (x:xs)
                |odd x = sum_EList xs
                |otherwise = x + sum_EList xs


count_list :: [Int] -> Int
count_list [] = 0
count_list (x:xs) = 1 + count_list xs

avg::[Int] -> Int
avg [] = 0
avg (x:xs) = sum_list (x:xs) `div` count_list (x:xs)


set_in::Int -> [Int] -> Bool
set_in x [] = False
set_in x (y:ys) 
                |x == y = True
                |otherwise = set_in x ys
set_add::Int -> [Int] -> [Int]
set_add x [] = (x:[])
set_add x l
                |set_in x l = l
                |otherwise = (x:l)
set_union::[Int] -> [Int] -> [Int]
set_union [] [] = []
set_union l [] = l 
set_union [] l = l 
set_union (x:xs) l = set_union xs (set_add x l)

slice::Int -> Int -> [Int] -> [Int]
slice 1 m liste = take m liste
slice n m (x:xs) = slice (n-1) (m-1) xs

dropCond::(Int->Bool)->[Int]->[Int]
dropCond c [] = []
dropCond c (x:xs)   | c x = dropCond c xs
                    | otherwise = (x:(dropCond c xs))

lastEl::[Int]->Int
lastEl [] = 0
lastEl (x:[]) = x
lastEl (x:xs) = lastEl xs

nthEl::Int->[Int]->Int
nthEl _ [] = -1
nthEl 1 (x:xs) = x
nthEl n (x:xs) = nthEl (n-1) xs

listLength::[Int]->Int
listLength [] = 0
listLength (x:xs) = 1 + listLength xs

listRev::[Int]->[Int]
listRev [] = []
listRev (x:xs) = listRev xs ++ [x]