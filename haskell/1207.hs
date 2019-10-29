list_produce::[Int]->Int

list_produce [] = 1
list_produce (x:xs) | even x = x*list_produce xs
                    | otherwise = list_produce xs