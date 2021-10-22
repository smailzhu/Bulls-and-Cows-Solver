
{-
 - check how many char are A (correct position and correct char)
 - checkA ans_perm test_perm
 -
 - checkA [1,2,3,4] [1,3,4,5] == 1
 - checkA [1,2,3,4] [1,2,3,4] == 4
 -}

checkA :: Eq a => [a] -> [a] -> Int
checkA [] [] = 0
checkA (x:xs) (y:ys)
    | x == y    = 1 + (checkA xs ys)
    | otherwise = checkA xs ys


{-
 - check A and B in two list
 - check ans_perm test_perm
 -
 - checkAB [9,5,2,7] [1,2,3,4] == (0, 1)
 - checkAB [9,5,2,7] [1,3,2,4] == (1, 0)
 -}

checkAB :: Eq a => [a] -> [a] -> (Int, Int)
checkAB xs ys = (a_num, b_num)
    where a_num = checkA xs ys
          b_num = subtract a_num $ length $ filter (True==) $ map (\y -> elem y xs) ys


{- check if list has duplicate element
 -
 - hasSame [1,2,3,4] == False
 - hasSame [1,2,3,1] == True
 -}

hasSame :: Eq a => [a] -> Bool
hasSame [] = False
hasSame (x:xs) = elem x xs || hasSame xs

