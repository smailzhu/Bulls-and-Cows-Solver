import Data.Char
import System.IO
import System.Random
import Control.Monad -- for replicateM
import Data.List -- for nub

main:: IO()
main = do
    let ansSet = ['0'..'9']
    i <- prompt "How many digit you want to play with? "
    putStrLn $ "Choosing " ++ show i ++ " digit from: " ++ ansSet
    solver $ allAnswer (read i :: Int) ansSet


-- https://stackoverflow.com/a/13190872
prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine


{-
 - find the solution from ansPool (all possible solutions)
 - if ansPool is empty: it may have some problem
 - if ansPool only has one solution that whould be the correct one
 - if ansPool has more than one possible solutions: random choose one and ask user
 -}
solver :: (Show a, Eq a) => [[a]] -> IO()
solver ansPool
    | 0 == length ansPool = putStrLn "I cannot find any possible solotion"
    | 1 == length ansPool = print $ head ansPool
    | otherwise = do
              newTest <- randomChoose ansPool
              putStrLn $ "I guess " ++ show newTest
              a <- prompt "A:? "
              b <- prompt "B:? "
              solver $ updateAnswers ansPool newTest (read a :: Int, read b :: Int)

{-
 - random choose one from all possibles
 - An alternative for chooseFirst
 -}
randomChoose :: Eq a => [[a]] -> IO [a]
randomChoose pool = do
            r <- randomRIO (0, (length pool)-1)
            return $ pool !! r


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
 - hasDuplicates [1,2,3,4] == False
 - hasDuplicates [1,2,3,1] == True
 -}
hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates xs = length (nub xs) /= length xs


{- Answer Pool
 -
 - Generate all possible answer
 - remove wihch has duplicate element
 -}
allAnswer :: Eq a => Int -> [a] -> [[a]]
allAnswer n x = filter (\x -> not $ hasDuplicates x) $ replicateM n x


{- Update answer pool
 - choose the solutions which match the rules base on the test
 -}
updateAnswers :: Eq a => [[a]] -> [a] -> (Int, Int) -> [[a]]
updateAnswers ans test rules = filter ((rules==) .(\x -> checkAB test x)) ans

