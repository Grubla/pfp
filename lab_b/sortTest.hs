import Criterion.Main
import System.Random (mkStdGen, randoms)

quickSortS1 :: (Ord a) => [a] -> [a]
quickSortS1 []     = []
quickSortS1 (x:[]) = [x]
quickSortS1 (x:xs) = less ++ equal ++ greater
  where less     = quickSortS1 [y | y <- xs, y < x]
        equal    = [y | y <- (x:xs) , y == x]
        greater  = quickSortS1 [y | y <- xs , y > x]


mergeSortS1 :: (Ord a) => [a] -> [a]
mergeSortS1 []     = []
mergeSortS1 (x:[]) = [x]
mergeSortS1 ls     = mergeS1 (mergeSortS1 l1) (mergeSortS1 l2)
  where (l1,l2) = splitAt (div (length ls) 2) ls

mergeS1 :: (Ord a) => [a] -> [a] -> [a]
mergeS1 [] ys = ys
mergeS1 xs [] = xs
mergeS1 (x:xs) (y:ys)
  | x < y       = x:(mergeS1 xs (y:ys))
  | otherwise   = y:(mergeS1 (x:xs) ys)

main = defaultMain [bench "MergeS1" (nf mergeSortS1 randomInts),
  bench "QuickS1" (nf quickSortS1 randomInts)]

randomInts = take 100000 (randoms (mkStdGen 17465864)) :: [Integer]
