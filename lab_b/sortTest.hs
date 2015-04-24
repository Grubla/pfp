import Criterion.Main
import System.Random (mkStdGen, randoms)
import Data.List
import Data.Array.Repa as R
import Data.Functor.Identity
import Control.Monad.Par 

quickSortS1 :: (Ord a) => [a] -> [a]
quickSortS1 []     = []
quickSortS1 (x:[]) = [x]
quickSortS1 (x:xs) = less Prelude.++ equal Prelude.++ greater
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

quickSortP1 :: [Int] -> [Int]
quickSortP1 []      = []
quickSortP1 (x:[])  = [x]
quickSortP1 xs      = toList $ quickSortP1' repaList
  where repaList = (fromListUnboxed (Z :. ((length xs)::Int)) xs :: Array U DIM1 Int)

quickSortP1' :: Array U DIM1 Int -> Array D DIM1 Int
quickSortP1' ys 
  | n <= 1      = delay ys
  | otherwise   = runIdentity $ do 
  let pivot     = ys!(Z:.0)
  less          <- selectP (\a -> (ys!(Z:.a)) < pivot) (\a -> (ys!(Z:.a))) n
  equal         <- selectP (\a -> (ys!(Z:.a)) == pivot) (\a -> (ys!(Z:.a))) n
  greater       <- selectP (\a -> (ys!(Z:.a)) > pivot) (\a -> (ys!(Z:.a))) n
  return $ (quickSortP1' less) R.++ (delay equal) R.++ (quickSortP1' greater)
    where (Z:.n)    = extent ys


quickSortP2 :: (Ord a) => [a] -> [a]
quickSortP2 ys = runPar $ go ys 
  where
    go ys
      | (length ys) <= 1  = ys
      | otherwise         = do
        let pivot = ys!!0
        l <- spawn $ go $ filter (\y -> y < pivot) ys
        e <- spawn $ go $ filter (\y -> y == pivot) ys
        g <- spawn $ go $ filter (\y -> y > pivot) ys
        less    <- get l
        equal   <- get e
        greater <- get g
        return (less Data.List.++ greater Data.List.++ equal)

main = defaultMain [bench "Sort" (nf sort randomInts),
  bench "MergeS1" (nf mergeSortS1 randomInts),
  bench "QuickS1" (nf quickSortS1 randomInts),
  bench "QuickP1" (nf quickSortP1 randomInts)]

randomInts = take 50 (randoms (mkStdGen 17465864)) :: [Int]
