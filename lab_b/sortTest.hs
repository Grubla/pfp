import Criterion.Main
import System.Random (mkStdGen, randoms)
import Data.List
import Data.Functor.Identity
import Control.Parallel.Strategies (rpar, rseq, runEval)
import Control.Parallel (par, pseq)
import Control.Monad.Par 
import Control.DeepSeq (NFData, force)
import Control.Parallel.Strategies

quickSortS1 :: (Ord a) => [a] -> [a]
quickSortS1 []     = []
quickSortS1 (x:[]) = [x]
quickSortS1 (x:xs) = less ++ [x] ++ greater
  where less     = quickSortS1 [y | y <- xs, y <= x]
        greater  = quickSortS1 [y | y <- xs , y > x]

quickSortP2 :: Ord a => [a] -> [a]
quickSortP2 [] = []
quickSortP2 (x:xs) 
  | length (x:xs) < 10 = quickSortS1 (x:xs)
  | otherwise          = runEval $ do 
    less <- rpar $ force quickSortP2 [y | y <- xs, y <= x]
    greater <- rpar $ force quickSortP2 [y | y <- xs, y > x]
    rseq less
    rseq greater
    return $ less ++ [x] ++ greater

quickSortP3 :: NFData a => Ord a => [a] -> [a]
quickSortP3 []     = []
quickSortP3 (x:xs) 
  | length (x:xs) < 10 = quickSortS1 (x:xs)
  | otherwise      = less ++ [x] ++ greater
    where 
     (less,greater) = force ( (quickSortP3 [y | y <- xs, y <= x], quickSortP3 [y | y <- xs, y > x]) 
       `using` (parTuple2 rpar rseq))

mergeSortS1 :: (Ord a) => [a] -> [a]
mergeSortS1 []     = []
mergeSortS1 (x:[]) = [x]
mergeSortS1 ls     = mergeS1 (mergeSortS1 l1) (mergeSortS1 l2)
  where (l1,l2) = splitAt (div (length ls) 2) ls

mergeSortP1 :: (NFData a) => (Ord a) => [a] -> [a]
mergeSortP1 [] = []
mergeSortP1 ls = 
  runPar $ go ls
    where
      go ls
        | length ls < 10 = return $ quickSortS1 ls
        | otherwise  = do
          let (a,b) = splitAt (div (length ls) 2) ls
          i <- spawn $ go a
          j <- spawn $ go b
          a <- get i
          b <- get j
          return (mergeS1 a b)

mergeSortP2 :: NFData a => (Ord a) => [a] -> [a]
mergeSortP2 [] = []
mergeSortP2 ls 
  | length ls < 10 = quickSortS1 ls
  | otherwise      = runEval $ do 
    let (l1, l2) = splitAt (div (length ls) 2) ls
    a <- rpar $ force $ mergeSortP2 l1
    b <- rpar $ force $ mergeSortP2 l2
    rseq a 
    rseq b
    return $ mergeS1 a b 

mergeSortP3 :: NFData a => Ord a => [a] -> [a]
mergeSortP3 [] = []
mergeSortP3 ls
  | length ls < 10 = quickSortS1 ls
  | otherwise      = mergeS1 a b
    where 
     (a,b) = force ( (mergeSortP3 l1, mergeSortP3 l2) 
       `using` (parTuple2 rpar rseq))
     (l1, l2) = splitAt (div (length ls) 2) ls

mergeS1 :: (Ord a) => [a] -> [a] -> [a]
mergeS1 [] ys = ys
mergeS1 xs [] = xs
mergeS1 (x:xs) (y:ys)
  | x < y       = x:(mergeS1 xs (y:ys))
  | otherwise   = y:(mergeS1 (x:xs) ys)

quickSortP1 :: (NFData a) => (Ord a) => [a] -> [a]
quickSortP1 ys = runPar $ go ys 
  where
    go ys
     | length ys < 10 = return $ quickSortS1 ys
     | otherwise     = do
      let pivot = head ys
      l <- spawn $ go $ [y | y <- (tail ys), y <= pivot]
      g <- spawn $ go $ [y | y <- ys, y > pivot]
      less    <- get l
      greater <- get g
      return (less Prelude.++ [pivot] Prelude.++ greater)

main = defaultMain [bench "Sort" (nf sort randomInts),
 
  bench "QuickSequential" (nf quickSortS1 randomInts),
  bench "QuickParMonad" (nf quickSortP1 randomInts),
  bench "QuickEvalMonad" (nf quickSortP2 randomInts),  
  bench "QuickStrategies" (nf quickSortP3 randomInts),
  bench "QuickParPseq" (nf qsort randomInts),
  bench "MergeSequential" (nf mergeSortS1 randomInts), 
  bench "MergeParMonad" (nf mergeSortP1 randomInts),
  bench "MergeEvalMonad" (nf mergeSortP2 randomInts),
  bench "MergeStrategies" (nf mergeSortP3 randomInts),
  bench "MergeParPSeq" (nf msort randomInts)]

randomInts = take 100000 (randoms (mkStdGen 17465864)) :: [Integer]

msort :: NFData a => Ord a => [a] -> [a]
msort [] = []
msort (x:[]) = x:[]
msort xs 
  | length xs < 10 = quickSortS1 xs 
  | otherwise      = force first `par` (force rest `pseq` (mergeS1 first rest))
    where (xs1, xs2) = splitAt (div (length xs) 2) xs
          first = msort xs1
          rest = msort xs2

qsort :: NFData a => Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) 
  | length (x:xs) < 10 = quickSortS1 (x:xs)
  | otherwise = force less `par` (force greater `pseq` (less ++ [x] ++ greater))
    where less    =  qsort [y | y <- xs, y <= x]
          greater =  qsort [y | y <- xs, y >  x]

