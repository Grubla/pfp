import Data.Array.Repa as R
import Data.Maybe
import Data.List
import Data.Functor.Identity
import Control.DeepSeq (NFData)
import Criterion.Main
import System.Random (mkStdGen, randoms)

main = defaultMain [bench "Sequential" (whnf mainMain randomInts),
		bench "Parallel" (whnf mainMainP randomInts),
		bench "ParallelG" (whnf mainMainG randomInts),
		bench "SequentialG" (whnf mainMainGS randomInts)]

randomInts = take 10000 (randoms (mkStdGen 1337)) :: [Int]

mainMain :: [Int] -> Array U DIM0 (Int, Int, Int)
mainMain as = buySell $ (fromListUnboxed (Z :. ((length as)::Int)) as :: Array U DIM1 Int)


buySell :: Array U DIM1 Int -> Array U DIM0 (Int, Int, Int)
buySell as = 
	foldS (\b@(_, _, bp) c@(_, _, cp) -> if cp > bp then c else b)
		(0, 0, 0) $ getbuySellTriples as

getbuySellTriples :: Array U DIM1 Int -> Array U DIM1 (Int, Int, Int)	
getbuySellTriples as = 
	computeS $ R.map (\(i, i2) -> (i, i2, ((as!(Z:.i2))-(as!(Z:.i))))) $ getIndexTuples as

getIndexTuples :: Array U DIM1 Int -> Array U DIM1 (Int, Int)
getIndexTuples as = 
	computeS $ R.map (\(i) -> (i, getIDByMax i i as)) ids
	where 
		(Z:.n) = extent as
		ids = (fromListUnboxed (Z :. (n::Int)) [0..(n-1)] :: Array U DIM1 Int)

getIDByMax :: Int -> Int -> Array U DIM1 Int -> Int
getIDByMax bestID currID as
	| currID == n 							= bestID
	| (as!(Z:.currID))>(as!(Z:.bestID)) 	= getIDByMax currID (currID+1) as
	| otherwise								= getIDByMax bestID (currID+1) as
		where (Z:.n) = extent as


--PARALLEL
mainMainP :: [Int] -> Array U DIM0 (Int, Int, Int)
mainMainP as = buySellP $ (fromListUnboxed (Z :. ((length as)::Int)) as :: Array U DIM1 Int)


buySellP :: Array U DIM1 Int -> Array U DIM0 (Int, Int, Int)
buySellP as = runIdentity $ do
	result <- foldP (\b@(_, _, bp) c@(_, _, cp) -> if bp > cp then b else c)
		(0, 0, 0) $ getbuySellTriplesP as
	return result

getbuySellTriplesP :: Array U DIM1 Int -> Array U DIM1 (Int, Int, Int)	
getbuySellTriplesP as = runIdentity $ do
	computeP $ R.map (\(i, i2) -> (i, i2, ((as!(Z:.i2))-(as!(Z:.i))))) $ getIndexTuplesP as

getIndexTuplesP :: Array U DIM1 Int -> Array U DIM1 (Int, Int)
getIndexTuplesP as = runIdentity $ do
	computeP $ R.map (\(i) -> (i, getIDByMaxP i i as)) ids
	where 
		(Z:.n) = extent as
		ids = (fromListUnboxed (Z :. (n::Int)) [0..(n-1)] :: Array U DIM1 Int)

getIDByMaxP :: Int -> Int -> Array U DIM1 Int -> Int
getIDByMaxP bestID currID as
	| currID == n 							= bestID
	| (as!(Z:.currID))>(as!(Z:.bestID)) 	= getIDByMaxP currID (currID+1) as
	| otherwise								= getIDByMaxP bestID (currID+1) as
		where (Z:.n) = extent as

mainMainG :: [Int] -> Array U DIM0 (Int, Int, Int)
mainMainG as = buySellG $ (fromListUnboxed (Z :. ((length as)::Int)) as :: Array U DIM1 Int)

buySellG :: Array U DIM1 Int -> Array U DIM0 (Int, Int, Int)
buySellG as = runIdentity $ do
	foldP (\a@(_, _, p1) b@(_, _, p2) -> if p1 < p2 then b else a) (0,0,0)
	$ R.map (\i -> 
		(i, (bestSell!(Z:.i)), ((as!(Z:.(bestSell!(Z:.i)))) - as!(Z:.i)))) ids
	where
		(Z:.n) = extent as
		bestSell = bestSell' as (n - 1) (n - 1)
		ids = (fromListUnboxed (Z :. (n::Int)) [0..(n-1)] :: Array U DIM1 Int)

bestSell' :: Array U DIM1 Int -> Int -> Int -> Array U DIM1 Int
bestSell' as bestIndex 0					= arrIdx bestIndex
bestSell' as bestIndex currIndex
	| as!(Z:.bestIndex) >= as!(Z:.currIndex)	= computeS $ R.append (bestSell' as bestIndex (currIndex-1)) (arrIdx bestIndex)
	| otherwise									= computeS $ R.append (bestSell' as currIndex (currIndex-1)) (arrIdx currIndex)

arrIdx :: Int -> Array U DIM1 Int
arrIdx i = (fromListUnboxed (Z :. (1::Int)) [i] :: Array U DIM1 Int)


mainMainGS :: [Int] -> Array U DIM0 (Int, Int, Int)
mainMainGS as = buySellGS $ (fromListUnboxed (Z :. ((length as)::Int)) as :: Array U DIM1 Int)

buySellGS :: Array U DIM1 Int -> Array U DIM0 (Int, Int, Int)
buySellGS as = runIdentity $ do
	foldS (\a@(_, _, p1) b@(_, _, p2) -> if p1 < p2 then b else a) (0,0,0)
	$ R.map (\i -> 
		(i, (bestSell!(Z:.i)), ((as!(Z:.(bestSell!(Z:.i)))) - as!(Z:.i)))) ids
	where
		(Z:.n) = extent as
		bestSell = bestSellS' as (n - 1) (n - 1)
		ids = (fromListUnboxed (Z :. (n::Int)) [0..(n-1)] :: Array U DIM1 Int)

bestSellS' :: Array U DIM1 Int -> Int -> Int -> Array U DIM1 Int
bestSellS' as bestIndex 0					= arrIdxS bestIndex
bestSellS' as bestIndex currIndex
	| as!(Z:.bestIndex) >= as!(Z:.currIndex)	= computeS $ R.append (bestSellS' as bestIndex (currIndex-1)) (arrIdxs bestIndex)
	| otherwise									= computeS $ R.append (bestSellS' as currIndex (currIndex-1)) (arrIdxs currIndex)

arrIdxS :: Int -> Array U DIM1 Int
arrIdxS i = (fromListUnboxed (Z :. (1::Int)) [i] :: Array U DIM1 Int)