import Control.Parallel (par, pseq)
import Control.DeepSeq (force)
import Criterion.Main
import System.Random (mkStdGen, randoms)

--parScan1 :: (Num a) => (a -> a -> a) -> [a] -> [a]
parScan1 f []       = []
parScan1 f (x:[])   = [x]
parScan1 f xs       =  eSum f xs $ downSweep f $ clear $ upSweep f xs
    
--eSum :: (Num a) => [a] -> [a] -> [a]
eSum f (x:[]) (y:[]) = [f x y]
eSum f xs ys 
	| length xs < t = eSum' f xs ys
	| otherwise 	= force first `par` (force second `pseq` (first ++ second))  
	where 
		first 		= eSum f xs1 ys1
		second 		= eSum f xs2 ys2
		(xs1, xs2) 	= split xs
		(ys1, ys2) 	= split ys

eSum' f (x:[]) (y:[]) = [f x y]
eSum' f (x:xs) (y:ys) = (f x y) : (eSum' f xs ys)

--split :: [a] -> ([a], [a])
split xs = (take half xs, drop half xs)
    where half = (length xs) `div` 2

--downSweep :: (a -> a -> a) -> [a] -> [a]
downSweep f (x:[]) 	= [x]
downSweep f xs 		= force first `par` (force second `pseq` (first ++ second))
	where
		first		= downSweep f xs1'
		second 		= downSweep f xs2'
		xs1'		= (init xs1) ++ [(last xs2)]
		xs2'		= (init xs2) ++ [(f (last xs1) (last xs2))]
		(xs1, xs2)	= split xs

--upSweep :: (a -> a -> a) -> [a] -> [a]
upSweep f (x:[]) 	= [x]
upSweep f (x:y:[]) 	= x:(f x y):[]
upSweep f xs = force first `par` (force second `pseq` ((first ++ (init second)) ++ [f (last first) (last second)]))
	where 
		first 		= upSweep f xs1
		second		= upSweep f xs2
		(xs1, xs2) 	= split xs

clear xs = (init xs)++[iden]

parScan2 f []       = []
parScan2 f (x:[])   = [x]
parScan2 f (x:y:[]) = x:[f x y]
parScan2 f xs       
	| length xs < t = myScan1 f xs
	| otherwise		= force first `par` (force second `pseq` (first ++ (map (f (last first)) second)))
    where 
        (xs1, xs2)  = split xs
        first       = parScan2 f xs1
        second      = parScan2 f xs2

myScan1 :: (a -> a -> a) -> [a] -> [a]
myScan1 f (x:[]) = x:[]
myScan1 f (x:xs) = x:myScan1' f x xs

myScan1' f s (x:[]) = (f s x):[]
myScan1' f s (x:xs) = ish:(myScan1' f ish xs) 
    where ish = f s x

iden :: (Num a) => a
iden = 0

op :: (Num a) => (a -> a -> a)
op = (+)

t :: (Num a) => a
t = 8

main = defaultMain [bench "Parallel1" (nf (parScan1 op) randomInts), 
	bench "Parallel2" (nf (parScan2 op) randomInts),
	bench "Sequential" (nf (myScan1 op) randomInts)]
randomInts = take 50000 (randoms (mkStdGen 211570155)) :: [Integer]
