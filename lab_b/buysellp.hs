import Data.Array.Repa as R
import Data.Functor.Identity
import Data.Maybe
import Data.List

mainMain :: [Int] -> Array U DIM0 (Int, Int, Int)
mainMain as = buySellP $ (fromListUnboxed (Z :. ((length as)::Int)) as :: Array U DIM1 Int)


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