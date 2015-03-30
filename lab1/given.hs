import Data.Complex
import Data.Array
import Data.Bits
import System.Random
import Data.Random.Normal
import Criterion.Main
import Control.Parallel
import Control.DeepSeq (force, NFData)
import Control.Monad.Par  
import Control.Parallel.Strategies

-- file given.hs for use with Lab 1 Part 1 of the Chalmers PFP Course
-- Please write your names in the file if submitting it


-- generating input for FFT or DFT. Borrowed from Simon Marlow I believe.
mX, mY, sdX, sdY :: Float
mX = 0
mY = 0
sdX = 0.5
sdY = 1.5 
n = 18000

generate2DSamplesList :: Int           -- number of samples to generate
                  -> Float -> Float    -- X and Y mean
                  -> Float -> Float    -- X and Y standard deviations
                  -> IO [Complex Float]
generate2DSamplesList n mx my sdx sdy = do
  let gen = mkStdGen 657654532 
  let (genx, geny) = split gen
      xsamples = normals' (mx,sdx) genx
      ysamples = normals' (my,sdy) geny
  return $ zipWith (:+) (take n xsamples) ysamples

randomFloats = ((generate2DSamplesList n mX mY sdX sdY)) :: IO [Complex Float]
main = do 
          rnd <- randomFloats
          print $ last $ force $ fft2 rnd

-- Task 1
divConq :: (prob -> Bool)              -- is the problem indivisible?
            -> (prob -> [prob])        -- split
            -> ([sol] -> sol)          -- join
            -> (prob -> sol)           -- solve a sub-problem
            -> (prob -> sol)

divConq indiv split join f prob = undefined



-- Task 2


-- twiddle factors
tw :: Int -> Int -> Complex Float
tw n k = cis (-2 * pi * fromIntegral k / fromIntegral n)

dft :: [Complex Float] -> [Complex Float]
dft xs = [ sum [ xs!!j * tw n (j*k) | j <- [0..n']] | k <- [0..n']]
  where
    n = length xs
    n' = n-1

fftPar :: [Complex Float] -> [Complex Float]
fftPar [a] = [a]
fftPar as 
  | length as < 10 = fft as
  | otherwise      = runEval $ do
    let (cs, ds) = bflySPar as
    ls <- rpar $ force fftPar cs
    rs <- rseq $ force fftPar ds
    rseq ls
    return (interleave ls rs)

bflySPar :: [Complex Float] -> ([Complex Float], [Complex Float])
bflySPar as 
  | length as < 10  = bflyS as
  | otherwise       = runEval $ do 
    let (ls,rs) = halve as
    los <- rpar $ force zipWith (+) ls rs
    ros <- rseq $ force zipWith (-) ls rs
    rts <- rseq $ force zipWith (*) ros [tw (length as) i | i <- [0..(length ros) -1]] 
    rseq los
    return (los, rts)

-- In case you are wondering, this is the Decimation in Frequency (DIF) 
-- radix 2 Cooley-Tukey FFT

fft2 :: [Complex Float] -> [Complex Float]
fft2 [a] = [a]
fft2 as 
  | length as < 40  = fft as
  | otherwise       = interleave ls rs
    where
      (ls,rs) = force ( (fft2 cs, fft2 ds) `using` (parTuple2 rpar rseq))
      (cs,ds) = bflyS as

bflyS2 :: [Complex Float] -> ([Complex Float], [Complex Float])
bflyS2 as = runEval $ do 
      let (ls,rs) = halve as
      los <- rpar $  zipWith (+) ls rs
      ros <- rseq $  zipWith (-) ls rs
      rts <- rseq $ zipWith (*) ros [tw (length as) i | i <- [0..(length ros) - 1]]
      rseq los
      return (los,rts)

fft :: [Complex Float] -> [Complex Float]
fft [a] = [a]
fft as = interleave ls rs
  where
    (cs,ds) = bflyS as
    ls = fft cs
    rs = fft ds

interleave [] bs = bs
interleave (a:as) bs = a : interleave bs as

bflyS :: [Complex Float] -> ([Complex Float], [Complex Float])
bflyS as = (los,rts)
  where
    (ls,rs) = halve as
    los = zipWith (+) ls rs
    ros = zipWith (-) ls rs
    rts = zipWith (*) ros [tw (length as) i | i <- [0..(length ros) - 1]]


-- missing from original file
halve as = splitAt n' as
  where
    n' = div (length as + 1) 2







