
module PrelimCJamCoins where

import Prelude (head)
import ClassyPrelude hiding (head)

import qualified Data.Vector.Unboxed as UnboxedVector
import qualified Data.Vector.Unboxed.Mutable as UnboxedVectorMutable
import qualified Data.Text as Text
import Debug.Trace
import Math.NumberTheory.Primes.Factorisation (factorise)
import Math.NumberTheory.Primes.Testing (isPrime)
import Numeric

type Base = Integer

type Candidate = UnboxedVector.Vector Char

candidateToString :: Candidate -> String
candidateToString candidate = "1" <> UnboxedVector.toList candidate <> "1"

-- candidateToInteger :: Base -> Candidate -> [(Integer, String)]
candidateToInteger :: Base -> Candidate -> Integer
candidateToInteger base candidate =
    fst $ head $ readInt base (\c -> '0' == c || '1' == c) cToInt $ candidateToString candidate
  where
    cToInt :: Char -> Int
    cToInt '0' = 0
    cToInt '1' = 1

startingCandidate :: Int -> Candidate
startingCandidate length = UnboxedVector.replicate length '0'

nextCandidate :: Candidate -> Candidate
nextCandidate candidate' = go candidate' (length candidate')
  where
    go :: Candidate -> Int -> Candidate
    go candidate 0 = candidate
    go candidate n =
        let actualN = n - 1
            elem' = candidate UnboxedVector.! actualN
        in case elem' of
               '0' ->
                    UnboxedVector.modify (\vector -> UnboxedVectorMutable.write vector actualN '1') candidate
               '1' ->
                    let updatedCandidate = UnboxedVector.modify (\vector -> UnboxedVectorMutable.write vector actualN '0') candidate
                    in go updatedCandidate actualN

candidateIsPrimeForBase :: Candidate -> Base -> Bool
candidateIsPrimeForBase candidate base = isPrime $ candidateToInteger base candidate

candidateIsPrimeForAnyBase :: Candidate -> Bool
candidateIsPrimeForAnyBase candidate = or $ fmap (candidateIsPrimeForBase candidate) [2..10]

isLastCandidate :: Candidate -> Bool
isLastCandidate = all (== '1')

findJamCoins :: Int -> Int -> [Candidate]
findJamCoins length total =
    let newLength = length - 2
        initialCandidate = startingCandidate newLength
    in go initialCandidate (0, [])
  where
    go :: Candidate -> (Int, [Candidate]) -> [Candidate]
    go candidate (accumNum, accum) = if candidateIsPrimeForAnyBase candidate then f (accumNum, accum) else f (accumNum + 1, candidate : accum)
      where
        f :: (Int, [Candidate]) -> [Candidate]
        f (newAccumNum, newAccum) =
            if isLastCandidate candidate || newAccumNum == total then newAccum else go (nextCandidate candidate) (newAccumNum, newAccum)

findJamCoinsPretty :: Int -> Int -> [String]
findJamCoinsPretty length total = candidateToString <$> findJamCoins length total

findFactor :: Candidate -> Base -> Integer
findFactor candidate base = fst $ head $ factorise $ candidateToInteger base candidate

findAllFactors :: Candidate -> [Integer]
findAllFactors candidate = findFactor candidate <$> [2..10]

jamCoinsDefaultMain :: IO ()
jamCoinsDefaultMain = do
    putStrLn "Case #1:"
    let allCandidates = findJamCoins 32 500
        allFactors = fmap findAllFactors allCandidates
    forM_ (zip allCandidates allFactors) $ \(candidate, factors) ->
        putStrLn $ pack (candidateToString candidate) <> " " <> unwords (fmap tshow factors)
