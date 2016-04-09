
module PrelimASheep where

import ClassyPrelude

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text

type Digit = Char

emptyDigits :: Set Digit
emptyDigits = Set.empty

digitFromInteger :: Integer -> Set Digit
digitFromInteger int = Set.fromList $ show int

seenDigits :: Set Digit -> Integer -> Set Digit
seenDigits set int = Set.union set (digitFromInteger int)

seenAllDigits :: Set Digit -> Bool
seenAllDigits set = all (`Set.member` set) ['0'..'9']

type I = Integer
type N = Integer

updateSeenDigits :: Set Digit -> N -> I -> Set Digit
updateSeenDigits set n i = seenDigits set (n * i)

doSheep :: Integer -> Maybe Integer
doSheep 0 = Nothing
doSheep n = fst $ foldWhile n emptyDigits f
  where
    f :: N -> I -> Set Digit -> (Maybe Integer, Set Digit)
    f n i set
        | seenAllDigits set = (Just (n * (i - 1)), set)
        | otherwise         = (Nothing, seenDigits set (i * n))

foldWhile :: N -> Set Digit -> (N -> I -> Set Digit -> (Maybe Integer, Set Digit)) -> (Maybe Integer, Set Digit)
foldWhile n set' f = go 1 set'
  where
    go :: I -> Set Digit -> (Maybe Integer, Set Digit)
    go i set =
        case f n i set of
            (Nothing, newSet) -> go (i + 1) newSet
            (Just ans, newSet) -> (Just ans, newSet)

sheepDefaultMain :: IO ()
sheepDefaultMain = do
    contents <- tailEx . Text.lines <$> hGetContents stdin
    forM_ (zip contents [1..]) $ \(line, i) ->
        case readMay line >>= doSheep of
            Just int -> putStrLn $ "Case #" <> tshow i <> ": " <> tshow int
            Nothing -> putStrLn $ "Case #" <> tshow i <> ": INSOMNIA"
