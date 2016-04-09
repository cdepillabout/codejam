
module PrelimBPancakes where

import ClassyPrelude

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Text as Text
import Debug.Trace

data Direction = Happy | Sad
    deriving Eq

instance Show Direction where
    show Happy = "+"
    show Sad = "-"

type PancakeStack = Vector Direction

readPancakeStack :: String -> Maybe PancakeStack
readPancakeStack input = fromList <$> (sequence $ fmap f input)
  where
    f :: Char -> Maybe Direction
    f '+' = Just Happy
    f '-' = Just Sad
    f other = Nothing

reverseDir :: Direction -> Direction
reverseDir Happy = Sad
reverseDir Sad = Happy

ignoreCorrect :: CorrectDirection -> PancakeStack -> (PancakeStack, PancakeStack)
ignoreCorrect dir ps =
    let (correct, wrong) = Vector.span (== dir) $ Vector.reverse ps
    in (Vector.reverse correct, Vector.reverse wrong)

flipPancakeStack :: PancakeStack -> PancakeStack
flipPancakeStack = fmap reverseDir . Vector.reverse

type I = Integer

type CorrectDirection = Direction
type Flips = Integer

allSame :: PancakeStack -> Bool
allSame ps = all (== Vector.head ps) ps

solve' :: I -> PancakeStack -> CorrectDirection -> (Flips, PancakeStack)
solve' i ps correctDir = ("solve called with case: i = " <> show i <> ", ps = " <> show ps <> ", dir = " <> show correctDir)
    `trace` solve2 i ps correctDir

solve2 :: I -> PancakeStack -> CorrectDirection -> (Flips, PancakeStack)
solve2 i ps dir
    | i > 15 = error "too much"
    | otherwise =
        let (correct, wrong) = ignoreCorrect dir ps
        in case Vector.length wrong of
               0 ->
                   {- ("no wrongs: i = " <> show i <> ", ps = " <> show ps <> ", wrong = " <> show wrong <> ", correct = " <> show correct) `trace` -} (i, ps)
               n ->
                   let (flips, resultPancakeStack) = solve2 i wrong $ reverseDir dir
                       resStack = flipPancakeStack resultPancakeStack <> correct
                   in {- ("last case: flips = " <> show flips <> ", ps = " <> show ps <> ", resstack = " <> show resStack) `trace` -} (flips + 1, resStack)

solve :: PancakeStack -> Integer
solve ps = fst $ solve2 0 ps Happy

readStack :: Text -> Maybe PancakeStack
readStack input = readPancakeStack $ unpack input

pancakesDefaultMain :: IO ()
pancakesDefaultMain = do
    contents <- tailEx . Text.lines <$> hGetContents stdin
    forM_ (zip contents [1..]) $ \(line, i) ->
        let maybeStack :: Maybe PancakeStack = readStack line
        in case fmap solve maybeStack of
            Just int -> putStrLn $ "Case #" <> tshow i <> ": " <> tshow int
            Nothing -> putStrLn $ "error, could not solve, case #" <> tshow i <> ", line: " <> line
