
module Round1AALastWord where

import ClassyPrelude

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text

wordToLastWord :: String -> String
wordToLastWord word = foldl' f "" word
  where
    f :: String -> Char -> String
    f [] char = [char]
    f accum@(h:t) char
        | char == h = char : accum
        | [char] < accum = accum ++ [char]
        | otherwise = char : accum

lastWordDefaultMain :: IO ()
lastWordDefaultMain = do
    contents <- tailEx . Text.lines <$> hGetContents stdin
    forM_ (zip contents [1..]) $ \(line, i) ->
        putStrLn $ "Case #" <> tshow i <> ": " <> pack (wordToLastWord (unpack line))
