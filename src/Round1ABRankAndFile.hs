
module Round1ABRankAndFile where

import ClassyPrelude hiding (Vector)

import Data.Int

type RowOrColumn = [Int]
type SideLength = Int

sortInput :: [RowOrColumn] -> [RowOrColumn]
sortInput = sort

solve :: SideLength -> [RowOrColumn] -> RowOrColumn
solve sideLength rowOrColumnList = undefined

exampleRows :: [RowOrColumn]
exampleRows =
    [ [ 1, 2, 8 ]
    , [ 2, 5, 8 ]
    , [ 1, 3, 7 ]
    ]

lastWordDefaultMain :: IO ()
lastWordDefaultMain = undefined -- do
    -- contents <- tailEx . Text.lines <$> hGetContents stdin
    -- forM_ (zip contents [1..]) $ \(line, i) ->
    --     putStrLn $ "Case #" <> tshow i <> ": " <> pack (wordToLastWord (unpack line))
