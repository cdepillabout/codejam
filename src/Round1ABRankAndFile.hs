{-# LANGUAGE LambdaCase #-}

module Round1ABRankAndFile where

import ClassyPrelude hiding (Index, Vector)

import Data.Int
import Data.Array

type RowOrColumn = [Int]
type SideLength = Int
type Row = Int
type Column = Int
type CurrentRow = Int
type CurrentColumn = Int
type Index = (Row, Column)

type Field = (SideLength, CurrentRow, CurrentColumn, Array (Int, Int) (Maybe Int))

sortInput :: [RowOrColumn] -> [RowOrColumn]
sortInput = sort

addFirstRow :: Field -> RowOrColumn -> Field
addFirstRow (sideLength, 0, 0, field) row =
    (sideLength, 1, 0, field // fmap f (zip [0..] row))
  where
    f :: (Column, Int) -> (Index, Maybe Int)
    f (col, item) = ((0, col), Just item)
addFirstRow _ _ = error "got an error"

tryAddRow :: Field -> RowOrColumn -> Maybe Field
tryAddRow (sideLength, curRow, curCol, field) row =
    undefined

addToField :: Field -> RowOrColumn -> Field
addToField field rowOrColumn = undefined

solve :: SideLength -> [RowOrColumn] -> RowOrColumn
solve sideLength rowOrColumnList = undefined

emptyField :: SideLength -> Field
emptyField sideLength =
    (sideLength, 0, 0,
        array ((0, 0), (len, len))
            [ ((x, y), Nothing) | x <- [0..len], y <- [0..len] ])
  where
    len = sideLength - 1

printField :: Field -> IO ()
printField (_, _, _, field) =
    let items = assocs field
        sameRowItems = groupBy (\((aX, _), _) ((bX, _), _) -> aX == bX) items
    in for_ sameRowItems $ \sameRow -> do
        for_ sameRow $ \case
                            ((_, _), Just num) -> putStr $ tshow num <> " "
                            ((_, _), Nothing) -> putStr $ "_" <> " "
        putStrLn ""

exampleRows :: [RowOrColumn]
exampleRows =
    [ [ 1, 2, 3 ]
    , [ 2, 3, 5 ]
    , [ 3, 5, 6 ]
    , [ 2, 3, 4 ]
    , [ 1, 2, 3 ]
    ]

lastWordDefaultMain :: IO ()
lastWordDefaultMain = undefined -- do
    -- contents <- tailEx . Text.lines <$> hGetContents stdin
    -- forM_ (zip contents [1..]) $ \(line, i) ->
    --     putStrLn $ "Case #" <> tshow i <> ": " <> pack (wordToLastWord (unpack line))
