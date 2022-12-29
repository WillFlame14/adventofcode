module Utils (
    Point,
    Point3,
    chunked,
    wordsWhen,
    at,
    pointAdd,
    point3Add,
    point3Mult,
    findInGrid
) where

import Data.List

type Point = (Int, Int)
type Point3 = (Int, Int, Int)

-- Splits an array into chunks of the given size (the last array may be slightly smaller).
chunked :: [a] -> Int -> [[a]]
chunked [] size = []
chunked xs size =
    let (group, rest) = splitAt size xs
    in group:chunked rest size

wordsWhen :: (a -> Bool) -> [a] -> [[a]]
wordsWhen _ [] = []
wordsWhen func xs =
    let (a, b) = span func xs
    in a:(wordsWhen func $ if null b then [] else tail b)

at :: [[a]] -> Point -> a
at grid (x, y) = (grid !! y) !! x

pointAdd :: Point -> Point -> Point
pointAdd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

point3Add :: Point3 -> Point3 -> Point3
point3Add (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

point3Mult :: Point3 -> Int -> Point3
point3Mult (x, y, z) a = (a * x, a * y, a * z)

findInGrid :: (Char -> Bool) -> [String] -> [Point]
findInGrid f grid = concat . map (\(i, row) -> map (\col -> (col, i)) $ findIndices f row) $ zip [0..] grid