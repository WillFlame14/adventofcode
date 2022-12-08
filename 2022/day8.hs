import Data.Char
import Data.List
import qualified Data.Set as Set

-- `map reverse $ transpose grid` is my jam :sunglasses:
main = interact sol

sol :: String -> String
sol input =
    let contents = lines input
        grid = map (map digitToInt) contents :: [[Int]]
        dim = length (grid !! 0)
        vLeft = visibleAcross $ zip [0..] grid
        vRight = map (\(x, y) -> (x, dim - y - 1)) . visibleAcross $ zip [0..] (map reverse grid)
        vTop = map (\(x, y) -> (y, x)) . visibleAcross $ zip [0..] (transpose grid)
        vBottom = map (\(x, y) -> (dim - y - 1, x)) . visibleAcross $ zip [0..] (map reverse $ transpose grid)
        vAll = foldl Set.union Set.empty $ map Set.fromList [vLeft, vRight, vTop, vBottom]
    in show (Set.size vAll, maximum $ concat [[scenicScore dim (x, y) grid | y <- [0..(dim-1)]] | x <- [0..(dim-1)]])

visible :: Int -> [(Int, Int)] -> [Int]
visible _ [] = []
visible threshold ((i, x):xs) = 
    if x > threshold then
        i:(visible x xs)
    else
        visible threshold xs

visibleAcross :: [(Int, [Int])] -> [(Int, Int)]
visibleAcross [] = []
visibleAcross ((i, x):xs) =
    let threshold = head x
        indexes = 0:visible threshold (zip [1..] $ tail x)
        points = map (\y -> (i, y)) indexes
        in points ++ visibleAcross xs

scenicScore :: Int -> (Int, Int) -> [[Int]] -> Int
scenicScore dim (x, y) grid =
    let height = (grid !! x) !! y
        sTop = last $ 0:takeWhile (\i -> x - i >= 0 && (grid !! (x - i)) !! y < height) [1..]
        sLeft = last $ 0:takeWhile (\i -> y - i >= 0 && (grid !! x) !! (y - i) < height) [1..]
        sRight = last $ 0:takeWhile (\i -> y + i < dim && (grid !! x) !! (y + i) < height) [1..]
        sBottom = last $ 0:takeWhile (\i -> x + i < dim && (grid !! (x + i)) !! y < height) [1..]
        csTop = sTop + (if (x - sTop - 1) < 0 then 0 else 1)
        csLeft = sLeft + (if (y - sLeft - 1) < 0 then 0 else 1)
        csRight = sRight + (if (y + sRight + 1) >= dim then 0 else 1)
        csBottom = sBottom + (if (x + sBottom + 1) >= dim then 0 else 1)
        in product [csTop, csLeft, csRight, csBottom]

