import Data.Char
import Data.List

main = interact sol

sol :: String -> String
sol input =
    let contents = lines input
        assigns = map (splitOn ',') contents
        splitted = map (\(x, y) -> (splitOn '-' x, splitOn '-' y)) assigns
        nums = map (\(x, y) -> ((read $ fst x, read $ snd x), (read $ fst y, read $ snd y)) :: ((Int, Int), (Int, Int))) splitted
    in show (length $ filter overlap nums, length $ filter overlap2 nums)

splitOn :: Char -> String -> (String, String)
splitOn del x =
    let (a, b) = span (/= del) x
    in (a, tail b)

overlap :: ((Int, Int), (Int, Int)) -> Bool
overlap ((x1, x2), (y1, y2)) = (x1 <= y1 && x2 >= y2) || (x1 >= y1 && x2 <= y2)

overlap2 :: ((Int, Int), (Int, Int)) -> Bool
overlap2 ((x1, x2), (y1, y2)) = (x1 <= y1 && y1 <= x2) || (y1 <= x1 && x1 <= y2)