import Data.List
import Data.Maybe
import Debug.Trace
import Utils

-- Not that bad except for how the wraparound wasn't symmetric (and instead always preferred to end up on the right side).
main = interact sol

sol :: String -> String
sol input =
    let contents = map read $ lines input
        blended = map snd . mix $ zip [0..] contents
        blended2 = map (map snd) (iterate mix . zip [0..] $ map (*811589153) contents) !! 10
        in show (calcAnswer blended, calcAnswer blended2)

calcAnswer :: [Int] -> Int
calcAnswer blended =
    let zero_i = fromJust $ findIndex (== 0) blended
        indices = map (\i -> (zero_i + i) `mod` (length blended)) [1000, 2000, 3000]
        in sum $ map (blended !!) indices

mix :: [(Int, Int)] -> [(Int, Int)]
mix xs = foldl' (\acc x -> mix' acc x) xs [0..(length xs - 1)]

mix' :: [(Int, Int)] -> Int -> [(Int, Int)]
mix' xs x =
    let n@(_,value) = fromJust $ find (\(i, v) -> i == x) xs
        index = fromJust $ findIndex (\(i, v) -> i == x) xs
        removed = filter (\(i, v) -> i /= x) xs
        new_index = fixIndex (index, value) (length xs)
        in insertAt n new_index removed

-- Given an index and a value, "fixes" it so it's between 0 and length (with correct wraparound).
fixIndex :: (Int, Int) -> Int -> Int
fixIndex (i, v) len =
    let ni = (i + v) `mod` (len - 1)
        in if ni < 0 then ni + len else if (ni == 0 && v /= 0) then len - 1 else ni 

insertAt :: a -> Int -> [a] -> [a]
insertAt n i []
    | i == 0    = [n]
    | otherwise = trace ("something bad occurred " ++ show i) $ [n]
insertAt n i (x:xs)
    | i == 0    = n:x:xs
    | otherwise = x:(insertAt n (i - 1) xs)