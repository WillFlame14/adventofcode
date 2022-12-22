import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import Debug.Trace

type Point = (Int, Int)

main = interact sol

sol :: String -> String
sol input =
    let contents = lines input
        pairs = map parse contents
        x_ranges = map (intersectY 2000000) pairs
        part1 = Set.size . x_range $ x_ranges
        part2 = let (x, y) = fromJust $ check_edges pairs . concat $ map edge_points pairs in x * 4000000 + y
        in show (part1, part2)

parse :: String -> (Point, Point)
parse xs =
    let parts = words xs
        [x1, y1, x2] = map (\i -> read . drop 2 . init $ parts !! i) [2, 3, 8]
        y2 = read . drop 2 $ parts !! 9
        in ((x1, y1), (x2, y2))

dist :: (Point, Point) -> Int
dist ((x1, y1), (x2, y2)) = abs (x2 - x1) + abs (y2 - y1)

intersectY :: Int -> (Point, Point) -> (Int, Int)
intersectY y p@((sx, sy), _) =
    let radius = dist p
        x_left = radius - abs (sy - y)
        in if x_left <= 0 then (0, 0) else (sx - x_left, sx + x_left)

x_range :: [(Int, Int)] -> Set.Set Int
x_range ps = foldr (\(s, e) acc -> Set.union acc (Set.fromList [s..(e-1)])) Set.empty ps

edge_points :: (Point, Point) -> [Point]
edge_points p@((sx, sy), (bx, by)) =
    let radius = dist p
        top_left = map (\dx -> (sx - radius - 1 + dx, sy + dx)) [0..radius]
        top_right = map (\dx -> (sx + dx, sy + radius + 1 - dx)) [0..radius]
        bottom_right = map (\dx -> (sx + radius + 1 - dx, sy - dx)) [0..radius]
        bottom_left = map (\dx -> (sx - dx, sy - radius - 1 + dx)) [0..radius]
        in filter (\(x, y) -> x >= 0 && x <= 4000000 && y >= 0 && y <= 4000000) $ concat [top_left, top_right, bottom_right, bottom_left]

check_point :: [(Point, Point)] -> Point -> Bool
check_point pairs x = all (\p@(s, b) -> dist (s, x) > dist p) pairs

check_edges:: [(Point, Point)] -> [Point] ->  Maybe Point
check_edges pairs xs = find (check_point pairs) xs
