import Data.List
import qualified Data.Set as Set
import Debug.Trace
import Utils

type Wind = (Char, Point)

main = interact sol

sol :: String -> String
sol input =
    let grid = lines input
        winds = map (\p -> (grid `at` p, p)) $ findInGrid (`elem` "<>v^") grid
        walls = Set.fromList $ findInGrid (=='#') grid
        all_spaces = filter (\p -> Set.notMember p walls) [(x, y) | y <- [0..(length grid - 1)], x <- [0..(length (grid !! y))]]
        start = (1, 0)
        end = (length (grid !! 0) - 2, length grid - 1)
        (part1, wind1) = travel grid all_spaces winds [start] end
        part2 =
            let (dist2, wind2) = travel grid all_spaces wind1 [end] start
                (dist3, wind3) = travel grid all_spaces wind2 [start] end
                in part1 + dist2 + dist3
        in show (part1, part2)

findWinds :: [(Int, String)] -> [Wind]
findWinds [] = []
findWinds ((i,x):xs) = 
    let wind_cols = filter (\(i, x) -> x `elem` "<>v^") $ zip [0..] x
        winds = map (\(col, wind) -> (wind, (col, i))) wind_cols
        in findWinds xs ++ winds

moveWinds :: [[Char]] -> [Wind] -> [Wind]
moveWinds _ [] = []
moveWinds grid ((dir, (x, y)):xs) =
    let (new_pos, wrapped_pos) = case dir of
            '<' -> ((x - 1, y), (last $ findIndices (/= '#') (grid !! y), y))
            '>' -> ((x + 1, y), (head $ findIndices (/= '#') (grid !! y), y))
            '^' -> ((x, y - 1), (x, last $ findIndices (\row -> (row !! x) /= '#') grid))
            'v' -> ((x, y + 1), (x, head $ findIndices (\row -> (row !! x) /= '#') grid))
        in (dir, if grid `at` new_pos == '#' then wrapped_pos else new_pos):moveWinds grid xs

possible_moves = [(0, 0), (0, 1), (0, -1), (1, 0), (-1, 0)]

travel :: [[Char]] -> [Point] -> [Wind] -> [Point] -> Point -> (Int, [Wind])
travel grid all_spaces winds [] dest = error "ohno"
travel grid all_spaces winds points dest =
    if any (== dest) points then
        (0, winds)
    else
        let next_winds = moveWinds grid winds
            obstruct = Set.fromList $ map snd next_winds
            free_spaces = Set.fromList $ filter (`Set.notMember` obstruct) all_spaces
            potential_steps = nub . concat $ map (\p -> map (pointAdd p) possible_moves) points
            valid_steps = filter (`Set.member` free_spaces) potential_steps
            (remaining_steps, final_winds) = travel grid all_spaces next_winds valid_steps dest
            in (1 + remaining_steps, final_winds)