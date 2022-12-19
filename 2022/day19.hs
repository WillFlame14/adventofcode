import Data.List
import Debug.Trace

-- Finally, an easier day. Thought I might need dp at first, but turns out brute force DFS (with slight optimization) is pretty good. Some important observations:
-- 1. You can build at most 1 robot per minute.
-- 2. You never need to build more machines than the maximum cost for that resource for any machine (as a result of #1).
-- 3. If you decide to build a certain machine next, it is optimal to build it immediately once you have enough resources (i.e. waiting is strictly worse).
-- With these 3 observations, brute force takes ~10s to run both parts (even uncompiled).
main = interact sol

type State = ([Int], [Int])
type Cost = (Int, Int)

sol :: String -> String
sol input =
    let contents = lines input
        blueprints = map parse contents
        base_state = ([1, 0, 0, 0], [0, 0, 0, 0])
        part1 = map (last . snd . solve 24 0 base_state) blueprints
        part2 = map (last . snd . solve 32 0 base_state) $ take 3 blueprints
        in show (sum . map (\(i, g) -> i * g) $ zip [1..] part1, product part2)

parse :: String -> [(Int, Int)]
parse xs =
    let parts = words xs
        ore = (read (parts !! 6), 0)
        clay = (read (parts !! 12), 0)
        obsidian = (read (parts !! 18), read (parts !! 21))
        geode = (read (parts !! 27), read (parts !! 30))
        in [ore, clay, obsidian, geode]

solve :: Int -> Int -> State -> [Cost] -> State
solve max_turn turn state@(m@[rm, cm, om, gm], s@[r, c, o, g]) blueprint@[(r1, _), (r2, _), (r3, c1), (r4, o1)]
    | turn == max_turn                                          = state     -- Out of time
    | (rm > maximum [r1, r2, r3, r4]) || (cm > c1) || (om > o1) = state     -- Too many machines
    | (r - rm >= r4) && (o - om >= o1)                          = state     -- Too many resources (unsure if this actually a necessary condition)
    | otherwise =
        let new_s = map (\(x, y) -> x + y) $ zip m s
            can_buy = buyable state blueprint
            results = map (\b ->
                let new_state = buyMachine (m, new_s) (blueprint, b)
                    in solve max_turn (turn + 1) new_state blueprint
                ) can_buy
            in foldr1 (\x@([_, _, om, gm], [r, c, o, g]) acc@([_, _, om', gm'], [r', c', o', g']) ->
                -- Prioritize valuable resources and their machines first
                if (g > g') || (g == g' && gm > gm') ||
                    (g == g' && o > o') || (g == g' && o == o' && om > om') ||
                    (g == g' && o == o' && c > c') then x else acc
                ) results

-- Given a state and a blueprint, determines the indexes of machines that are buyable (and were not buyable on the previous turn).
buyable :: State -> [Cost] -> [Int]
buyable state@(m@[rm, cm, om, gm], s@[r, c, o, g]) [(r1, _), (r2, _), (r3, c1), (r4, o1)] =
    findIndices (== True) [
        r >= r1 && r - rm < r1,
        r >= r2 && r - rm < r2,
        r >= r3 && c >= c1 && c - cm < c1,
        r >= r4 && o >= o1,
        True
    ]

-- Given a state and a cost (given as (blueprint, index to buy)), returns a new state after buying the machine.
buyMachine :: State -> ([Cost], Int) -> State
buyMachine ([rm, cm, om, gm], [r, c, o, g]) ([(r1, _), (r2, _), (r3, c1), (r4, o1)], index)
    | index == 0    = ([rm + 1, cm, om, gm], [r - r1, c, o, g])
    | index == 1    = ([rm, cm + 1, om, gm], [r - r2, c, o, g])
    | index == 2    = ([rm, cm, om + 1, gm], [r - r3, c - c1, o, g])
    | index == 3    = ([rm, cm, om, gm + 1], [r - r3, c, o - o1, g])
    | index == 4    = ([rm, cm, om, gm], [r, c, o, g])