import Data.Char
import Data.List

-- items are the actual item worry numbers (for part 1 only)
-- mod_items is an array of the item worry numbers, mod the test value for each monkey (in order)
-- Part 2 works by maintaining mod_items instead of items, using modular arithmetic.
data Monkey = Monkey { num::Int, op::(Char, Int), items::[Int], mod_items::[[Int]], test::Int, dests::(Int, Int), inspects::Int } deriving Show

main = interact sol

sol :: String -> String
sol input =
    let contents = lines input
        raw_monkeys = wordsWhen (/= "") contents
        monkeys = map parseMonkey raw_monkeys
        mods = map test monkeys
        mod_monkeys = map (calcMods mods) monkeys
        part1_monkeys = (iterate doRound monkeys) !! 20
        part2_monkeys = (iterate (doRound2 mods) mod_monkeys) !! 10000
        in show (map (product . take 2 . reverse . sort . map inspects) [part1_monkeys, part2_monkeys])

wordsWhen :: (a -> Bool) -> [a] -> [[a]]
wordsWhen _ [] = []
wordsWhen func xs =
    let (a, b) = span func xs
    in a:(wordsWhen func $ if null b then [] else tail b)

-- Parses a (single) monkey from an array of strings.
-- Note that any operation assumes the new value is "old" multiplied/added with something.
-- If the mulitplier/addend is "old", a dummy value of 0 is used instead.
-- Additionally, note that the list of items is reversed.
-- (i.e. new items are added at the front, items are thrown starting from the back)
parseMonkey :: [String] -> Monkey
parseMonkey xs =
    let num = digitToInt $ (xs !! 0) !! 7
        items = reverse . map (\x -> read (if last x == ',' then init x else x)) . drop 2 $ words (xs !! 1)
        op =
            let [cmd, val] = drop 4 $ words (xs !! 2)
            in (head cmd, if val == "old" then 0 else read val)
        [test, dtrue, dfalse] = map (read . last . words) $ drop 3 xs
        in Monkey num op items [[]] test (dtrue, dfalse) 0

-- Calculates the mod_items array for a monkey.
calcMods :: [Int] -> Monkey -> Monkey
calcMods mods monkey = monkey { mod_items = map (\x -> map (\y -> x `mod` y) mods) $ items monkey }

-- Performs a round for the given array of monkeys (part 1).
doRound :: [Monkey] -> [Monkey]
doRound monkeys = foldl (\m i -> doTurn (i, m)) monkeys [0..(length monkeys - 1)]

-- Performs a turn for the monkey at a particular index by recursively throwing each item.
doTurn :: (Int, [Monkey]) -> [Monkey]
doTurn (i, monkeys) =
    let monkey = monkeys !! i
        xs = items monkey
        in if null xs then
            monkeys
        else 
            let worry = (performOp (last xs) (op monkey)) `div` 3
                result = worry `mod` (test monkey)
                dest = (if result == 0 then fst else snd) $ dests monkey
                -- This (slightly awkward) mapping function is used to "modify" the array of monkeys by throwing the item.
                modify = \x ->
                    if num x == i then
                        x { items = init (items x), inspects = inspects x + 1 }
                    else if num x == dest then
                        x { items = worry:(items x) }
                    else x
                in doTurn (i, map modify monkeys)

-- Performs a round for the given array of monkeys (part 2).
doRound2 :: [Int] -> [Monkey] -> [Monkey]
doRound2 mods monkeys = foldl (\m i -> doTurn2 mods (i, m)) monkeys [0..(length monkeys - 1)]

-- Similar to doTurn, but without dividing the worry by 3 and calculating mod_items instead of items.
-- For part 2, the items attribute is left unused.
doTurn2 :: [Int] -> (Int, [Monkey]) -> [Monkey]
doTurn2 mods (i, monkeys) =
    let monkey = monkeys !! i
        xs = mod_items monkey
        in if null xs then
            monkeys
        else 
            let item = last xs
                -- Calculates what the new "mod_items" array should look like.
                new_mods = map (\(i, x) -> (performOp x (op monkey)) `mod` (mods !! i)) $ zip [0..] item
                result = new_mods !! (num monkey)
                dest = (if result == 0 then fst else snd) $ dests monkey
                modify = \x ->
                    if num x == i then
                        x { mod_items = init (mod_items x), inspects = inspects x + 1 }
                    else if num x == dest then
                        x { mod_items = new_mods:(mod_items x) }
                    else x
                in doTurn2 mods (i, map modify monkeys)

performOp :: Int -> (Char, Int) -> Int
performOp x (cmd, _val)
    | cmd == '*'    = x * val
    | cmd == '+'    = x + val
    where val = (if _val == 0 then x else _val)
