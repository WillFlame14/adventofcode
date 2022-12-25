import Data.Maybe
import Data.List
import qualified Data.Set as Set
import Debug.Trace
import Utils

type Elf = Point

-- I spent way too long getting minor things wrong. pain
main = interact sol

sol :: String -> String
sol input =
    let contents = lines input
        elves = concat . map (\(i, row) -> map (\col -> (col, i)) $ findIndices (== '#') row) $ zip [0..] contents
        elf_history = scanl (\acc x -> let dir = x `mod` 4 in moveElves acc dir) elves [0..]
        in show (calcAnswer (elf_history !! 10), 1 + (fromJust $ findIndex (\(i, x) -> x == elf_history !! (i + 1)) $ zip [0..] elf_history))

calcAnswer :: [Elf] -> Int
calcAnswer elves =
    let [xs, ys] = map (\f -> map f elves) [fst, snd]
        in (maximum xs - minimum xs + 1) * (maximum ys - minimum ys + 1) - length elves

moveOptions = [
    [(0, -1), (-1, -1), (1, -1)],
    [(0, 1), (-1, 1), (1, 1)],
    [(-1, 0), (-1, -1), (-1, 1)],
    [(1, 0), (1, 1), (1, -1)]
    ]

all8 = [(0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1)]

proposeMove :: Int -> Set.Set Elf -> Int -> Elf -> Elf
proposeMove tries elves_set dir (x, y)
    | tries == 4    = (x, y)   -- If we've tried 4 times already, we're back to the original elf.
    | otherwise     =
        if all (\p -> Set.notMember p elves_set) $ map (pointAdd (x, y)) all8 then
            (x, y)
        else
            let dir_moves = moveOptions !! dir
                next_dir = (dir + 1) `mod` 4
                in if any (\p -> Set.member p elves_set) $ map (pointAdd (x, y)) dir_moves then
                    -- If any elf is overlapping, propose a new move with the next direction.
                    proposeMove (tries + 1) elves_set next_dir (x, y)
                else
                    -- If no overlaps, return the moved elf.
                    pointAdd (x, y) $ head dir_moves

moveElves :: [Elf] -> Int -> [Elf]
moveElves elves dir =
    let elves_set = Set.fromList elves
        proposed_elves = map (proposeMove 0 elves_set dir) elves
        isUnique = \(i, p) ->
            isNothing $ find (\(i', p') -> p == p' && i /= i') $ zip [0..] proposed_elves
        in --trace (show (elves, proposed_elves)) $
            map (\(i, p) -> if isUnique (i, p) then p else elves !! i) $ zip [0..] proposed_elves
