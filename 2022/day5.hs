main = interact sol

sol :: String -> String
sol input =
    let contents = lines input
        stacks = readStacks (reverse $ take 8 contents) [[], [], [], [], [], [], [], [], []]
        commands = drop 10 contents
        final_stacks = map (\ver -> foldl (readCommand ver) stacks commands) [1, 2]
        in show (map (map head) final_stacks)

-- Splits an array into chunks of the given size (the last array may be slightly smaller).
chunked :: [a] -> Int -> [[a]]
chunked [] size = []
chunked xs size =
    let (group, rest) = splitAt size xs
    in group:chunked rest size

-- Reads the input into a set of stacks.
-- Note that the input lines need to be reversed so the cons operator can be used.
readStacks :: [String] -> [[Char]] -> [[Char]]
readStacks [] stacks = stacks
readStacks (x:xs) stacks =
    let chunks = chunked x 4
        in readStacks xs $ zipWith (\x y -> if (y !! 1 == ' ') then x else (y !! 1):x) stacks chunks

-- Given a version (affects which move algorithm is used), applies the given command to the stacks.
readCommand :: Int -> [[Char]] -> String -> [[Char]]
readCommand ver stacks cmd =
    let parts = words cmd
        amount = read (parts !! 1)
        origin = read (parts !! 3) - 1
        dest = read (parts !! 5) - 1
        (new_origin, new_dest) = (if ver == 1 then move else move2) amount (stacks !! origin, stacks !! dest)
        in [if i == origin then new_origin else if i == dest then new_dest else x | (i, x) <- zip [0..] stacks]

-- Moves N items from the origin stack to the destination stack, one at a time.
move :: Int -> ([Char], [Char]) -> ([Char], [Char])
move 0 stacks = stacks
move amount (origin, dest) = (iterate (moveOne) (origin, dest)) !! amount

-- Moves 1 item from the origin stack to the destination stack.
moveOne :: ([Char], [Char]) -> ([Char], [Char])
moveOne (origin, dest) =
    let top = head origin
    in (tail origin, top:dest)

-- Moves N items from the origin stack to the destination stack, all together.
move2 :: Int -> ([Char], [Char]) -> ([Char], [Char])
move2 amount (origin, dest) = (drop amount origin, moved ++ dest)
    where moved = take amount origin
