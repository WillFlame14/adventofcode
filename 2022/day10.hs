-- Off by 1 errors my beloathed
main = interact sol

sol :: String -> String
sol input =
    let contents = lines input
        start = (1, 1, False)
        regs = genReg start contents
        in show (execute start contents, map drawCrtLine $ chunked regs 40)

-- Splits an array into chunks of the given size (the last array may be slightly smaller).
chunked :: [a] -> Int -> [[a]]
chunked [] size = []
chunked xs size =
    let (group, rest) = splitAt size xs
    in group:chunked rest size

-- This was a lot more complicated than I thought it should be.
execute :: (Int, Int, Bool) -> [String] -> Int
execute _ [] = 0
execute (cycles, reg, inter) (x:commands) =
    let signal = if cycles `mod` 40 == 20 then cycles * reg else 0
        in signal + case x of
            "noop"      -> execute (cycles + 1, reg, False) commands
            otherwise   -> if not inter then
                    execute (cycles + 1, reg, True) (x:commands)
                else
                    let [_, val] = words x
                        in execute (cycles + 1, reg + (read val), False) commands

genReg :: (Int, Int, Bool) -> [String] -> [Int]
genReg _ [] = []
genReg (cycles, reg, inter) (x:commands) = reg:case x of
    "noop"      -> genReg (cycles + 1, reg, False) commands
    otherwise   -> if not inter then
            genReg (cycles + 1, reg, True) (x:commands)
        else
            let [_, val] = words x
                in genReg (cycles + 1, reg + (read val), False) commands

drawCrtLine :: [Int] -> String
drawCrtLine xs =
    let parts = zip [0..] xs
        in [ if abs (a - b) <= 1 then '#' else '.' | (a, b) <- parts]
