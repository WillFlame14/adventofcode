main = interact sol

sol :: String -> String
sol input =
    let contents = lines input
    in show (sum $ map (rps . words) contents, sum $ map (rps2 . words) contents)

rps :: [String] -> Int
rps (u:[v])
    | u == "A" =
        if v == "X" then 3 + 1
            else if v == "Y" then 6 + 2
                else 0 + 3
    | u == "B" =
        if v == "X" then 0 + 1
            else if v == "Y" then 3 + 2
                else 6 + 3
    | u == "C" =
        if v == "X" then 6 + 1
            else if v == "Y" then 0 + 2
                else 3 + 3

rps2 :: [String] -> Int
rps2 (u:[v])
    | u == "A" =
        if v == "X" then 0 + 3
            else if v == "Y" then 3 + 1
                else 6 + 2
    | u == "B" =
        if v == "X" then 0 + 1
            else if v == "Y" then 3 + 2
                else 6 + 3
    | u == "C" =
        if v == "X" then 0 + 2
            else if v == "Y" then 3 + 3
                else 6 + 1
