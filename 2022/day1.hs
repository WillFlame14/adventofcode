import Data.List

main = interact sol

sol :: String -> String
sol input =
    let contents = lines input
        grouped = filter (/= [""]) $ groupBy (\x y -> x /= "" && y /= "") contents
        calories = map (sum . map read) grouped
    in show (maximum calories, sum . take 3 . reverse $ sort calories)
