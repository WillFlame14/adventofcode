import Data.List

main = interact sol2

sol :: String -> String
sol input =
    let line = head $ lines input
    in show (map (\len -> head $ filter (\i -> uniq $ slice (i-len) len line) [len..]) [4, 14])

slice :: Int -> Int -> [a] -> [a]
slice start amount xs = take amount $ drop start xs

uniq :: (Eq a) => [a] -> Bool
uniq xs = length xs == length (nub xs)