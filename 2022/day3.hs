import Data.Char
import Data.List

main = interact sol

sol :: String -> String
sol input =
    let contents = lines input
    in show (sum $ map (priority . matching) contents, sum . map priority $ matching2 contents)

matching :: String -> Char
matching str =
    let (l, r) = splitAt ((length str) `div` 2) str
    in head [a | a <- l, b <- r, a == b]

matching2 :: [String] -> String
matching2 [] = ""
matching2 (s1:s2:s3:xs) = (head [a | a <- s1, b <- s2, c <- s3, a == b, b == c]):matching2 xs

priority :: Char -> Int
priority x
    | c <= ord 'Z'  = (c - ord 'A') + 27
    | otherwise     = (c - ord 'a') + 1
    where c = ord x
