import Data.List
import Data.Maybe
import Debug.Trace

main = interact sol

sol :: String -> String
sol input =
    let contents = lines input
        in show (foldr addSnafu "0" contents)

snafu = "=-012"

addSnafu :: String -> String -> String
addSnafu a b = reverse $ addSnafu' (reverse a) (reverse b) 0

addSnafu' :: String -> String -> Int -> String
addSnafu' [] [] carry = if carry /= 0 then [snafu !! (carry + 2)] else ""
addSnafu' (a:as) [] carry =
    let digit_sum = carry + (fromJust $ findIndex (== a) snafu) - 2
        (digit_char, new_carry) = performCarry digit_sum
        in digit_char:addSnafu' as [] new_carry
addSnafu' [] (b:bs) carry =
    let digit_sum = carry + (fromJust $ findIndex (== b) snafu) - 2
        (digit_char, new_carry) = performCarry digit_sum
        in digit_char:addSnafu' [] bs new_carry
addSnafu' (a:as) (b:bs) carry =
    let digit_sum = carry + (sum $ map (\x -> (fromJust $ findIndex (== x) snafu) - 2) [a, b])
        (digit_char, new_carry) = performCarry digit_sum
        in digit_char:addSnafu' as bs new_carry

performCarry :: Int -> (Char, Int)
performCarry digit_sum =
    if digit_sum > 2 then
        (snafu !! (digit_sum - 5 + 2), 1)
    else if digit_sum < -2 then
        (snafu !! (digit_sum + 5 + 2), -1)
    else
        (snafu !! (digit_sum + 2),  0)