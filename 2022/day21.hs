import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Debug.Trace
import Utils

data Monkey = Single Int | Expr { left::String, operation::Char, right::String } deriving Show

-- Subtraction and division are evil operations. Other than that, this was pretty easy.
main = interact sol

sol :: String -> String
sol input =
    let contents = lines input
        monkeys = Map.fromList $ map parse contents
        root = fromJust $ Map.lookup "root" monkeys
        (dependent, notD) = orderDependent monkeys (left root, right root) "humn"
        start = compute monkeys notD
        in show (compute monkeys "root", computeReverse monkeys dependent "humn" start)

parse :: String -> (String, Monkey)
parse xs =
    let parts = words xs
        name = init $ head parts
        in (name, case length parts of
            2 -> Single (read $ last parts)
            4 -> Expr (parts !! 1) (head $ parts !! 2) (parts !! 3)
            )

-- Recursively determines the number a monkey will shout.
compute :: Map.Map String Monkey -> String -> Int
compute monkeys m =
    let monkey = fromJust $ Map.lookup m monkeys
        in case monkey of
            Expr m1 op m2   -> performOp op (compute monkeys m1) (compute monkeys m2)
            Single x        -> x

performOp :: Char -> Int -> Int -> Int
performOp op x y
    | op == '+' = x + y
    | op == '-' = x - y
    | op == '*' = x * y
    | op == '/' = x `div` y

-- Given two monkeys' names, orders the one dependent on the dependant first.
-- This probably isn't the correct English usage but whatever, I just needed a variable name.
orderDependent :: Map.Map String Monkey -> (String, String) -> String -> (String, String)
orderDependent monkeys (m1, m2) dependant =
    if all (\x -> dependsOn monkeys x dependant) [m1, m2] then
        trace("AAA") $ (m1, m2)
    else if dependsOn monkeys m1 dependant then
        (m1, m2)
    else 
        (m2, m1)

-- Given a monkey's name, returns whether it depends on the dependant.
dependsOn :: Map.Map String Monkey -> String -> String -> Bool
dependsOn monkeys m dependant
    | m == dependant    = True
    | otherwise         =
        let monkey = fromJust $ Map.lookup m monkeys
            in case monkey of
                Expr m1 op m2   -> any (\x -> dependsOn monkeys x dependant) [m1, m2]
                Single x        -> False

-- Given a monkey's name, recursively attempts to make it shout "other" by modifying the dependant's number.
computeReverse :: Map.Map String Monkey -> String -> String -> Int -> Int
computeReverse monkeys m dependant other
    | m == dependant    = other
    | otherwise         =
        let monkey = fromJust $ Map.lookup m monkeys
            in case monkey of
                Expr m1 op m2   ->
                    let (dependent, notD) = orderDependent monkeys (m1, m2) "humn"
                        notD_val = compute monkeys notD
                        in computeReverse monkeys dependent dependant $
                            -- If the 2nd one is dependent, subtraction and division cause the operation to remain the same while changing the order of operands.
                            -- e.g. 150 = 4 - x ==> x = 4 - 150
                            if m2 == dependent && op `elem` "-/" then
                                performOp op notD_val other
                            else
                                performOp (reverseOp op) other notD_val
                Single x        -> x

reverseOp :: Char -> Char
reverseOp op
    | op == '+' = '-'
    | op == '-' = '+'
    | op == '*' = '/'
    | op == '/' = '*'