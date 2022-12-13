import Data.List

-- HOOH parsing took forever. After that it was pretty smooth sailing.
data ListItem = List { items::[ListItem] } | Item { value::Int } deriving Show
instance Eq ListItem where
    x == y = compareL x y == EQ
instance Ord ListItem where
    compare = compareL

main = interact sol

sol :: String -> String
sol input =
    let contents = lines input
        chunks = wordsWhen (/= "") contents
        pairs = map (map parse) chunks
        sorted = sortBy compareL $ concat pairs
        index1 = (length $ takeWhile (< parse "[[2]]") sorted) + 1
        index2 = (length $ takeWhile (< parse "[[6]]") sorted) + 2  -- includes "[[2]]" inserted earlier
        in show (sum . map (+1) $ findIndices (\[x, y] -> x < y) pairs, index1 * index2)

insertItem :: [ListItem] -> ListItem -> (Int, [ListItem])
insertItem xs n =
    let (lt, gt) = span (< n) xs
        in (length lt + 1, lt ++ [n] ++ gt) 

wordsWhen :: (a -> Bool) -> [a] -> [[a]]
wordsWhen _ [] = []
wordsWhen func xs =
    let (a, b) = span func xs
    in a:(wordsWhen func $ if null b then [] else tail b)

parse :: String -> ListItem
parse s = fst $ parse' s []

-- Parses one "item" off of the string, given an accumulation of items in the current list.
-- Returns a pair of (entire parsed section of the list so far, remaining string to parse).
parse' :: String -> [ListItem] -> (ListItem, String)
parse' [] acc = (head acc, "")
parse' n@(x:xs) acc =
    if x == '[' then
        -- We need to parse everything inside with a fresh accumulator (will stop when ']' is hit).
        -- Then append it to acc and continue parsing from there.
        let (contents, rest) = parse' xs []
            in parse' rest $ acc ++ [contents]
    else if x == ']' then
        -- Return a list with all items in acc (i.e. stop parsing and accumulating), as well as the remaining string.
        (List { items = acc }, xs)
    else if x == ',' then
        -- Ignore the comma (items so far are stored in acc).
        parse' xs acc
    else
        -- Read a number and push it onto acc, then keep reading.
        let (part, rest) = span (\c -> not $ c `elem` ",]") n
            item = Item $ read part
            in parse' rest (acc ++ [item])

compareL :: ListItem -> ListItem -> Ordering
compareL x y =
    case x of
        List xs ->
            case y of
                List ys ->
                    -- Zip the arrays together and find the first non EQ.
                    -- If all are EQ, then compare their lengths.
                    let compared = find (\(x', y') -> x' /= y') $ zip xs ys
                        in case compared of
                            Just (x', y')   -> compare x' y'
                            Nothing         -> compare (length xs) (length ys)
                Item yv -> compareL x (List [y])
        Item xv ->
            case y of
                List ys -> compareL (List [x]) y
                Item yv -> compare xv yv
