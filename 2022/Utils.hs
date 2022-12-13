module Utils (
	chunked,
	wordsWhen,
	at
) where

-- Splits an array into chunks of the given size (the last array may be slightly smaller).
chunked :: [a] -> Int -> [[a]]
chunked [] size = []
chunked xs size =
    let (group, rest) = splitAt size xs
    in group:chunked rest size

wordsWhen :: (a -> Bool) -> [a] -> [[a]]
wordsWhen _ [] = []
wordsWhen func xs =
    let (a, b) = span func xs
    in a:(wordsWhen func $ if null b then [] else tail b)

at :: [[a]] -> (Int, Int) -> a
at grid (v, h) = (grid !! v) !! h
