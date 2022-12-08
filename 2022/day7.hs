import Data.List

-- A directory item can either be a File (leaf) or a Directory (nested structure).
-- It took me embarassingly long to figure out what data type I needed to use.
data DirectoryItem = File { name::String, size::Int } | Directory { name::String, contents::[DirectoryItem] } deriving (Show)

-- First, generate the entire directory structure.
-- Then, list all the directory sizes.
-- Finally, do whatever summing/searching operation needs to be done.
-- It seems that this strategy is fast enough since the directory structure isn't huge.
main = interact sol

sol :: String -> String
sol input =
    let contents = lines input
        root = Directory { name="/", contents = [] }
        path = "/"
        dirSizes = allDirSizes . fst $ parse (root, path) contents
    in show (sum $ filter (<= 100000) dirSizes, head . filter (>= maximum dirSizes - 40000000) $ sort dirSizes)

-- Recursively parses the entire output, maintaining the root directory and the current path.
parse :: (DirectoryItem, String) -> [String] -> (DirectoryItem, String)
parse (root, path) [] = (root, path)
parse (root, path) output =
    let parts = words $ head output
        in case (parts !! 1) of
            "ls" ->
                let (listed, rest) = span (\x -> (x !! 0) /= '$') (tail output)
                in parse (foldl (dirFullInsert path) root listed, path) rest
            "cd" ->
                parse (root, changeDir path (parts !! 2)) (tail output)

-- Directly inserts an output line into the directory item (assumed to be a Directory).
dirInsert :: DirectoryItem -> String -> DirectoryItem
dirInsert root line =
    let [l, r] = words line
        item = case l of
            "dir"       -> Directory { name = r, contents = [] }
            otherwise   -> File { name = r, size = read l :: Int }
        in Directory { name = name root, contents = item:(contents root) }

-- Traverses the directory structure to return the directory item specified by the path.
navigateDir :: DirectoryItem -> String -> DirectoryItem
navigateDir root [] = root
navigateDir root "/" = root
navigateDir root path =
    let (dir, rest) = span (/= '/') (tail path)
        in navigateDir (head . filter (matchDir dir) $ contents root) rest

-- Inserts an output line into the directory structure, given its full path.
dirFullInsert :: String -> DirectoryItem -> String -> DirectoryItem
dirFullInsert "" root line = dirInsert root line
dirFullInsert "/" root line = dirInsert root line
dirFullInsert path root line =
    let (dir, rest) = span (/= '/') (tail path)
        old_contents = filter (not . matchDir dir) (contents root)
        new_contents = dirFullInsert rest (head $ filter (matchDir dir) (contents root)) line
        in Directory { name = name root, contents = new_contents:old_contents }

-- Changes the directory based on the directory name provided.
changeDir :: String -> String -> String
changeDir path dir
    | dir == "/"    = "/"
    | dir == ".."   = parentPath path
    | otherwise     = path ++ (dir ++ "/")

-- Recursively calculates the total size of the directory item.
dirSize :: DirectoryItem -> Int
dirSize root = case root of
    File _ size             -> size
    Directory _ contents    -> sum $ map dirSize contents

-- Returns all nested directory sizes (including itself) in an array.
allDirSizes :: DirectoryItem -> [Int]
allDirSizes root = case root of
    File _ size             -> [-1]
    Directory _ contents    -> (dirSize root):(filter (>= 0) . concat $ map allDirSizes contents)

-- Returns true if the given directory item is a Directory and its name matches.
matchDir :: String -> DirectoryItem -> Bool
matchDir dirName item = case item of
    File _ _ -> False
    Directory name _ -> (name == dirName)

-- Returns the path to the parent of the given path.
parentPath :: String -> String
parentPath = reverse . dropWhile (/= '/') . reverse . init
