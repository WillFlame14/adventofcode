import Data.List
import Data.Maybe
import Debug.Trace
import Utils

-- Spent a while trying to think of a better solution for part 2 before I decided to screw it.
-- I cut out a net in the same shape and just wrote down all the coordinates, then figured out the transformations.
-- Surprisingly, I only made 1 mistake in the huge list of guards.
main = interact sol

sol :: String -> String
sol input =
    let contents = lines input
        grid = padGrid . init $ init contents
        path = last contents
        start = (0, (head $ findIndices (== '.') (head grid), 0))
        results = map (\part1 -> travel part1 grid path start) [True, False]
        in show (map (\(orient, (x, y)) -> 1000 * (y+1) + 4 * (x+1) + orient) results)

-- Pads a grid so that all of the rows have the same length.
padGrid :: [[Char]] -> [[Char]]
padGrid grid =
    let max_len = maximum $ map length grid
        pad_row = \max_l row -> reverse $ foldr (:) (reverse row) (replicate (max_l - length row) ' ')
        in map (pad_row max_len) grid

-- Returns the final orientation and position after travelling on the grid, given a path and whether this is for part 1 or not.
travel :: Bool -> [[Char]] -> String -> (Int, Point) -> (Int, Point)
travel _ _ "" (orient, loc) = (orient, loc)
travel part1 grid path@(x:rest) (orient, loc)
    | x `elem` "LR" =
        let new_orient = (orient + 4 + (if x == 'R' then 1 else -1)) `mod` 4
            in travel part1 grid rest (new_orient, loc)
    | otherwise     =
        let (distance, rest) = span (not . flip elem "LR") path
            new_pos = goStraight part1 grid (read distance) (orient, loc)
            in travel part1 grid rest new_pos

-- Moves the given distance in the direction given, following the grid according to whether this is for part 1 or not.
goStraight :: Bool -> [[Char]] -> Int -> (Int, Point) -> (Int, Point)
goStraight _ _ 0 (orient, loc) = (orient, loc) 
goStraight part1 grid dist (orient, (x, y)) =
    let next_loc = case orient of
            0 -> (x + 1, y) -- Right
            1 -> (x, y + 1) -- Down
            2 -> (x - 1, y) -- Left
            3 -> (x, y - 1) -- Up
        new_pos = case grid `at'` next_loc of
            '.' -> (orient, next_loc)
            '#' -> (orient, (x, y))
            ' ' -> 
                let (new_orient, wrapped_loc) = (if part1 then wrapMap else wrapCube) grid (orient, next_loc)
                    in case grid `at'` wrapped_loc of
                        '.' -> (new_orient, wrapped_loc)
                        '#' -> (orient, (x, y))
                        ' ' -> trace (show (new_orient, wrapped_loc)) $ error "Still ended up on an invalid tile after wrapping around"
        in goStraight part1 grid (dist - 1) new_pos

-- Modifies Utils.at slightly so that a location out of bounds returns ' ' instead of crashing.
at' :: [[Char]] -> Point -> Char
at' grid (x, y)
    | x < 0 || y < 0            = ' '
    | y >= length grid          = ' '
    | x >= length (grid !! y)   = ' '
    | otherwise                 = grid `at` (x, y)

-- Performs wrapping around on an out-of-bounds point, as if the grid was a 2D map.
wrapMap :: [[Char]] -> (Int, Point) -> (Int, Point)
wrapMap grid (orient, (x, y)) =
    (orient, case orient of
        0 -> (head $ findIndices (/= ' ') (grid !! y), y)
        1 -> (x, head $ findIndices (\row -> (row !! x) /= ' ') grid)
        2 -> (last $ findIndices (/= ' ') (grid !! y), y)
        3 -> (x, last $ findIndices (\row -> (row !! x) /= ' ') grid)
        )

-- Performs wrapping around on an out-of-bounds point, as if the grid was a 3D net.
-- Note: This only works on the specific net given in my input.
wrapCube :: [[Char]] -> (Int, Point) -> (Int, Point)
wrapCube grid (orient, (x, y))
    | y < 0 = 
        if x > 99 then  --2 (up), heading to 6 (up)
            (3, (x `mod` 50, 199))
        else            -- 1 (up), heading to 6 (right)
            (0, (0, 100 + x))
    | x < 0 =
        if y > 149 then -- 6 (left), heading to 1 (down)
            (1, (50 + y `mod` 50, 0))
        else            -- 4 (left), heading to 1 (right)
            (0, (50, 49 - y `mod` 50))
    | x > 149 =         -- 2 (right), heading to 5 (left)
            (2, (99, 100 + 49 - y)) 
    | y > 199 =         -- 6 (down), heading to 2 (down)
            (1, (x + 100, 0))
    | x < 50 && orient == 2 =
        if y < 50 then  -- 1 (left), heading to 4 (right) 
            (0, (0, 100 + 49 - y))
        else if y < 100 then  -- 3 (left), heading to 4 (down)
            (1, (y `mod` 50, 100))
        else    -- normal
            (orient, (x - 1, y))
    | y < 100 && x < 50 =   -- 4 (up), heading to 3 (right)
        (0, (50, 50 + x))
    | x > 99 && orient == 0 =
        if y > 99 then      -- 5 (right), heading to 2 (left)
            (2, (149, 49 - y `mod` 50))
        else if y > 49 then -- 3 (right), heading to 2 (up)
            (3, (100 + y `mod` 50, 49))
        else    -- normal
            (orient, (x + 1, y))
    | x > 99 && y > 49 =    -- 2 (down), heading to 3 (left)
        (2, (99, x - 50))
    | x > 49 && y > 149 =
        if orient == 0 then -- 6 (right), heading to 5 (up)
            (3, (y - 100, 149))
        else                -- 5 (down), heading to 6 (left)
            (2, (49, x + 100))
