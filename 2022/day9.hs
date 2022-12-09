import qualified Data.Set as Set

type Point = (Int, Int)

-- I really liked how I was able to extend part 1 into part 2 easily. It wasn't this neat at first, but it was close.
main = interact sol

sol :: String -> String
sol input =
    let contents = lines input
        commands = map parse contents
        results = map (sim_tail commands) [1, 9]
        in show (map Set.size results)

parse :: String -> (Char, Int)
parse x =
    let [dir, len] = words x
        in (dir !! 0, read len)

-- Given a list of commands and a tail length, returns a set of the unique points that the tail tip visits.
sim_tail :: [(Char, Int)] -> Int -> Set.Set Point
sim_tail commands tail_len  =
    let h = (0, 0)
        tails = replicate tail_len (0, 0)
        (_, _, ftails) = foldl readLine (h, tails, []) commands
        in Set.fromList ftails

-- Given a tuple of (head, tails, previously visited points by the tail tip) and one command,
-- returns a modified version of the tuple after the command has been executed.
readLine :: (Point, [Point], [Point]) -> (Char, Int) -> (Point, [Point], [Point])
readLine (h, ts, tails) (_, 0) = (h, ts, (last ts):tails)
readLine (h, ts, tails) (dir, len) =
    let new_h = moveHead h dir
        new_ts = moveTails ts new_h
        (fh, fts, ftails) = readLine (new_h, new_ts, tails) (dir, len - 1)
        in (fh, fts, (last ts):ftails)

-- Moves a "head" point in the direction given.
moveHead :: Point -> Char -> Point
moveHead (x, y) dir
    | dir == 'L'  = (x - 1, y)
    | dir == 'U'  = (x, y + 1)
    | dir == 'R'  = (x + 1, y)
    | dir == 'D'  = (x, y - 1)

-- Moves a "tail" point relative to the given head.
moveTail :: Point -> Point -> Point
moveTail (tx, ty) (hx, hy) =
    let dx = hx - tx
        dy = hy - ty
        in if max (abs dx) (abs dy) <= 1 then
            (tx, ty)
        else
            let mx = if dx == 0 then 0 else dx `div` abs dx
                my = if dy == 0 then 0 else dy `div` abs dy
                in (tx + mx, ty + my)

-- Moves a set of tails, given a starting head.
moveTails :: [Point] -> Point -> [Point]
moveTails [] _ = []
moveTails (t:ts) h = (moveTail t h):(moveTails ts t)