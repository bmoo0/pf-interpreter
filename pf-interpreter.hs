{-
 n north (up)
 s south (down)
 e east ( right)
 w west ( left)
 * flip current cell
 [ if cell is 0 jump past matching ]
 ] jump to matching [
-}

type Matrix = [[Int]]

initialiseGrid :: Int -> Int -> Matrix
initialiseGrid 0 w = [initialiseRow w]
initialiseGrid h w = [initialiseRow w] ++ (initialiseGrid (h-1) w)

initialiseRow :: Int -> [Int]
initialiseRow l = take l (repeat 0)

getGridLayer :: [[a]] -> Int -> [a]
getGridLayer g l = g !! l

getIndex :: Int -> Int -> [[a]] -> a
getIndex row col mat = ((mat !! row) !! col)

getValAtPos :: (Int, Int) -> Matrix -> Int
getValAtPos (r,c) mat = getIndex r c mat

editList :: Int -> a -> [a] -> [a]
editList _ _ [] = []
editList i val (x:xs)
  | i == 0 = val:xs
  | otherwise = x:editList (i-1) val xs

editMatrix :: Int -> Int -> a -> [[a]] -> [[a]]
editMatrix row col val mat =
  editList row (editList col val (getGridLayer mat row)) mat

north :: (Int,Int) -> (Int,Int)
north (x,s) = (x+1,s)

south :: (Int,Int) -> (Int,Int)
south (x,s) = (x-1,s)

east :: (Int,Int) -> (Int,Int)
east (x,s) = (x,s+1)

west :: (Int,Int) -> (Int,Int)
west (x,s) = (x,s-1)

pfFlip :: Int -> Int -> Matrix -> Matrix
pfFlip row col mat
  | getIndex row col mat == 1 = editMatrix row col 0 mat
  | otherwise = editMatrix row col 1 mat

flipAtPos :: (Int, Int) -> Matrix -> Matrix
flipAtPos (r,c) g = pfFlip r c g

jmpForward :: String-> String -> (String, String)
jmpForward _ [] = ([],[])
jmpForward prev (x:xs)
  | x == ']' = (prev, xs)
  | otherwise = jmpForward (prev++[x]) xs

jumpForward :: String -> String -> (String, String)
jumpForward prev s = jmpForward  prev s

jmpBackward :: String -> String -> (String, String)
jmpBackward _ [] = ([],[])
jmpBackward prev (x:xs)
  | x == '[' = ((reverse prev), (reverse xs))
  | otherwise = jmpBackward (prev++[x]) xs

jumpBackward :: String -> String -> (String, String)
jumpBackward prev s = jmpBackward (reverse prev) (reverse s)

interpret :: String -> Matrix -> (Int, Int) -> String -> Matrix
interpret _ grid _ [] = grid
interpret prev grid pos (x:xs)
  | x == 'n' = interpret (prev++[x]) grid (north pos) xs
  | x == 's' = interpret (prev++[x]) grid (south pos) xs
  | x == 'e' = interpret (prev++[x]) grid (east pos) xs
  | x == 'w' = interpret (prev++[x]) grid (west pos) xs
  | x == '*' = interpret (prev++[x]) (flipAtPos pos grid) pos xs
  | x == '[' =
    if (getValAtPos pos grid) == 0 then
      interpret (prev++[x]) grid pos xs
    else
      interpret (fst $ jumpForward prev xs) grid pos (snd $ jumpForward prev xs)
  | x == ']' = interpret (fst $ jumpBackward prev xs) grid pos (snd $ jumpBackward prev xs)
  | otherwise = grid


main :: IO ()
main = do
  -- initialise a 36 by 36 grid
  let g = initialiseGrid 36 36
  let origin = (0,0)
  line <- getLine
  if null line
    then return ()
    else do
    mapM_ (putStrLn . show) (interpret [] g origin line)
