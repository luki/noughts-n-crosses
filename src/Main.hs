module Main where

import Data.Char

-- | TODO:
--   Check if player has won
--   Avoid crashes (e.g. if Pos is too big)

data Owner = Nobody | X | O deriving (Show, Eq)

other :: Owner -> Owner
other o = case o of
    X         -> O
    O         -> X
    otherwise -> Nobody

type Move = Owner
type Board = [Owner]

-- Create a board with size (will be x * x)
newBoard :: Int -> Board
newBoard s = replicate size Nobody
  where size = s * s

intSquareRoot :: Int -> Int
intSquareRoot n = try n
  where try i | i*i > n   = try (i - 1)
              | i*i <= n  = i

makeMove :: Board -> Move -> Int -> Board
makeMove b o p = swapElementAt b p o
  where swapElementAt (x:xs) n e  | n == 0    = e : xs
                                  | otherwise = x : swapElementAt xs (n-1) e

data Pos = Pos Int Int deriving (Show)

posToField :: Pos -> Int -> Int
posToField (Pos x y) l = y * (intSquareRoot l) + x

readPos :: String -> Pos
readPos str = Pos x y
  where
        x = digitToInt $ str !! 0
        y = digitToInt $ str !! 2

isMoveAllowedAt :: Board -> Pos -> Bool
isMoveAllowedAt b p = b !! position == Nobody
    where position = posToField p $ length b

newRound b m = do
    putStrLn $ "It's " ++ show m ++ " turn!"
    putStrLn "Type in a position to take in the format x,y"

    str <- getLine
    let pos = readPos str

    -- Check if won
    if isMoveAllowedAt b pos
      then do
        newRound (makeMove b m . posToField pos $ length b) (other m)
      else do
        putStrLn $ "WARNING: " ++ str ++ " is taken!"
        newRound b m

main :: IO ()
main = do
  putStrLn "Positions start at x and y = 0!"
  newRound (newBoard 3) X
  putStrLn "It works"
