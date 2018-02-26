module Main where

import Data.List (concat)
import Data.Char (digitToInt)

type Vector2D = (Int, Int)
data Owner = O | X | Nobody deriving (Eq, Show)
data Pos = Pos Vector2D Owner deriving (Show)
type Board = [Pos]

-- Combines coordinates (ranges 0..x and 0..y; distribution)
(<&*>) x y = concat $ map (\cx -> map (\cy -> (cx, cy)) [0..y]) [0..x]

intSquareRoot :: Int -> Int
intSquareRoot n = try n
  where try i | i*i > n   = try (i - 1)
              | i*i <= n  = i

coordinatesToIndex :: Vector2D -> Board -> Int
coordinatesToIndex (x,y) b = intSquareRoot ((length b) + 1) * x + y

type Turn = Owner

data Game = Game Board Turn

newGame :: Int -> Turn -> Game
newGame size t = Game board t
  where board = map (\s -> Pos s Nobody) (size <&*> size)

moveAllowed :: Vector2D -> Board -> Bool
moveAllowed v@(x,y) vs = isNobody sv
  where searchedIndx                   = coordinatesToIndex v vs
        sv                             = vs !! searchedIndx
        isNobody (Pos _ o)             = o == Nobody

setOwner :: Board -> Vector2D -> Owner -> Board
setOwner b v o = replace b
  where replace (x@(Pos vec _):xs)  | vec == v  = (Pos vec o) : xs
                                    | otherwise = x : replace xs

strCoordToVec :: String -> Vector2D
strCoordToVec s = (x,y)
  where x = digitToInt $ s !! 0
        y = digitToInt $ s !! 1

runGame :: Game -> IO ()
runGame g@(Game b t) = do
    putStrLn $ "Current Move: " ++ show t
    putStrLn "Put in the Coordinates in (x,y) format"
    coords <- getLine

    let vec = strCoordToVec coords

    case moveAllowed vec b of
      True  -> do putStrLn "Set!"
                  runGame $ Game (setOwner b vec t) (getOther t)
      False -> do putStrLn "Not Allowed!"
                  runGame g
  where getOther p  | p == O    = X
                    | otherwise = O
