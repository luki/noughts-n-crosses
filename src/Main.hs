module Main where

data Player = Nobody | X | O deriving (Show, Eq)
data Pos = Pos Int Int deriving (Show)

data Field = Field Pos Player deriving Show
type Grid = [[Field]]

newGrid xn yn =
    map (\y -> map (\x -> Field (Pos x y) Nobody) [0..xn - 1]) [0..yn - 1]

hasSpace :: Field -> Bool
hasSpace (Field _ p) = p == Nobody

moveAllowed :: Grid -> Pos -> Bool
moveAllowed g (Pos x y) = hasSpace $ (g !! y) !! x

-- currentIndex, SearchedIndex, InList, new element
replace :: Int -> Int -> [a] -> a -> [a]
replace _ _ [] _ = []
replace i goal (x:xs) e
    | i == goal = e : xs
    | otherwise = x : replace (i + 1) goal xs e

-- First int = current index
replacePlayer :: Int -> Grid -> Pos -> Player -> Grid
replacePlayer i (x:xs) pos@(Pos px y) p
    | i == y    = replace 0 px x (Field pos p) : xs
    | otherwise = x : replacePlayer (i + 1) xs pos p

-- Only use if allowed
makeMove :: Grid -> Pos -> Player -> Grid
makeMove g pos@(Pos x y) p
    | moveAllowed g pos = replacePlayer 0 g pos p
    | otherwise         = g

type CurrentMove = Player

getSymbolOfField :: Field -> String
getSymbolOfField (Field _ p)
    | p == Nobody = "-"
    | otherwise   =  show p

gridToStr :: Grid -> [String]
gridToStr g = concat $ map (\l -> (map (\f -> getSymbolOfField f) l) ++ newLine) g
  where newLine = ["\n"]

newRound :: Grid -> CurrentMove -> IO ()
newRound g m = do
    putStrLn $ concat $ gridToStr g
    putStrLn $ "Current move: " ++ show m
    putStrLn "What position would you like to take? Format: x,y"
    posStr <- getLine

    putStrLn $ show $ moveAllowed g (Pos 3 2)
    putStrLn "Skidaddle Skidoodle"

    -- case (moveAllowed g (Pos 3 2)) of
    --    False -> newRound g m
    --    True -> putStrLn "Allowed!"

main :: IO ()
main = do
	newRound (newGrid 3 3) X
	putStrLn "It works"
