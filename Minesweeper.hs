module Main where

import Data.Char
import System.IO
import System.Random

-- Game board:
type GBoard = [[Char]]
-- Mine Board. True = mine, False = not mine:
type MBoard = [[Bool]]

-- Game board ilustration (9x9):
gBoard :: GBoard
gBoard = [['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-'],
          ['-','-','-','-','-','-','-','-','-']]

-- Mine board ilustration (9x9):

mBoard :: MBoard
mBoard = [[True, False, False, False, False, False, False, False, False],
          [False, True, False, False, False, False, False, False, False],
          [False, False, True, False, False, False, False, False, False],
          [False, False, False, True, False, False, False, False, False],
          [False, False, False, False, True , False, False, False, False],
          [False, False, False, False, False, True, False, False, False],
          [False, False, False, False, False, False, True, False, False],
          [False, False, False, False, False, False, False, True, False],
          [False, False, False, False, False, False, False, False, True]]


-- PART 1: BOARDS MANIPULATION (2D ARRAYS)


-- gArr (get array): inputs are a position (p) and a list (array). 
-- Output is the element of given position (p)

gArr :: Int -> [t] -> t
gArr p x
      | p > length x - 1 = error "INVALID POSITION"
      | otherwise        = head(drop p x)

-- uArr (update array): inputs are a position (p), a new value (v), and a list (array).
-- Output is a new array updated at given position (p) with the new value (v) 

uArr :: Int -> a -> [a] -> [a]
uArr p v x
      | p > length x - 1 = error "INVALID POSITION"
      | otherwise        = take p x ++ [v] ++ drop (p + 1) x

-- gPos (get position): inputs are a line (l), a column (c) and a board.
-- Output is the element of given position board[l,c] 

gPos :: Int -> Int -> [[a]] -> a
gPos l c board = gArr c (gArr l board) 

-- uPos (update position): inputs are a board position (line and column), a new value (v) and a board.
-- Output is a new board at given position (l,c) with the new value (v)

uPos :: Int -> Int ->  a -> [[a]] -> [[a]]
uPos l c v board
      | (l < length board) && (c < length board) = take l board ++ [uArr c v (gArr l board)] ++ drop (l + 1) board
      | otherwise                                = error "INVALID POSITION"


-- PART 2: LOGIC BEHIND THE GAME


-- isMine: inputs are a line, a column and a Mine Board.
-- Output is a boolean. True = there's a mine. False = there isn't a mine

isMine :: Int -> Int -> MBoard -> Bool
isMine l c board
      | gPos l c board == True = True
      | otherwise              = False

-- isValidPos: inputs are the size of the board (e.g, if it's a 9x9 board, it's size is 9), a line and a column. 
-- Output is a boolean. True = valid position. False = invalid postition

isValidPos :: Int -> Int -> Int -> Bool
isValidPos size l c
      | (l >= 0 && c>= 0) && (size > l && size > c) == True = True
      | otherwise                                           = False

-- validMoves: inputs are the size of the board and a position (line and column).
-- Output is a list (array) containing all adjacent postions of given position (line and column)

validMoves :: Int -> Int -> Int -> [(Int,Int)]
validMoves size l c 
    | isValidPos size l c == True = listFilter size l c (adjacentPositions size l c)
    | otherwise                   = error "INVALID POSITION"
        where
            listFilter :: Int -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
            listFilter size l c ((x,y):xs)
                  | isValidPos size x y == False = listFilter size l c xs
                  | otherwise                    = (x,y) : listFilter size l c xs
            listFilter _ _ _ [] = []
            adjacentPositions :: Int -> Int -> Int -> [(Int,Int)]
            adjacentPositions size l c = [(l-1,c-1),(l-1,c),(l-1,c+1),(l,c-1),(l,c+1),(l+1,c-1),(l+1,c),(l+1,c+1)]

-- cMinas: inputs are a position (line and column) and a Mine Board
-- Output is the number of existing mines adjacent to the given position

cMinas :: Int -> Int -> MBoard -> Int
cMinas l c board = counter board (validMoves (length board) l c)
      where
            counter :: MBoard -> [(Int,Int)] -> Int
            counter board [] = 0
            counter board ((x,y):xs)
                  | isMine x y board == True = 1 + counter board xs
                  | otherwise                = counter board xs

-- unlockPlay: main function of the game!!
-- Inputs are a position (line and column), a Mine Board and a Game Board
--  Output is a updated Game Board

unlockPlay :: Int -> Int -> MBoard -> GBoard -> GBoard
unlockPlay l c mineBoard gameBoard
      | isMine l c mineBoard == True                                   = gameBoard
      | gPos l c gameBoard   /= '-'                                    = gameBoard
      | (isMine l c mineBoard == False) && (cMinas l c mineBoard /= 0) = uPos l c (intToDigit (cMinas l c mineBoard)) gameBoard
      | (isMine l c mineBoard == False) && (cMinas l c mineBoard == 0) = unlockPlayLoop (validMoves (length mineBoard) l c) mineBoard (uPos l c (intToDigit (cMinas l c mineBoard)) gameBoard)
            where
                  unlockPlayLoop :: [(Int,Int)] -> MBoard -> GBoard -> GBoard
                  unlockPlayLoop [] mineBoard gameBoard = gameBoard
                  unlockPlayLoop ((x,y):xs) mineBoard gameBoard
                        | isMine x y mineBoard == True  = gameBoard
                        | otherwise                     = unlockPlayLoop xs mineBoard (uPos x y (intToDigit (cMinas x y mineBoard)) gameBoard)

-- openBoard: inputs are a Mine Board and a Game Board.
-- Output is a Game Board that shows all mine positions.
-- It's used in case of victory or defeat!!!

openBoard :: MBoard -> GBoard -> GBoard
openBoard mineBoard gameBoard = verifyPosition (createPosition (length gameBoard) (length gameBoard)) mineBoard gameBoard
      where
            verifyPosition :: [(Int,Int)] -> MBoard -> GBoard -> GBoard
            verifyPosition [] mineBoard gameBoard = gameBoard
            verifyPosition ((x,y):xs) mineBoard gameBoard
                  | isMine x y mineBoard == True = verifyPosition xs mineBoard (uPos x y '*' gameBoard)
                  | otherwise                    = verifyPosition xs mineBoard (uPos x y (intToDigit (cMinas x y mineBoard)) gameBoard)
            createPosition :: Int -> Int -> [(Int,Int)]
            createPosition size 0 = []
            createPosition size x = createPosition2 size (createList size (x - 1)) ++ createPosition size (x-1)
            createPosition2 :: Int -> [Int] -> [(Int,Int)]
            createPosition2 size x =  zip x (take size (iterate (\x -> x+1) 0))

-- lockedPlay: input is a Game Board.
-- Output is the number of locked plays (e.g, '-')

lockedPlay :: GBoard -> Int
lockedPlay []     = 0
lockedPlay (x:xs) = counterOflockedPlays x + lockedPlay xs
      where
            counterOflockedPlays :: [Char] -> Int
            counterOflockedPlays [] = 0
            counterOflockedPlays (x:xs)
                  | x == '-'  = 1 + counterOflockedPlays xs
                  | otherwise = counterOflockedPlays xs

-- mineCounter: input is a Mine Board.
-- Output is the number of existing mines in the board

mineCounter :: MBoard -> Int
mineCounter []     = 0
mineCounter (x:xs) = mineCounter2 x + mineCounter xs
      where
            mineCounter2 :: [Bool] -> Int
            mineCounter2 [] = 0
            mineCounter2 (x:xs)
                  | x == True = 1 + mineCounter2 xs
                  | otherwise = mineCounter2 xs

-- endGame: inputs are a Mine Board and a Game Board.
-- Output is a boolean. True = game is over. False = game is not over.
-- The game is over when the number of locked plays is equal to the number mines

endGame :: MBoard -> GBoard -> Bool
endGame mineBoard gameBoard
      | mineCounter mineBoard == lockedPlay gameBoard = True
      | otherwise                                     = False


-- PART 3: PRINTING AND CREATING BOARDS


printBoard :: GBoard -> String
printBoard board = "\n" ++ " " ++ createColumns (length board) ++ "\n" ++ createLines (length board) board ++ "\n"
      where
            createColumns :: Int -> String
            createColumns 1    = columns 1
            createColumns size = createColumns (size - 1) ++ columns size 
            createLines :: Int -> GBoard -> String
            createLines 1 board    = lines 1 board
            createLines size board = createLines (size - 1) board ++ lines size board
            columns :: Int -> String
            columns size 
                  | size <= 10                   = "  " ++ show (size - 1)
                  | (size > 10) && (size <= 100) = " " ++ show (size - 1)
            lines :: Int -> GBoard -> String
            lines size board 
                  | size <= 10                   = " " ++ show (size - 1) ++ " " ++ (addSpace (gArr (size - 1) board)) ++ " " ++ "\n"
                  | (size > 10) && (size <= 100) = show (size - 1) ++ " " ++ (addSpace (gArr (size - 1) board)) ++ " " ++ "\n"
            addSpace :: String -> String
            addSpace [] = []
            addSpace (x:xs)  = x : ' ' : ' ' : addSpace xs

-- createList: inputs are a intenger (n) and a new value (v)
-- Output is a list containing (v) n times!

createList :: Int -> a -> [a]
createList n v = replicate n v 

createNewBoard :: Int -> GBoard
createNewBoard 0   = [] 
createNewBoard size = createList size (createList size '-')

createNomineBoardd :: Int -> MBoard
createNomineBoardd 0   = []
createNomineBoardd size = createList size (createList size False)

-- If you'd like to play, type "main"!

main :: IO ()
main = do
   putStr "Size of the board: "
   size <- getLine
   mb <- genMinesBoard (read size)
   gameLoop mb (createNewBoard (read size)) 

gameLoop :: MBoard -> GBoard -> IO ()
gameLoop mb gb = do
   putStr (printBoard gb)
   putStr "Type a line: "
   linha <- getLine
   putStr "Type a line: "
   coluna <- getLine
   if (isMine (read linha) (read coluna) mb)
      then do
            putStr "YOU LOST!\n"
            putStr $ printBoard $ openBoard mb gb
            putStr "GIVE IT ANOTHER TRY!\n"
      else do
            let newGB = (unlockPlay (read linha) (read coluna) mb gb)
            if (endGame mb newGB)
                 then do
                     putStr "YOU WON!!!!!!!!\n"
                     putStr $ printBoard $ openBoard mb newGB
                     putStr "CONGRATULATIONS!!!!!!!!!!!\n"
                 else
                     gameLoop mb newGB

----- DO NOT GO BEYOUND THIS POINT   

genMinesBoard :: Int -> IO MBoard
genMinesBoard size = do
        board <- addMines (round   ((fromIntegral (size *size)) * 0.15)) size (createNomineBoardd size) 
        return board

addMines :: Int -> Int -> MBoard -> IO MBoard
addMines 0 size b = return b
addMines n size b = do
                l <- randomRIO (0,(size-1))
                c <- randomRIO (0,(size-1))
                case isMine l c b of
                      True -> addMines n size b
                      False -> addMines (n-1) size (uPos l c True b)