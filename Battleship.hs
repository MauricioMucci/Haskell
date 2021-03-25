{-    MAURICIO CARVALHO MUCCI - 19101675

            COMO O JOGO FUNCIONA:
> DIGITE MAIN NO TERMINAL PARA JOGAR <
> AMBOS OS JOGADORES IRAO POSICIONAR SEUS BARCOS <
> O JOGADOR IRA REALIZAR SEUS ATAQUES COM BASE NO
  NUMERO DE NAVIOS QUE LHE RESTA <
> O JOGO TERMINA QUANDO O NUMERO DE NAVIOS DE UM
  JOGADOR E IGUAL A ZERO <
> LEGENDA:
--> 'H' = acerto (hit)
--> 'S' = navio (ship)
--> '~' = agua (water)

            FUNCOES DO JOGO:
> PARTE 1:
--> gArr: pega um elemento de um vetor, com base
    na sua posicao!
--> uArr: atualiza um elemento do vetor, com base
    na sua posicao!
--> gPos: pega um elemento de uma matriz, com base
    na sua posicao!
--> uPos: atualiza um elemento de uma matriz, com 
    base na sua posicao!

> PARTE 2:
--> isShip: indica se ha um navio ou nao!
--> unlockPlay: com base em uma lista de ataques
    (linhas e colunas), atualiza a Game Board!
--> openBoard: atualiza a Game Board em caso de
    vitoria de um jogador!
--> shipCounter: conta o numero de navios restantes!
--> sinkShip: atualiza a ShipBoard, determina se um
    ataque foi certeiro ou nao!
--> endGame: encerra o jogo!

> PARTE 3
--> printBoard: imprime a Game Board!
--> createList: cria uma lista com um valor repetido
    "n" vezes!
--> createNewBoard: criar uma Game Board com todas as
    posicoes fechadas!
--> createNoshipBoard: cria um Ship Board com todas as
    posicoes contendo False

> MAIN
--> main e gameLoop: executam o jogo!
--> readLineAndColumn: le uma linha e uma coluna e
    transforma em uma tupla (par ordenado)!
--> readLCLoop: executa a funcao readLineAndColumn
    "n" vezes!
--> genShipBoard e addShip: gera um ShipBoard com 
    base nas escolhas dos jogadores. Aloca os navios
    nos lugares desejados!
-}

module Main where

import Data.Char
import System.IO
import System.Console.ANSI

-- Game board:
type GBoard = [[Char]]
-- Ship Board. True = ship, False = water:
type SBoard = [[Bool]]


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
-- Output is the element of given position (l,c)

gPos :: Int -> Int -> [[a]] -> a
gPos l c board = gArr c (gArr l board) 

-- uPos (update position): inputs are a board position (line and column), a new value (v) and a board.
-- Output is a new board at given position (l,c) with the new value (v)

uPos :: Int -> Int ->  a -> [[a]] -> [[a]]
uPos l c v board
      | (l < length board) && (c < length board) = take l board ++ [uArr c v (gArr l board)] ++ drop (l + 1) board
      | otherwise                                = error "INVALID POSITION"


-- PART 2: LOGIC BEHIND THE GAME


-- isShip: inputs are a line, a column and a Ship Board.
-- Output is a boolean. True = there's a ship. False = there isn't a ship

isShip :: Int -> Int -> SBoard -> Bool
isShip l c board
    | gPos l c board == True = True
    | otherwise              = False

-- unlockPlay: inputs are a list of lines and columns, a Ship Board and a Game Board.
-- Output is an updated Game Board

unlockPlay :: [(Int,Int)] -> SBoard -> GBoard -> GBoard
unlockPlay [] shipBoard gameBoard = gameBoard
unlockPlay ((x,y):xs) shipBoard gameBoard
      | gPos x y gameBoard /= '*'     = unlockPlay xs shipBoard gameBoard
      | isShip x y shipBoard == False = unlockPlay xs shipBoard (uPos x y '~' gameBoard)
      | otherwise                     = unlockPlay xs shipBoard (uPos x y 'H' gameBoard)

-- openBoard: inputs are a Ship Board and a Game Board.
-- Output is an updated Game Board

openBoard :: SBoard -> GBoard -> GBoard
openBoard shipBoard gameBoard = verifyPosition (generatePosition (length gameBoard) (length gameBoard)) shipBoard gameBoard
      where
            verifyPosition :: [(Int,Int)] -> SBoard -> GBoard -> GBoard
            verifyPosition [] shipBoard gameBoard = gameBoard
            verifyPosition ((x,y):xs) shipBoard gameBoard
                  | gPos x y gameBoard == 'H' = verifyPosition xs shipBoard gameBoard
                  | isShip x y shipBoard      = verifyPosition xs shipBoard (uPos x y 'S' gameBoard)
                  | otherwise                 = verifyPosition xs shipBoard (uPos x y '~' gameBoard)
            generatePosition :: Int -> Int -> [(Int,Int)]
            generatePosition size 0 = []
            generatePosition size x = generatePosition2 size (createList size (x - 1)) ++ generatePosition size (x-1)
            generatePosition2 :: Int -> [Int] -> [(Int,Int)]
            generatePosition2 size x =  zip x (take size (iterate (\x -> x+1) 0))

-- shipCounter: input is a Ship Board.
-- Output is the number of existing ships in the board

shipCounter :: SBoard -> Int
shipCounter [] = 0
shipCounter (x:xs) = shipCounter2 x + shipCounter xs
      where 
            shipCounter2 :: [Bool] -> Int
            shipCounter2 [] = 0
            shipCounter2 (x:xs)
                  | x == True = 1 + shipCounter2 xs
                  | otherwise = shipCounter2 xs

-- sinkShip: inputs are a Ship Board and a Game Board.
-- Output is an updated Ship Board

sinkShip :: SBoard -> GBoard -> SBoard
sinkShip shipBoard gameBoard = verifyPosition (generatePosition (length shipBoard) (length shipBoard)) shipBoard gameBoard
      where
            verifyPosition :: [(Int,Int)] -> SBoard -> GBoard -> SBoard
            verifyPosition [] shipBoard gameBoard = shipBoard
            verifyPosition ((x,y):xs) shipBoard gameBoard
                  | gPos x y gameBoard == 'H' = verifyPosition xs (uPos x y False shipBoard) gameBoard
                  | otherwise                 = verifyPosition xs shipBoard gameBoard
            generatePosition :: Int -> Int -> [(Int,Int)]
            generatePosition size 0 = []
            generatePosition size x = generatePosition2 size (createList size (x - 1)) ++ generatePosition size (x-1)
            generatePosition2 :: Int -> [Int] -> [(Int,Int)]
            generatePosition2 size x =  zip x (take size (iterate (\x -> x+1) 0)) 

-- endGame: input is a Ship Board.
-- Output is a boolean. True = game is over. False = game is not over.
-- The game is over when one's number of ship is equal to zero

endGame :: SBoard -> Bool
endGame board
      | shipCounter board == 0 = True
      | otherwise              = False


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
            columns size = "  " ++ show (size - 1)
            lines :: Int -> GBoard -> String
            lines size board = " " ++ show (size - 1) ++ " " ++ (addSpace (gArr (size - 1) board)) ++ " " ++ "\n"
            addSpace :: String -> String
            addSpace [] = []
            addSpace (x:xs)  = x : ' ' : ' ' : addSpace xs

-- createList: inputs are an intenger (n) and a new value (v)
-- Output is a list containing (v) n times!

createList :: Int -> a -> [a]
createList n v = replicate n v 

createNewBoard :: Int -> GBoard
createNewBoard 0    = [] 
createNewBoard size = createList size (createList size '*')

createNoshipBoardd :: Int -> SBoard
createNoshipBoardd 0    = []
createNoshipBoardd size = createList size (createList size False)


-- MAIN


main :: IO ()
main = do
      clearScreen
      putStr "\t\tThe size of the board is 10x10 and you will begin with 5 ships!\n"
      putStr "Player 1, would you kindly place your ships?\n"
      p1 <- readLCLoop 5
      sb1 <- genShipBoard p1 10
      clearScreen
      putStr "Player 2, would you kindly place your ships?\n"
      p2 <- readLCLoop 5
      sb2 <- genShipBoard p2 10
      clearScreen
      gameLoop sb1 sb2 (createNewBoard 10) (createNewBoard 10)


gameLoop :: SBoard -> SBoard -> GBoard -> GBoard -> IO ()
gameLoop sb1 sb2 gb1 gb2 = do
                  clearScreen
                  putStr "\tPlayer 1's turn\n"
                  putStr (printBoard gb1)
                  putStr ("You have " ++ show (shipCounter sb1) ++ " attacks\n")
                  attacks <- readLCLoop (shipCounter sb1)
                  clearScreen
                  let newGB2 = (unlockPlay attacks sb2 gb2)
                  let newSB2 = (sinkShip sb2 newGB2)
                  if (endGame newSB2)
                        then do
                              putStr "\tPLAYER 1 WON!!!"
                              putStr (printBoard (openBoard sb1 gb1))
                              putStr "\tPlayer 2's board:"
                              putStr (printBoard (openBoard newSB2 newGB2))
                        else do
                              putStr "\tPlayer 2's turn\n"
                              putStr (printBoard newGB2)
                              putStr ("You have " ++ show (shipCounter newSB2) ++ " attacks\n")
                              attacks <- readLCLoop (shipCounter newSB2)
                              clearScreen
                              let newGB1 = (unlockPlay attacks sb1 gb1)
                              let newSB1 = (sinkShip sb1 newGB1)
                              if (endGame newSB1)
                                    then do
                                          putStr "\tPLAYER 2 WON!!!"
                                          putStr (printBoard (openBoard newSB2 newGB2))
                                          putStr "\tPlayer 1's board:"
                                          putStr (printBoard (openBoard newSB1 newGB1))
                                    else do
                                          gameLoop newSB1 newSB2 newGB1 newGB2

readLineAndColumn :: IO (Int,Int)
readLineAndColumn = do
                  putStr "Type a line: "
                  l <- getLine
                  putStr "Type a column: "
                  c <- getLine
                  return ((read l),(read c))
                  
readLCLoop :: Int -> IO [(Int,Int)]
readLCLoop 0 = return []
readLCLoop n = do
            x <- readLineAndColumn
            lx <- readLCLoop (n - 1)
            return (x:lx)

genShipBoard :: [(Int,Int)] -> Int -> IO SBoard
genShipBoard list size = do
                        board <- addShip list (createNoshipBoardd size)
                        return board

addShip :: [(Int,Int)] -> SBoard -> IO SBoard
addShip [] shipBoard = return shipBoard
addShip ((x,y):xs) shipBoard = do
                              case isShip x y shipBoard of
                                    True -> addShip xs shipBoard
                                    False -> addShip xs (uPos x y True shipBoard)