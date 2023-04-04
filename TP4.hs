--BOULLIER Arthur-- 
--    TDA-TP2    --
{-Partie 1-}

-- a --
import Data.Char
import Data.List
import System.IO

data Player = O | X | B
    deriving (Eq, Ord)

instance Show Player where
    show O = "O"
    show X = "X"
    show B = " "

type Grid =(Int , [[Player]])

-- b --
next :: Player -> Player
next O = X
next X = O
next B = B

{-Partie 2-}
-- c --
empty :: Int -> Grid
empty n = (n ,(replicate n (replicate n B)))

-- d --
full :: Grid -> Bool
full (_ , xs) = all (/= B)( concat xs )

-- e --
diag :: Grid -> [Player]
diag (n , xs) = [xs !! i !! i| i <- [0..(n-1)]]

-- f --
wins :: Player -> Grid -> Bool
wins p (n , xs) = any (all (== p)) (xs ++ transpose xs ++ [diag (n , xs)] ++ [diag (n , reverse xs)])

-- g --
won :: Grid -> Bool
won (n , xs) = wins O (n , xs) || wins X (n , xs)

{-Partie 3-}
-- h --
insVert :: [String] -> [String]
insVert [] = []
insVert [x] = [x]
insVert (x:xs) = x : "|" : insVert xs

-- i --
showRow :: [Player] -> [String]
showRow xs = insVert [ " " ++ show x ++ " " | x <- xs]

-- j --
insHoriz :: Int -> [String]
insHoriz n = replicate n "---" ++ replicate (n-1) "-"


-- k --
showGrid :: Grid -> IO ()
showGrid (n , (x:xs)) = do
    putStrLn (concat (showRow x))
    if xs /= [] then do
        putStrLn (concat (insHoriz n))
        showGrid (n , xs)
    else
        return ()

-- l --
{-
*Main> showGrid (4, [[B,O,X,B],[X,O,X,B],[X,X,O,X],[B,B,B,X]])
   | O | X |   
---------------
 X | O | X |   
---------------
 X | X | O | X 
---------------
   |   |   | X 
-}

{-Partie 4-}
-- m --
valid :: Grid -> Int -> Bool
valid (n , xs) x = x >= 0 && x < n^2 && (xs !! (x `div` n) !! (x `mod` n)) == B


-- n --
cut :: Int -> [a] -> [[a]]
cut n [] = []
cut n xs = take n xs : cut n (drop n xs)

-- o --
move :: Grid -> Int -> Player -> [Grid]
move (n , xs) x p | valid (n , xs) x = [(n , cut n (take x (concat xs) ++ [p] ++ drop (x+1) (concat xs)))]
                  | otherwise = []

{-Partie 5-}
-- p --

cls:: IO()
cls = putStr "\ESC[2J"
-- Fonction qui nettoie le terminal

goto :: (Int, Int) -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")
-- Fonction qui place le curseur à la position [x,y] dans le terminal

run :: Grid -> Player -> IO ()
run g p = do cls
             goto (1,1)
             showGrid g
             run' g p
-- Fonction qui affiche la grille et appelle run' pour jouer


getNat :: String -> IO Int
getNat message = do putStr message
                    xs <- getLine
                    if xs /= [] && all isDigit xs then
                        return (read xs)
                    else
                        do putStrLn "Error: invalid number"
                           getNat message
-- Fonction qui demande à l'utilisateur de rentrer un nombre

tictactoe :: IO ()
tictactoe = do size <- getNat "Entrez la taille de la grille : "
               run (empty size) O
-- Fonction qui demande à l'utilisateur de rentrer la taille de la grille et appelle run

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "
-- Fonction qui affiche le message demandant au joueur de rentrer son coup

-- q --
run' :: Grid -> Player -> IO ()
run' g p | wins O g = putStrLn "Player O wins!\n"
         | wins X g = putStrLn "Player X wins!\n"
         | full g = putStrLn "It's a draw!\n"
         | otherwise = do i <- getNat (prompt p)
                          case move g i p of
                            [] -> do putStrLn "Error: invalid move"
                                     run g p
                            [g'] -> run g' (next p)