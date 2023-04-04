--BOULLIER Arthur-- 
--    TDA-TP2    --
{-Exercice 1-}

import Data.Char
import Data.List
import System.IO
-- a --
getCh::IO Char
getCh = do hSetEcho stdin False
           c <- getChar
           hSetEcho stdin True
           return c

-- b --
sgetLine::IO String
sgetLine = do x <- getCh
              if x == '\n' then
                  do putChar x
                     return []
              else
                  do putChar '-'
                     xs <- sgetLine
                     return (x:xs)
-- c --
match::String -> String -> String
match [] _ = []
match _ [] = []
match xs ys = [if elem x ys then x else '-' | x <- xs ]

-- d --
play :: String -> IO ()
play xs = do putStr "? "
             c <- getLine
             if c == xs then putStrLn "you got it !"
             else do putStrLn (match xs c)
                     play xs

-- e --
hangman :: IO ()
hangman = do putStrLn "Think of a word:"
             s <- sgetLine
             putStrLn "Try to guess it:"
             play s

main = hangman


{-Exercice 2-}
-- a --
type Matf = Int -> Int -> (Bool,Int)

-- b --
exemple :: Matf
exemple i j | i >= 1 && i <= 6 && j >= 1 && j <= 5 = (True,(2*i) + j)
            | otherwise = (False,0)


-- c --
identite4x4 :: Matf
identite4x4 i j | i == j  && i <= 4 = (True,1)
                | i <= 4 && j <= 4 = (True,0)
                | otherwise = (False,0)


-- d --


nbLines :: Matf -> Int
nbLines m = nbLines' m 1
            where nbLines' m i | fst (m i 1) = 1 + nbLines' m (i+1)
                               | otherwise = 0

nbCols :: Matf -> Int
nbCols m = nbCols' m 1
            where nbCols' m j | fst (m 1 j) = 1 + nbCols' m (j+1)
                              | otherwise = 0

dims :: Matf -> (Int,Int)
dims m = (nbLines m, nbCols m)

-- e --
cmpDims :: Matf -> Matf -> Bool
cmpDims m n = dims m == dims n

-- f --
add :: Matf -> Matf -> Matf
add m n i j | cmpDims m n = (True, (snd (m i j)) + (snd (n i j)))
            | otherwise = error "Les matrices ne font pas la même taille"


-- g --
identite6x5 :: Matf
identite6x5 i j | i == j  && i <= 6 && j<= 5 = (True,1)
                | i <= 6 && j<= 5 = (True,0)
                | otherwise = (False,0)

