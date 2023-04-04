--BOULLIER Arthur-- 
--    TDA-TP2    --
{-Partie 1-}
import Data.Char

-- a --
let2int :: Char -> Int
let2int c = ord c - ord 'A'

-- b --
int2let :: Int -> Char
int2let n = chr (ord 'A' + n)

-- c --
shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int (toUpper c) + n) `mod` 26)
          | isUpper c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c

-- d --
-- Version récursive --
cypherRec :: Int -> String -> String
cypherRec n [] = []
cypherRec n (x:xs) | isAlpha x = (shift n x:cypher n xs)
                | otherwise = cypher n xs

-- Version avec une liste de compréhension --
cypher :: Int -> String -> String
cypher n xs = [ shift n x | x <- xs , isAlpha x]
  
{-Partie 2-}
table::[Float]
table = [9.42,1.02,2.64,3.39,15.87,0.95,1.04,0.77,8.41,0.89,0.001,5.34,3.24,7.15,5.14,2.86,1.06,6.46,7.90,7.26,6.24,2.15,0.001,0.30,0.24,0.32]

-- e --
percent::Int -> Int -> Float
percent x y = ((fromIntegral x)/(fromIntegral y )) * 100

-- f --
count::Char -> String -> Int
count c [] = 0
count c xs = length [ x | x <- xs, x == c]

-- g --
freqs::String -> [Float]
freqs xs = [ percent (count x xs) (length xs) | x <- ['A'..'Z']  ]

{- Partie 3 -}
-- h --
chisqr::[Float] ->[Float] -> Float 
chisqr os es = sum [ ((x - y)^2) / y | (x,y) <- zip os es]

-- i --
rotate:: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

-- j --
positions::Eq a => a -> [a] -> [Int]
positions n xs = [ x | (x,y) <- zip [0..(length xs)] xs  ,  y == n]

-- k --
crack::String -> String
crack xs = cypher (-factor) xs
  where factor = head (positions (minimum chitab) chitab)
        chitab = [ chisqr (rotate n table') table | n <- [0..25] ]
        table' = freqs xs