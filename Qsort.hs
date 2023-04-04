{- Exercice 2 -}
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort(x:xs) = qsort smaller ++ [x] ++ qsort larger
 where smaller = [a|a<-xs,a<=x]
       larger = [a|a<-xs,a>x]
       
{- Exercice 3 -}
-- a --
egaux4 :: Int -> Int -> Int -> Int -> Bool
egaux4 a b c d = a == b && b == c && c == d

-- b --
max4 :: Int -> Int -> Int -> Int -> Int
max4 a b c d = if a > b && a > c && a > d
 then a 
 else if b > c && b > d
  then b
  else if c > d 
  then c
   else d

{- Exercice 4 -}
-- a -- 
sumcarre :: Int
sumcarre = sum [ x^2 | x <- [1..100]]

-- b -- 
replic :: Int -> a -> [a]
replic 1 e = [e] 
replic x e = e : replic (x-1) e

-- c -- 
pyths :: Int -> [(Int,Int,Int)]
pyths a = [(x,y,z) | x <- [1..a] , y <- [1..a] , z <-[1..a] , x^2 + y^2 == z^2 || z^2 + y^2 == x^2 || x^2 + z^2 == y^2, x == a || y == a || z == a  ]


{- Exercice 5 -}
-- a --
inverse :: [a] -> [a]
inverse [] = []
inverse (x) = (last x) : inverse ( init x ) 

-- b --
isPalindrome :: Eq a => [a] -> Bool
isPalindrome (a) = a == (inverse a)

-- c --
doPalindrome :: [a] -> [a]
doPalindrome (a) = (init a) ++ (inverse a)


{- Exercice 5 -}
-- a -- 
data Parfum = Chocolat | Vanille | Framboise

-- b -- 
prixParfum :: Parfum -> Float 
prixParfum Chocolat = 1.5
prixParfum Vanille = 1.2
prixParfum Framboise = 1.4

-- c --
data Glace = UneBoule Parfum | DeuxBoules Parfum Parfum | TroisBoules Parfum Parfum Parfum

-- d --
prixGlace :: Glace -> Float
prixGlace (UneBoule a) = 0.1 + (prixParfum a)
prixGlace (DeuxBoules a b) = 0.15 + (prixParfum a) + (prixParfum b)
prixGlace (TroisBoules a b c) = 0.20 + (prixParfum a) + (prixParfum b) + (prixParfum c)


{- Exercice 7 -}
type Domino = (Int , Int)
-- a --
dominosA2Match :: Domino -> Domino -> Bool
dominosA2Match (xa,ya)(xb,yb) = xa == xb || xa == yb || ya == xb || ya == yb 

-- b -- 
dominosA3Match :: Domino -> Domino -> Domino -> Bool
dominosA3Match (xa,ya) (xb,yb) (xc,yc) = (xa == xb && ya == yb) && (xb == xc || yb == yc) || (xa == yb && ya == xb) && (xb == xc || yb == yc) ||(xb == xa && yb == ya) && (xa == xc || ya == yc) ||(xb == yb && xa == xc) && (ya == yc || yb == yc) ||(ya == xb && xa == yb) && (xb == xc || yb == yc) ||(ya == yb && xa == xc) && (xb == yc || yb == yc) ||(yb == xa && ya == xc) && (xa == yc || ya == yc) ||(yb == xb && xa == yc) && (ya == yc || yb == yc) ||(yc == xa && xb == ya) && (xa == yb || ya == yb) ||(yc == xb && xa == yb) && (ya == yb || yc == ya) ||(yc == ya && xa == yb) && (xb == yb || yc == xb) ||(yc == yb && xa == xb) && (ya == xb || yc == ya)