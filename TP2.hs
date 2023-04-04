{- Exercice 1 -}

-- a --
paire:: Int -> Bool
paire n | n == 0 = True
        | otherwise = impaire (n - 1)


impaire:: Int -> Bool
impaire n | n == 0 = False
          | otherwise = paire (n - 1)

-- b --
insert::Ord a=>a->[a]->[a]
insert a [] = [a]
insert a (x:xs) | a <= x = (a:x:xs)
                | otherwise = (x:insert a xs)

-- c --
isort::Ord a=>[a]->[a]
isort [] = []
isort (x:xs) = insert x (isort xs)


{- Exercice 2 -}

-- a --
halve::[a] -> ([a], [a])
halve [] = ([],[])
halve x = (take ((length x) `div` 2) x, drop ((length x) `div` 2) x)

-- b --
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []

-- Cas pas prévus ou les listes ne sont pas de la même taille -- 
merge x [] = x
merge [] x = x
merge (x:xs) (y:ys) | x <= y = (x:merge xs (y:ys))
                    | otherwise = (y:merge (x:xs) ys)

-- c --
msort :: Ord a => [a] -> [a] 
msort [] = []
msort [x] = [x]
msort x = merge (msort a) (msort b)
        where (a,b) = halve x 

{- Exercice 3 -}

-- Partie 1 -- 

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))
p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')
p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))
p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

type Assoc k v = [(k,v)]
type Subst = Assoc Char Bool

-- a --
find:: Eq k => k -> Assoc k v -> v
--find k a = head [ y | (x,y) <- a, x == k]
-- autre solution récursive
find k ((a,b):xs) | a == k =  b
                  | otherwise = find k xs

-- b --
eval::Subst -> Prop -> Bool
eval _ (Const b) = b
eval a (Var c) = find c a 
eval a (Not n) = not (eval a n)
eval a (And p1 p2) = (eval a p1) && (eval a p2)
eval a (Imply p1 p2) = (eval a p1) <= (eval a p2)
