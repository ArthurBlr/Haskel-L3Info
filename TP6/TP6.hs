--BOULLIER Arthur-- 
--    TDA-TP2    --
{-Partie 1-}

-- a --
data Op = Add|Sub|Mul|Div

instance Show Op where
    show Add = " + "
    show Sub = " - "
    show Mul = " * "
    show Div = " / "

-- b --
valid :: Op -> Int -> Int -> Bool
valid Add x y = True
valid Sub x y = x > y
valid Mul x y = True
valid Div x y = y /= 0 && x `mod` y == 0

-- c --
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

{-Partie 2-}
-- d --
data Expr = Val Int | App Op Expr Expr

instance Show Expr where
    show (Val x) = show x
    show (App o e1 e2) = parenthese e1 ++ show o ++ parenthese e2
        where
            parenthese (Val n) = show n
            parenthese e = "(" ++ show e ++ ")"

-- e --
values :: Expr -> [Int]
values (Val x) = [x]
values (App o e1 e2) = values e1 ++ values e2

-- f --
eval :: Expr -> [Int]
eval (Val x) = [x]
eval (App op e1 e2) = [apply op x y | x <- eval e1, y <- eval e2, valid op x y]

-- g --
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x l = [(take i l ++ [x] ++ drop i l) | i <- [0..length l]]

{-Partie 3-}

-- h --
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat [interleave x ys | ys <- perms xs]

-- i --
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = map (x:) (subs xs) ++ subs xs
-- Cette fonction retourne une liste de liste de a, qui contient toutes les sous-listes de la liste en paramètre (y compris la liste vide).

-- j --
choices :: [a] -> [[a]]
choices xs = concat [perms ys | ys <- subs xs]

{-Partie 4-}
-- k --
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]


{-Partie 5-}
-- i --
split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

-- m --
ops :: [Op]
ops = [Add,Sub,Mul,Div]

combine :: Expr -> Expr -> [Expr]
combine e1 e2 = [App o e1 e2 | o <- ops]


-- n --
exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- split ns, l <- exprs ls, r <- exprs rs, e <- combine l r]

-- o -- 
solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

printSol [] = return ()
printSol (x:xs) = do print x
                     printSol xs

main = printSol $ solutions [1,3,7,10,25,50] 765