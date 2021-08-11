data Op = Add | Sub | Mul | Div
  deriving Show

data Expr = Val Int | App Op Expr Expr
  deriving Show

numbers :: [Int]
numbers = [ 1, 3, 7, 10, 25, 50 ]

goal :: Int
goal = 765


solve :: [Int] -> Int -> [Expr]
solve ns n = [ e | e <- exprs ns,
                   eval e == Just n ]


exprs :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns = [ e | cs <- choices ns,
                 (ls, rs) <- splits cs,
                 l <- exprs ls,
                 r <- exprs rs,
                 e <- combine l r ]


splits :: [a] -> [([a], [a])]
splits []     = []
splits [_]    = []
splits (x:xs) = ([x],xs) : [ (x:ls, rs) | (ls, rs) <- splits xs ]


subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs


interleave :: a -> [a] -> [[a]]
interleave x []     =  [[x]]
interleave x (y:ys) =  (x:y:ys) : map (y:) (interleave x ys)


perms :: [a] -> [[a]]
perms []     =  [[]]
perms (x:xs) =  concat (map (interleave x) (perms xs))


choices :: [a] -> [[a]]
choices xs =  concat (map perms (subs xs))


combine :: Expr -> Expr -> [Expr]
combine l r = [ App op l r | op <- [ Add, Sub, Mul, Div ] ]


eval :: Expr -> Maybe Int
eval (Val x)       = Just x
eval (App op l r ) = if isTrue $ pure (valid op) <*> x <*> y
                     then pure (apply op)<*> x <*> y
                     else Nothing
                     where x      = eval l
                           y      = eval r
                           isTrue = (== Just True)


apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y


valid :: Op -> Int -> Int -> Bool
valid Add x y = True
valid Sub x y = x > y
valid Mul x y = True
valid Div x y = x `mod` y == 0 && y > 0


main :: IO ()
main = undefined
