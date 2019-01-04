{-# LANGUAGE GADTs #-}
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

data FreeAlgebra a b where
    Zero::FreeAlgebra a b
    Var:: b -> FreeAlgebra a b
    Sum::[FreeAlgebra a b] -> FreeAlgebra a b
    SProd:: Num a => a -> FreeAlgebra a b -> FreeAlgebra a b
    Prod::[FreeAlgebra a b] -> FreeAlgebra a b
    Pow::FreeAlgebra a b -> Integer -> FreeAlgebra a b

(|+|)::FreeAlgebra a b->FreeAlgebra a b->FreeAlgebra a b
x |+| y = Sum [x,y] 
(|*|)::FreeAlgebra a b->FreeAlgebra a b->FreeAlgebra a b
x |*| y = Prod [x,y] 
(|^|)::FreeAlgebra a b->Integer->FreeAlgebra a b
x |^| n = Pow x n

s::FreeAlgebra a b -> FreeAlgebra a b
s (Sum [])              = Zero
s (Sum [x])             = s x
s (Prod [x])            = s x
s (Sum (Zero:xs))       = s (Sum xs)
s (Prod (Zero:xs))      = Zero
s (Pow Zero 1)          = Zero
s (Pow x 1)             = x
s (Sum ((Sum xs):ys))   = Sum (xs ++ ys)
s (Prod ((Prod xs):ys)) = Prod (xs ++ ys)
s (SProd k (Sum xs))    = Sum [SProd k (s x)|x<-xs]
s (Prod ((Sum [x]):ys))    = s (Prod (x:ys))
s (Prod ((Sum (x:xs)):ys))    = (s (x |*| Prod ys)) |+| (s (Sum xs |*| Prod ys))
s (Pow (Prod xs) n)     = Prod [Pow x n|x<-xs]
s (Sum xs)              = Sum (s<$>xs)
s (Prod xs)             = Prod (s<$>xs)
s x                     = x

instance (Show a,Show b)=>Show (FreeAlgebra a b) where
     show Zero          = "0"
     show (Prod [])     = ""
     show (Sum  [])     = ""
     show (Var x)       = show x
     show (SProd k x)   = (show k) ++ (show x)
     show (Sum [x])     = show x
     show (Prod [x])    = case x of
                              Sum ys      -> "(" ++ show x ++ ")" 
                              otherwise   -> show x 
     show (Sum (x:xs))  = show x ++ "+" ++ show (Sum xs)
     show (Prod (x:xs)) = show (Prod [x]) ++ "*" ++ show (Prod xs)
     show (Pow x n)     = case x of
                               Var y       -> show y ++ "^"    ++ show n
                               otherwise   -> "("    ++ show x ++ ")^"   ++ show n

instance Functor (FreeAlgebra a) where
    fmap f (Var x)      = Var (f x)
    fmap f (Sum xs)     = Sum [f <$> x|x<-xs]
    fmap f (SProd k x)  = SProd k (f <$> x)
    fmap f (Prod xs)    = Prod [f <$> x|x<-xs]
    fmap f (Pow x n)    = Pow (f <$> x) n

instance Applicative (FreeAlgebra a) where
    pure = Var
    f <*> (Var x)      = ($ x)<$>f
    f <*> (Sum xs)     = Sum [f <*> x|x<-xs]
    f <*> (SProd k x)  = SProd k (f <*> x)
    f <*> (Prod xs)    = Prod [f <*> x|x<-xs]
    f <*> (Pow x n)    = Pow (f <*> x) n

instance Monad (FreeAlgebra a) where
     return            = Var
     Var x       >>= f = (f x)
     Sum xs      >>= f = Sum [x >>= f | x<-xs]
     SProd k x   >>= f = SProd k (x >>= f)
     Prod xs     >>= f = Prod [x >>= f | x<-xs]
     Pow p n     >>= f = Pow (p >>= f) n

--  instance Num (Polynomial a) where
--      p + q         = Sum [p,q]
--      negate p      = Prod [Const -1,p]
--      p - q         = Sum [p,negate q]
--      p * q         = Prod [p,q]
--      abs p         = p
--      signum p      = 1
--      fromInteger n = Const (fromInteger n)
-- simplify    :: Polynomial -> Polynomial
-- simplify p  = case p of
--     Sum ((Sum inner):outer) -> Sum (inner ++ outer) 
--     Sum (t1:t2:rest)        -> case (t1,t2) of
--         (Const c1,Const c2)                     -> Sum ((c1 + c2) : rest)
--         (t,t)                                   -> Sum ((Prod (Const 2) t) : rest)
--         (Prod (Const c1,t), Prod (Const c2,t')) -> if t == t' then
--                                                     Sum ((Prod (Const (c1 + c2)) t) : rest)
--                                                     else p
--         otherwise                               -> if deg t2 < deg t1 then Sum (t2:t1:rest) else p
--     otherwise               -> p
