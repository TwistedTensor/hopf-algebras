import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

data Polynomial a = Const Float
                   | Var a
                   | Sum [Polynomial a]
                   | Prod [Polynomial a]
                   | Pow (Polynomial a) Integer
                   deriving (Show)

-- instance (Show a)=>Show (Polynomial a) where
--     show (Const c)               = show c
--     show (Var x)                 = show x
--     show (Sum [term])            = show term
--     show (Prod [factor])         = case factor of
--                                      (Sum terms) -> "(" ++ show factor ++ ")" 
--                                      otherwise   -> show factor 
--     show (Sum (term:terms))      = show term   ++ "+" ++ show (Sum terms)
--     show (Prod (factor:factors)) = show factor ++ "*" ++ show (Prod factors)
--     show (Pow p n)               = case p of
--                                      (Const c)   -> show c ++ "^"    ++ show n
--                                      (Var x)     -> show x ++ "^"    ++ show n
--                                      otherwise   -> "("    ++ show p ++ ")^"   ++ show n

instance Functor Polynomial where
    fmap f (Const x)    = Const x
    fmap f (Var x)      = Var (f x)
    fmap f (Sum xs)     = Sum [f <$> x|x<-xs]
    fmap f (Prod xs)    = Prod [f <$> x|x<-xs]
    fmap f (Pow x n)    = Pow (f <$> x) n

instance Applicative Polynomial where
    pure = Var
    f <*> (Const x)    = Const x
    f <*> (Var x)      = ($ x)<$>f
    f <*> (Sum xs)     = Sum [f <*> x|x<-xs]
    f <*> (Prod xs)    = Prod [f <*> x|x<-xs]
    f <*> (Pow x n)    = Pow (f <*> x) n

instance Monad Polynomial where
     return            = Var
     Const c     >>= f = Const c
     Var x       >>= f = f x
     Sum xs      >>= f = Sum [x >>= f | x<-xs]
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
