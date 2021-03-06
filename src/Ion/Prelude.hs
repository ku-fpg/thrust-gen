{-- Emulating key pieces of the thrust API -}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds  #-}

module Ion.Prelude ( all_
		               , any_
		               , reduce
		               ) where

import Data.Boolean
import Data.Complex
import Ion.Private.Types
import Ion.Private.Lang
import Control.Monad.Free

reduce :: forall a. (Show a) => (Expr a, (Expr a -> Expr a -> Expr a)) 
	        -> Vector Device a 
		      -> Ion (Vector Device a)
reduce pr x = reduce__ x (fst $ pr) (snd $ pr)

-- Using double underscore suffix to denote same-name helper functions,
-- not to be exported
reduce__ :: (Show a) => Vector Device a -> Expr a 
	          -> (Expr a -> Expr a -> Expr a) 
		        -> Ion (Vector Device a)
reduce__ c initV expr = do p  <- newLabel
                           p2 <- newLabel
                           let body = (expr (Var "a"))(Var "b")  
                               to   = "v" ++ show p2
                               name = "f" ++ show p
                               func = CFunc name body Both BinaryFunc StructBased 2
                           liftF $ Fold to func c initV c
 
all_ :: (Boolean b) => (b, (b -> b -> b))
all_ = (true, (\x y -> x &&* y))

any_ :: (Boolean b) => (b, (b -> b -> b))
any_ = (false, (\x y -> x ||* y))
