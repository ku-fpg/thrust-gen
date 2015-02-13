import Lang
import Types
import Control.Monad.Free
foo :: Func (HVector Int)
foo = do a <- vector 20 [(0,5), (1, 20), (2, 4)]
         b <- transform (\x -> x + 1) a
         return b

{-prog :: Int -> Expr -> Func ()
prog x y = do res <- foo
              res' <- transform (\x -> x * 5) res
              {-let cfunc :: CFunctor
                  cfunc = CFunctor "foo" CInt [(CInt, "z"), (CInt, "y")] (4 + y)-}    
              return ()-}

--main = run (prog 0 1)
{-prog :: Stmt Int (HVector Integer)
prog = do let v = Vec 2 2 [(0, 5)] :: HVector Integer
          liftF $ Decl v v-}
          
test :: Func (HVector Int)
test = do a <- vector 20 [(0,5), (1, 20), (2, 4)]
          return a
          
main = run foo
-- Predefined cfunc example


{-adder :: CFunctor
adder = CFunctor "adder" CInt [(CInt, "z")] $ (\z -> (4 + z)) (Var "z")

main_:: IO ()
main_ = do 
          let cfunc = CFunctor "foo" CInt [(CInt, "z"), (CInt, "y")] $ (\y -> (4 + y)) (Var "y") 
          putStrLn $ show cfunc
          return ()-}  
