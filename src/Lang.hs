{-# LANGUAGE GADTs, KindSignatures, StandaloneDeriving, DeriveFunctor #-}

module Lang where

import Types
import Data.List
import Data.Maybe
import Control.Monad.State
import Control.Monad.Free

getLib :: Statement a -> String
getLib l = "#include <thrust/" ++ getLibNm l ++ ".h>"

getLibNm :: Statement a -> String
getLibNm (Decl _ _)    = "host_vector"
getLibNm (Trans _ _ _) = "transform"

getLibs :: Stmt a -> IO [String]
getLibs (Free d@(Decl _ next))    = liftM2 (++) (return $ [getLib d]) (getLibs next)
getLibs (Free t@(Trans _ _ next)) = liftM2 (++) (return $ [getLib t]) (getLibs next)
getLibs (Pure _)                  = return []

newLabel :: Func Int
newLabel = do p <- get
              put (p+1)
              return p

vector :: Int -> [(Int, Expr a)] -> Func (HVector a)
vector sz elems = do p <- newLabel
                     let vector = Vec p sz elems
                     liftF $ Decl (vector) vector

--transform :: (Expr a -> Expr a) -> HVector a -> Func (HVector a)
--transform fun col = let expr = cfunc fun (Var "x")
--                    in  let cfun = retract $ cfunc expr either None StructBased 
 --                       in liftF $ Trans cfun col col

--cfunc :: (Expr a) -> LocationDecl -> InheritDecl -> FuncType -> Func (CFunc a)
--cfunc b l i f = do label <- newLabel
--                   let strLabel = "f" ++ show newLabel 
--                       functor  = CFunc strLabel b l i f
--                   liftF $ functor 

interp :: Stmt a -> IO()
interp (Free a@(Decl v next))       = putStrLn (show a) >> interp next
interp (Free t@(Trans fun v next))  = putStrLn (show t) >> interp next
interp (Pure _)    = putStrLn "}"


run :: Func a -> IO()
run prog = do let prog' = evalStateT prog 0
              res <- getLibs prog'
              putStrLn $ concatMap (++"\n") $ nub res
              putStrLn "int main (){"
              interp prog'

