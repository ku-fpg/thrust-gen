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

vector :: Int -> [(Int, Expr a)] -> Func (Vector a)
vector sz elems = do p <- newLabel
                     let vector = HVector p sz elems
                     liftF $ Decl (vector) vector

transform :: Vector a -> Expr a -> Func (Vector a) 
transform c expr = do p <- newLabel
                      let func = CFunc ("f" ++ show p) expr Neither None StructBased  
                      liftF $ Trans func c c

interp :: Stmt a -> IO()
interp (Free a@(Decl v next))       = putStrLn (show a) >> interp next
interp (Free t@(Trans fun v next))  = putStrLn (show t) >> interp next
interp (Pure _)    = putStrLn "}"


generate :: Func a -> IO()
generate prog = do let prog' = evalStateT prog 0
                   res <- getLibs prog'
                   putStrLn $ concatMap (++"\n") $ nub res
                   putStrLn "int main (){"
                   interp prog'

