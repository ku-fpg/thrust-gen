{-# LANGUAGE GADTs, KindSignatures, StandaloneDeriving, DeriveFunctor #-}

module Lang where

import Types
import Data.List
import Data.Maybe
import Control.Monad.State
import Control.Monad.Free

{-getLib :: Statement a -> String
getLib l = "#include <thrust/" ++ getLibNm l ++ ".h>\n"

getLibNm :: Statement a -> String
getLibNm (Decl _ _)    = "device_vector.h"
getLibNm (Trans _ _ _) = "transform.h"

getLibs :: Stmt a -> IO [String]
getLibs (Free d@(Decl _ next))    = liftM2 (++) (return $ [getLib d]) (getLibs next)
getLibs (Free t@(Trans _ _ next)) = liftM2 (++) (return $ [getLib t]) (getLibs next)
getLibs (Pure _)                  = return []-}

new :: Func Int
new = do p <- get
         put (p+1)
         return p

vector :: Int -> [(Int, Expr)] -> Func HVector
vector sz elems = do p <- new
                     let vector = Vec p sz elems
                     liftF $ Decl (vector) vector

transform :: (Expr -> Expr) -> HVector -> Func HVector
transform fun col = let expr = fun (Var "x")
                    in liftF $ Trans expr col col

{-runLibs :: Prog () -> IO [String]
runLibs (Free (Proc t name args body next)) = liftM2 (++) (getLibs (evalStateT body 0)) (runLibs next)
runLibs (Pure _) = return []-}

interp :: Stmt a -> IO()
interp (Free a@(Decl v next))       = putStrLn (show a) >> interp next
interp (Free t@(Trans fun v next))  = putStrLn (show t) >> interp next
interp (Pure _)    = putStrLn "}"

{-runInterp :: Prog () -> IO ()
runInterp (Free p@(Proc t name args body next)) = do putStrLn (show p)
                                                     interp (evalStateT body 0)
                                                     runInterp next 
runInterp (Pure _) = putStrLn ""-}

{-run :: Func () -> IO()
run p = do res <- runLibs p
           putStrLn $ concatMap (++"\n") $ nub res
           runInterp p-}
           
run :: Func () -> IO()
run prog = do putStrLn "int main (){"
              interp (evalStateT prog 0)

