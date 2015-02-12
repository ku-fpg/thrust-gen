{-# LANGUAGE GADTs, KindSignatures, StandaloneDeriving, DeriveFunctor #-}

module Lang where

import Types
import Data.List
import Data.Maybe
import Control.Monad.State
import Control.Monad.Free

getLibGrp :: Statement a -> LibGroup 
getLibGrp (Print _ _) = StdLib
getLibGrp _           = Thrust

getLib :: Statement a -> String
getLib l = "#include <" ++ (show $ getLibGrp l) ++ "/" ++ getLibNm l ++ ".h>"

getLibNm :: Statement a -> String
getLibNm (Decl HVector {} _)    = "host_vector"
getLibNm (Decl DVector {} _)    = "device_vector"
getLibNm (Trans _ _ _) = "transform"
getLibNm (Print _ _)   = "iostream"

getLibs :: Stmt a -> IO [String]
getLibs (Free d@(Decl HVector {} next))    = liftM2 (++) (return $ [getLib d]) (getLibs next)
getLibs (Free d@(Decl DVector {} next))   = liftM2 (++) (return $ [getLib d]) (getLibs next)
getLibs (Free t@(Trans _ _ next)) = liftM2 (++) (return $ [getLib t]) (getLibs next)
getLibs (Pure _)                  = return []

new :: Func Int
new = do p <- get
         put (p+1)
         return p

vector :: Int -> [(Int, Expr)] -> Func Vector
vector sz elems = do p <- new
                     let vector = HVector { hIdent = p, hSize = sz, hAssignments = elems}
                     liftF $ Decl (vector) vector

transform :: (Expr -> Expr) -> Vector -> Func Vector
transform fun col = let expr = fun (Var "x")
                    in liftF $ Trans expr col col

interp :: Stmt a -> IO()
interp (Free a@(Decl v next))       = putStrLn (show a) >> interp next
interp (Free t@(Trans fun v next))  = putStrLn (show t) >> interp next
interp (Pure _)    = putStrLn "}"


run :: Func () -> IO()
run prog = do let prog' = evalStateT prog 0
              res <- getLibs prog'
              putStrLn $ concatMap (++"\n") $ nub res
              putStrLn "int main (){"
              interp prog'

