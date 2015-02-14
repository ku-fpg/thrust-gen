{-# LANGUAGE GADTs, KindSignatures, StandaloneDeriving, DeriveFunctor #-}

module Lang where

import Types
import Data.List
import Data.Maybe
import Control.Monad.State
import Control.Monad.Free

getLibInfo :: Statement a -> [(ImportDecl, LibName)]
getLibInfo (Decl _ _)    = [(Thrust, "host_vector")]
getLibInfo (Trans _ _ _) = [(Stdlib,"functional"), (Thrust,"transform")]

getLib :: Statement a -> String
getLib l = concat $ map 
            (\(x,y) -> "#include <" ++ show x ++ y ++ ".h>\n") (getLibInfo l) 

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

transform :: Vector a -> (Expr a -> Expr a) -> Func (Vector a) 
transform c expr = do p <- newLabel
                      let body = expr (Var "a")
                          name = "f" ++ show p
                          func = CFunc name body Neither None StructBased  
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

