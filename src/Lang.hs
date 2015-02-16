{-# LANGUAGE ScopedTypeVariables #-} 
{-# LANGUAGE InstanceSigs #-}  
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Lang where
import Types
import Data.List
import Data.Maybe
import Control.Monad.State
import Control.Monad.Free

getLibInfo :: Statement a -> [([ImportDecl], LibName)]
getLibInfo (Decl _ _)    = [([Thrust], "host_vector")]
getLibInfo (Trans _ _ _) = [([Stdlib],"functional"), ([Thrust],"transform")]

getLib :: Statement a -> String
getLib l = concatMap 
            (\(xs,y) -> "#include <" 
                        ++ (concat $ map show xs) 
                        ++ y 
                        ++ (if (show $ head xs) == "thrust/" then ".h" else "")
                        ++ ">\n") (getLibInfo l) 

getLibs :: Stmt a -> IO [String]
getLibs (Free d@(Decl _ next))    = liftM2 (++) (return $ [getLib d]) (getLibs next)
getLibs (Free t@(Trans _ _ next)) = liftM2 (++) (return $ [getLib t]) (getLibs next)
getLibs (Pure _)                  = return []

getStructs :: Stmt a -> IO ()
getStructs (Free d@(Decl _ next)) = getStructs next
getStructs (Free t@(Trans func _ next)) = putStrLn (show func) >> getStructs next
getStructs (Pure _)                    = putStr ""

newLabel :: Func Int
newLabel = do p <- get
              put (p+1)
              return p

vector :: Int -> [(Int, Expr a)] -> Func (Vector a)
vector sz elems = do p <- newLabel
                     let name = "v" ++ show p
                         vector = HVector name sz elems
                     liftF $ Decl (vector) vector

transform :: Vector a -> (Expr a -> Expr a) -> Func (Vector a) 
transform c expr = do p <- newLabel
                      let body = (expr (Var "a"))
                          name = "f" ++ show p
                          func = CFunc name body Neither None StructBased  
                      liftF $ Trans func c c


--fold :: Vector a -> (Expr a -> Expr a -> Expr a) -> Func (Vector a)
--fold c expr = do p <- newLabel
--                 let body = (expr (Var "a")) (Var "b")
--                     name = "f" ++ show p
--                     func = CFunc name body Neither None StructBased 
--                liftF $ Fold func (initV body) c c
                
interp :: Stmt a -> IO()
interp (Free a@(Decl v next))       = putStrLn (show a) >> interp next
interp (Free t@(Trans fun v next))  = putStrLn (show t) >> interp next
interp (Pure _)    = putStrLn "}"


generate :: Func a -> IO()
generate prog = do let prog' = evalStateT prog 0
                   res <- getLibs prog'
                   putStrLn $ concatMap (++"") $ nub res
                   getStructs prog'
                   putStrLn "int main (){"
                   interp prog'

