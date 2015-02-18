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
getLibInfo (Load _ _)    = [([Thrust], "device_vector")]
getLibInfo (Trans _ _ _) = [([Stdlib],"functional"), ([Thrust],"transform")]
getLibInfo (Cout _ _)    = [([Stdlib], "iostream")]
getLibInfo (Fold _ _ _ _ _) = [([Thrust], "reduce")]

getLib :: Statement a -> String
getLib l = concatMap 
            (\(xs,y) -> "#include <" 
                        ++ (concat $ map show xs) 
                        ++ y 
                        ++ (if (show $ head xs) == "thrust/" then ".h" else "")
                        ++ ">\n") (getLibInfo l) 

getLibs :: Stmt a -> IO [String]
getLibs (Free d@(Decl _ next))       = liftM2 (++) (return [getLib d]) (getLibs next)
getLibs (Free l@(Load _ next))       = liftM2 (++) (return [getLib l]) (getLibs next)
getLibs (Free t@(Trans _ _ next))    = liftM2 (++) (return [getLib t]) (getLibs next)
getLibs (Free c@(Cout _ next))       = liftM2 (++) (return [getLib c]) (getLibs next)
getLibs (Free f@(Fold _ _ _ _ next)) = liftM2 (++) (return [getLib f]) (getLibs next)
getLibs (Pure _)                     = return []

getStructs :: Stmt a -> IO ()
getStructs (Free d@(Decl _ next))           = getStructs next
getStructs (Free l@(Load _ next))           = getStructs next
getStructs (Free t@(Trans func _ next))     = putStrLn (show func) >> getStructs next
getStructs (Free c@(Cout _ next))           = getStructs next
getStructs (Free f@(Fold _ func _ _ next))  = putStrLn (show func) >> getStructs next
getStructs (Pure _)                         = putStr ""

newLabel :: Ion Int
newLabel = do p <- get
              put (p+1)
              return p

vector :: Int -> [(Int, Expr a)] -> Ion (Vector a)
vector sz elems = do p <- newLabel
                     let name = "v" ++ show p
                         vector = HVector name sz elems
                     liftF $ Decl (vector) vector

load :: Vector a -> Ion (Vector a)
load v = let dvec = DVector ('d' : (drop 1 $ label v)) (size v) (elems v)
         in liftF $ Load dvec dvec

transform :: Vector a -> (Expr a -> Expr a) -> Ion (Vector a) 
transform c expr = do p <- newLabel
                      let body = (expr (Var "a"))
                          name = "f" ++ show p
                          func = CFunc name body Neither None StructBased 1 
                      liftF $ Trans func c c

cout :: Vector a -> Ion ()
cout v = liftF $ Cout v ()

reduce c initV expr = do p  <- newLabel
                         p2 <- newLabel
                         let body = (expr (Var "a")) (Var "b")
                             to   = "v" ++ show p2
                             name = "f" ++ show p
                             func = CFunc name body Neither None StructBased 2
                         liftF $ Fold to func c initV c
                
interp :: Stmt a -> IO()
interp (Free a@(Decl v next))       = putStrLn (show a) >> interp next
interp (Free l@(Load v next))       = putStrLn (show l) >> interp next
interp (Free t@(Trans fun v next))  = putStrLn (show t) >> interp next
interp (Free c@(Cout v next))       = putStrLn (show c) >> interp next
interp (Free f@(Fold _ fun v _ next)) = putStrLn (show f) >> interp next
interp (Pure _)    = putStrLn "}"


toThrust :: Ion a -> IO()
toThrust prog = do let prog' = evalStateT prog 0
                   res <- getLibs prog'
                   putStrLn $ concatMap (++"") $ nub res
                   getStructs prog'
                   putStrLn "int main (){"
                   interp prog'

