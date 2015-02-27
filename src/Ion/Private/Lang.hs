{-# LANGUAGE ScopedTypeVariables #-} 
{-# LANGUAGE InstanceSigs #-}  
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Ion.Private.Lang where
import Ion.Private.Types
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import Data.Complex
import Data.Boolean
import Control.Monad.State
import Control.Monad.Free

getLibInfo :: Statement a -> [([ImportDecl], LibName)]
getLibInfo (Decl v _) = [([Thrust], "host_vector")] 
                        ++ (case (snd $ head $ elems $ v) of
                              Cx _ -> [([Cuda], "cuda_complex")]
                              _    -> [])
getLibInfo (Load _ _)       = [([Thrust], "device_vector")]
getLibInfo (Trans _ _ _)    = [([Stdlib],"functional"), ([Thrust],"transform")]
getLibInfo (Cout _ _)       = [([Stdlib], "iostream")]
getLibInfo (Fold _ _ _ _ _) = [([Thrust], "reduce")]
getLibInfo (Sort _ _)       = [([Thrust], "sort")]

getLib :: Statement a -> String
getLib l = concatMap 
            (\(xs,y) -> "#include <" 
                        ++ (concat $ map show xs) 
                        ++ y 
                        ++ (case (head xs) of
                              Thrust -> ".h" 
                              Cuda   -> ".hpp"
                              Stdlib -> "")
                        ++ ">\n") (getLibInfo l) 

getLibs :: Stmt a -> IO [String]
getLibs (Free d@(Decl _ next))       = liftM2 (++) (return [getLib d]) (getLibs next)
getLibs (Free l@(Load _ next))       = liftM2 (++) (return [getLib l]) (getLibs next)
getLibs (Free t@(Trans _ _ next))    = liftM2 (++) (return [getLib t]) (getLibs next)
getLibs (Free c@(Cout _ next))       = liftM2 (++) (return [getLib c]) (getLibs next)
getLibs (Free f@(Fold _ _ _ _ next)) = liftM2 (++) (return [getLib f]) (getLibs next)
getLibs (Free s@(Sort _ next))       = liftM2 (++) (return [getLib s]) (getLibs next)
getLibs (Pure _)                     = return []

getStructs :: Stmt a -> IO [String] 
getStructs (Free d@(Decl _ next))           = getStructs next
getStructs (Free l@(Load _ next))           = getStructs next
getStructs (Free t@(Trans func _ next))     = liftM2 (++) (return [(show func)]) (getStructs next)
getStructs (Free c@(Cout _ next))           = getStructs next
getStructs (Free f@(Fold _ func _ _ next))  = liftM2 (++) (return [(show func)]) (getStructs next)
getStructs (Free s@(Sort _ next))           = getStructs next
getStructs (Pure _)                         = return []

newLabel :: Ion Int
newLabel = get >>= \p -> put (p+1) >> return p

int :: Int -> Expr Int 
int = I

bool :: Bool -> Expr Bool
bool = B

complex :: Complex Double -> Expr (Complex Double)
complex = Cx

-- initialize a host vector
vector :: (ToExpr a) => (a -> Expr a) -> [a] -> Ion (Vector a)
vector _ elems =    do p <- newLabel
                       let name = "v" ++ show p
                           sz = length elems
                           vector = HVector name sz (zip [0..(sz-1)] (map toExpr elems))
                       liftF $ Decl (vector) vector

-- copy a host vector into a device vector
load :: Vector a -> Ion (Vector a)
load v = let dvec = DVector ('d' : (drop 1 $ label v)) (size v) (elems v)
         in liftF $ Load dvec dvec

-- a map operation across either host or device vectors
transform :: Vector a -> (Expr a -> Expr a) -> Ion (Vector a) 
transform c expr = do p <- newLabel
                      let body = (expr (Var "a"))
                          name = "f" ++ show p
                          func = CFunc name body Neither None StructBased 1 
                      liftF $ Trans func c c

cout :: Vector a -> Ion ()
cout v = liftF $ Cout v ()

sort :: Vector a -> Ion (Vector a)
sort v = liftF $ Sort v v

interp :: Stmt a -> IO ()
interp (Free a@(Decl v next))         = putStrLn (show a) >> interp next
interp (Free l@(Load v next))         = putStrLn (show l) >> interp next
interp (Free t@(Trans fun v next))    = putStrLn (show t) >> interp next
interp (Free c@(Cout v next))         = putStrLn (show c) >> interp next
interp (Free f@(Fold _ fun v _ next)) = putStrLn (show f) >> interp next
interp (Free s@(Sort v next))         = putStrLn (show s) >> interp next
interp (Pure _)    = putStrLn "}"


toThrust :: Ion a -> IO ()
toThrust prog = do let prog' = evalStateT prog 0
                   res <- getLibs prog'
                   putStrLn $ concatMap (++"") $ nub res
                   funcs <- getStructs prog'
                   putStrLn $ concatMap (++"\n") funcs
                   putStrLn "int main (){"
                   interp prog'

infixr 4 #
v # (fn,expr) = fn v expr  

infixr 6 <#>
(<#>) a b = (a,b)
