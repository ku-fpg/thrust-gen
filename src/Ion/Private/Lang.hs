{-# LANGUAGE ScopedTypeVariables #-} 
{-# LANGUAGE InstanceSigs #-}  
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

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
getLibInfo (Unload _ _)     = [([Thrust], "device_vector")]
getLibInfo (Trans _ _ _)    = [([Thrust],"transform")]
getLibInfo (Cout _ _)       = [([Stdlib], "iostream")]
getLibInfo (Fold _ _ _ _ _) = [([Thrust], "reduce")]
getLibInfo (Sort _ _)       = [([Thrust], "sort")]
getLibInfo (AdjDiff _ _)    = [([Thrust], "adjacent_difference")]
getLibInfo (UpperBound _ _ _ _) = [([Thrust], "binary_search")]
getLibInfo (RandomGen _ _ _) = [([Thrust], "generate")]
                                 ++  [([Thrust], "random")]
getLibInfo (IDecl i _)        = [([Thrust, Iterator], case i of
                                                         CountingIterator _ _ -> "counting_iterator")]

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
getLibs (Free d@(Decl _ next))             = liftM2 (++) (return [getLib d]) (getLibs next)
getLibs (Free l@(Load _ next))             = liftM2 (++) (return [getLib l]) (getLibs next)
getLibs (Free u@(Unload _ next))           = liftM2 (++) (return [getLib u]) (getLibs next)
getLibs (Free t@(Trans _ _ next))          = liftM2 (++) (return [getLib t]) (getLibs next)
getLibs (Free c@(Cout _ next))             = liftM2 (++) (return [getLib c]) (getLibs next)
getLibs (Free f@(Fold _ _ _ _ next))       = liftM2 (++) (return [getLib f]) (getLibs next)
getLibs (Free s@(Sort _ next))             = liftM2 (++) (return [getLib s]) (getLibs next)
getLibs (Free a@(AdjDiff _ next))          = liftM2 (++) (return [getLib a]) (getLibs next)
getLibs (Free c@(IDecl _ next))            = liftM2 (++) (return [getLib c]) (getLibs next)
getLibs (Free u@(UpperBound _ _ _ next))   = liftM2 (++) (return [getLib u]) (getLibs next)
getLibs (Free r@(RandomGen _ _ next))      = liftM2 (++) (return [getLib r]) (getLibs next)
getLibs (Pure _)                           = return []

getStructs :: Stmt a -> IO [String] 
getStructs (Free d@(Decl _ next))             = getStructs next
getStructs (Free l@(Load _ next))             = getStructs next
getStructs (Free u@(Unload _ next))           = getStructs next
getStructs (Free t@(Trans func _ next))       = liftM2 (++) (return [(show func)]) (getStructs next)
getStructs (Free c@(Cout _ next))             = getStructs next
getStructs (Free f@(Fold _ func _ _ next))    = liftM2 (++) (return [(show func)]) (getStructs next)
getStructs (Free s@(Sort _ next))             = getStructs next
getStructs (Free a@(AdjDiff _ next))          = getStructs next
getStructs (Free c@(IDecl _ next))            = getStructs next
getStructs (Free u@(UpperBound _ _ _ next))   = getStructs next
getStructs (Free r@(RandomGen _ _ next))  = getStructs next
getStructs (Pure _)                           = return []

newLabel :: Ion Int
newLabel = get >>= \p -> put (p+1) >> return p

int :: Int -> Expr Int 
int = I

bool :: Bool -> Expr Bool
bool = B

complex :: Complex Float -> Expr (Complex Float)
complex = Cx

-- initialize a host vector
vector :: forall a. (ToExpr a) => (a -> Expr a) -> [a] -> Ion (Vector Host a)
vector _ elems =    do p <- newLabel
                       let name = "v" ++ show p
                           sz = length elems
                           vect :: Vector Host a
                           vect = Vector name sz (zip [0..(sz-1)] (map toExpr elems))
                       liftF $ Decl vect vect

random :: Vector Host a -> (Int, Int) -> Ion (Random a)
random v (a,b)      = do nm  <- newLabel
                         nm2 <- newLabel
                         nm3 <- newLabel
                         let name  = "r" ++ show nm
                             name2 = "r" ++ show nm2
                             name3 = "r" ++ show nm3
                             rand = Random {rLabels = [name, name2, name3], bounds = (a,b)}
                         liftF $ RandomGen v rand rand  
                         
-- copy a host vector into a device vector
load :: forall a. Vector Host a -> Ion (Vector Device a)
load v = let dvec :: Vector Device a 
             dvec = Vector ('d' : (drop 1 $ label v)) (size v) (elems v)
         in liftF $ Load dvec dvec 

unload :: forall a. Vector Device a -> Ion (Vector Host a)
unload v = let hvec :: Vector Host a 
               hvec = Vector ('h' : (drop 1 $ label v)) (size v) (elems v)
         in liftF $ Unload hvec hvec 


-- a map operation across either host or device vectors
transform :: Vector Device a -> (Expr a -> Expr a) -> Ion (Vector Device a) 
transform c expr = do p <- newLabel
                      let body = (expr (Var "a"))
                          name = "f" ++ show p
                          func = CFunc name body Both None StructBased 1 
                      liftF $ Trans func c c

cout :: Vector Host a -> Ion ()
cout v = liftF $ Cout v ()

sort :: Vector Device a -> Ion (Vector Device a)
sort v = liftF $ Sort v v

adjdiff :: Vector Device a -> Ion (Vector Device a)
adjdiff v = liftF $ AdjDiff v v

countingiterator :: (Num a, ToExpr a) => (a -> Expr a) -> a -> Ion (Iterator a)
countingiterator _ expr = do p <- newLabel
                             let name = "i" ++ show p
                                 iter = CountingIterator name (toExpr expr)
                             liftF $ IDecl iter iter

upperbound :: Vector Device a -> Vector Device a -> Iterator a -> Ion(Vector Device a)
upperbound input output iter = liftF $ UpperBound input output iter output

interp :: Stmt a -> IO ()
interp (Free a@(Decl v next))                 = putStrLn (show a) >> interp next
interp (Free l@(Load v next))                 = putStrLn (show l) >> interp next
interp (Free u@(Unload v next))               = putStrLn (show u) >> interp next
interp (Free t@(Trans fun v next))            = putStrLn (show t) >> interp next
interp (Free c@(Cout v next))                 = putStrLn (show c) >> interp next
interp (Free f@(Fold _ fun v _ next))         = putStrLn (show f) >> interp next
interp (Free s@(Sort v next))                 = putStrLn (show s) >> interp next
interp (Free a@(AdjDiff v next))              = putStrLn (show a) >> interp next
interp (Free i@(IDecl iter next))             = putStrLn (show i) >> interp next
interp (Free u@(UpperBound v1 v2 i next))     = putStrLn (show u) >> interp next
interp (Free r@(RandomGen v rand next))       = putStrLn (show r) >> interp next
interp (Pure _)    = putStrLn "}"


toThrust :: Ion a -> IO ()
toThrust prog = do let prog' = evalStateT prog 0
                   res <- getLibs prog'
                   putStrLn $ concatMap (++"") $ nub res
                   funcs <- getStructs prog'
                   putStrLn $ concatMap (++"\n") funcs
                   putStrLn "int main (void){"
                   interp prog'

infixr 4 #
v # (fn,expr) = fn v expr  

infixr 6 <#>
(<#>) a b = (a,b)
