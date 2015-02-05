{-# LANGUAGE GADTs, KindSignatures, StandaloneDeriving, DeriveFunctor #-}

module Lang where

import Types
import Data.List
import Data.Maybe
import Control.Monad.State
import Control.Monad.Free
import Prelude hiding(lookup)

import Data.Char
import Data.Data
import Data.Typeable
import Data.Map.Lazy as Map

getLib :: Statement a -> String
getLib l = "#include <thrust/" ++ getLibNm l ++ ">\n"

getLibNm :: Statement a -> String
getLibNm (Decl _ _)    = "device_vector.h"
getLibNm (Trans _ _ _) = "transform.h"

getLibs :: Stmt a -> IO [String]
getLibs (Free d@(Decl _ next))    = liftM2 (++) (return $ [getLib d]) (getLibs next)
getLibs (Free t@(Trans _ _ next)) = liftM2 (++) (return $ [getLib t]) (getLibs next)
getLibs (Pure _)                  = return []

new :: Body Int
new = do p <- get
         put (p+1)
         return p

int :: ElemType
int = CInt

proc :: ElemType -> String -> [(ElemType, String)] -> Body () -> Prog ()
proc t name args body = liftF $ Proc t name args body ()

vector :: Int -> [(Int, Int)] -> Body HVector
vector sz elems = do p <- new
                     let vector = Vec p sz elems
                     liftF $ Decl (vector) vector

transform :: Name -> HVector -> Body ()
transform fun col = liftF $ Trans fun col ()

runLibs :: Prog () -> IO [String]
runLibs (Free (Proc t name args body next)) = liftM2 (++) (getLibs (evalStateT body 0)) (runLibs next)
runLibs (Pure _) = return []

interp :: Stmt a -> IO()
interp (Free a@(Decl v next))       = putStrLn (show a) >> interp next
interp (Free t@(Trans fun v next))  = putStrLn (show t) >> interp next
interp (Pure _)    = putStrLn "}"

runInterp :: Prog () -> IO ()
runInterp (Free p@(Proc t name args body next)) = do putStrLn (show p)
                                                     interp (evalStateT body 0)
                                                     runInterp next 
runInterp (Pure _) = putStrLn ""

run :: Prog () -> IO()
run p = do res <- runLibs p
           putStrLn $ concatMap (++"\n") $ nub res
           runInterp p
