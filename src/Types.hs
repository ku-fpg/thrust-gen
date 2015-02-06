{-# LANGUAGE GADTs, KindSignatures, StandaloneDeriving, DeriveFunctor #-}

module Types where

import Control.Monad.State
import Control.Monad.Free

data Expr = Lit Integer | Add Expr Expr | Sub Expr Expr | Mult Expr Expr | Var String
    deriving Show

instance Num Expr where
    fromInteger n = Lit n
    e1 + e2 = Add e1 e2
    e1 * e2 = Mult e1 e2
    e1 - e2 = Sub e1 e2

data HVector = Vec Int Int [(Int, Expr)]
    deriving Show

data ElemType where
    CInt :: ElemType

data Statement next = Decl HVector next | Trans Expr HVector next
    deriving (Functor)


instance Show (Statement next) where
    show (Decl (Vec ident sz elems) next) = "\tthrust::host_vector<type>v" 
                                            ++ (show ident) 
                                            ++ ";\n\t"
                                            ++ concatMap (\(ind, val) -> "v" 
                                                                         ++ (show ident) 
                                                                         ++ "[" 
                                                                         ++ (show ind)
                                                                         ++ "] = "
                                                                         ++ (show val) 
                                                                         ++ ";\n\t") elems
    show (Trans fun (Vec ident sz elems) next) = "\tthrust::transform(v" 
                                                  ++ show ident 
                                                  ++ ".begin(), v" 
                                                  ++ show ident 
                                                  ++ ".end(), " 
                                                  ++ show fun 
                                                  ++ ");"

instance Show ElemType where
    show (CInt) = "int"    

type Stmt = Free Statement

type Func = StateT Int Stmt

