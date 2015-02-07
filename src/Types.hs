{-# LANGUAGE GADTs , DeriveFunctor #-}

module Types where

import Control.Monad.State
import Control.Monad.Free

data Expr = Lit Integer 
        | Add Expr Expr 
        | Sub Expr Expr 
        | Mult Expr Expr 
        | Var String

instance Num Expr where
  fromInteger n = Lit n
  e1 + e2 = Add e1 e2
  e1 * e2 = Mult e1 e2
  e1 - e2 = Sub e1 e2

instance Show Expr where
  show (Add e1 e2)  = "(" ++ show e1 ++ " + " ++ show e2 ++ ")" 
  show (Sub e1 e2)  = "(" ++ show e1 ++ " + " ++ show e2 ++ ")" 
  show (Mult e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")" 
  show (Lit n)      = "(" ++ show n ++ ")"
  show (Var s)      = "(" ++ s ++ ")"

data HVector = Vec Int Int [(Int, Expr)]
    deriving Show

data ElemType where
    CInt :: ElemType

data Statement next = Decl HVector next 
                    | Trans Expr HVector next
    deriving (Functor)


instance Show (Statement next) where
  show (Decl (Vec ident _ elems) next) = "\tthrust::host_vector<type>v" 
                                          ++ (show ident) 
                                          ++ ";\n\t"
                                          ++ concatMap (\(ind, val) -> "v" 
                                            ++ (show ident) 
                                            ++ "[" 
                                            ++ (show ind)
                                            ++ "] = "
                                            ++ (show val) 
                                            ++ ";\n\t") elems

  show (Trans fun (Vec ident _ _) next) = "\tthrust::transform(v" 
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
