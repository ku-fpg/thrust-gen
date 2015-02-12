{-# LANGUAGE GADTs , DeriveFunctor, StandaloneDeriving, FlexibleInstances,ExistentialQuantification #-}

module Types where

import Data.List
import Control.Monad.State
import Control.Monad.Free

data Expr a where
    D :: Double -> Expr Double
    I :: Integer -> Expr Integer
    Add :: (Expr a) -> (Expr a) -> (Expr a)
    Mult :: (Expr a) -> (Expr a) -> (Expr a)
    Sub :: (Expr a) -> (Expr a) -> (Expr a)
    Var :: String -> Expr String
deriving instance Show (Expr a)

type ID = String

--type Args = [(ElemType, ID)]

{-data CFunctor = CFunctor ID ElemType Args Expr-}

instance Num (Expr Integer) where
    fromInteger n = I n
    lhs + rhs = Add lhs rhs
    lhs * rhs = Mult lhs rhs
    lhs - rhs = Sub lhs rhs
instance Num (Expr Double) where
    fromInteger = D . fromInteger
    lhs + rhs = Add lhs rhs
    lhs * rhs = Mult lhs rhs
    lhs - rhs = Sub lhs rhs

instance Fractional (Expr Double) where
    fromRational n = D (realToFrac n)

{-instance Show Expr where
  show (Add e1 e2)  = "(" ++ show e1 ++ " + " ++ show e2 ++ ")" 
  show (Sub e1 e2)  = "(" ++ show e1 ++ " + " ++ show e2 ++ ")" 
  show (Mult e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")" 
  show (Lit n)      = "(" ++ show n ++ ")"
  show (Var s)      = "(" ++ s ++ ")"

instance Show CFunctor where
  show (CFunctor id ret args expr) = "struct " 
                                     ++ id 
                                     ++ " { \n"
                                 --    ++ (concat $ map (\s -> "\t" ++ s ++ ";\n") args')
                                     ++ "\t"
                                     ++ show ret
                                     ++ " operator()( "
                                     ++ (tail $ foldl (\x y -> x ++ "," ++ y) "" args')
                                     ++ ") {\n \t\treturn"
                                     ++ show expr
                                     ++ "; \n \t}\n };\n"
                                       where args' = map (\(x,y) -> x ++ " " ++ y ) conv
                                             conv  = map (\(x,y) -> (show x, y)) args-}

data HVector a = Vec Int Int [(Int, Expr a)]
  deriving Show
  
data ElemType where
    CInt :: ElemType

data Statement a next = Decl (HVector a) next 
                      | Trans (Expr a) (HVector a) next
    deriving (Functor, Show)

{-instance Show (Statement next) where
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
                                          ++ ");"-}

{-instance Show ElemType where
    show (CInt) = "int"-}

--type Stmt a next = Free (Statement a) next

--type Func a = StateT Int (Stmt a ())
