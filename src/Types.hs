{-# LANGUAGE GADTs , DeriveFunctor, StandaloneDeriving, FlexibleInstances,ExistentialQuantification #-}

module Types where

import Data.List
import Control.Monad.State
import Control.Monad.Free

data Expr a where
  F     :: Float    -> Expr Float
  C     :: Char     -> Expr Char
  D     :: Double   -> Expr Double
  I     :: Int      -> Expr Int
  Add   :: Expr a   -> Expr a     -> Expr a
  Mult  :: Expr a   -> Expr a     -> Expr a
  Sub   :: Expr a   -> Expr a     -> Expr a
  Var   :: String   -> Expr a

--type Args = [(ElemType, ID)]
type Name = String

data CFunctor a b = CFunctor Name (Expr b) (Expr a)

--instance Show CFunctor where
--  show (CFunctor name expr expr) =  


instance Num (Expr Int) where
  fromInteger = I . fromIntegral
  lhs + rhs = Add lhs rhs
  lhs * rhs = Mult lhs rhs
  lhs - rhs = Sub lhs rhs

instance Num (Expr Double) where
  fromInteger = D . fromInteger
  lhs + rhs = Add lhs rhs
  lhs * rhs = Mult lhs rhs
  lhs - rhs = Sub lhs rhs

instance Num (Expr Float) where
  fromInteger = F . fromInteger
  lhs + rhs = Add lhs rhs
  lhs * rhs = Mult lhs rhs
  lhs - rhs = Sub lhs rhs

instance Fractional (Expr Double) where
  fromRational = D . realToFrac

instance Show (Expr a) where
  show (Add e1 e2)  = "(" ++ show e1 ++ " + " ++ show e2 ++ ")" 
  show (Sub e1 e2)  = "(" ++ show e1 ++ " + " ++ show e2 ++ ")" 
  show (Mult e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")" 
  show (I n)        = show n
  show (D n)        = show n
  show (Var s)      = "(" ++ s ++ ")"

{-instance Show CFunctor where
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

data HVector a where 
  Vec :: Int -> Int -> [(Int, Expr a)] -> HVector a

deriving instance Show (HVector a)

data Statement next where 
  Decl :: HVector a -> next -> Statement next 
  Trans :: (Expr a) -> HVector a -> next -> Statement next

instance Functor Statement where
  fmap f (Decl vec next) = Decl vec (f next)
  fmap f (Trans exp vec next) = Trans exp vec (f next)

instance Show (Statement next) where
  show (Decl (Vec ident _ elems) next) = "\tthrust::host_vector<" ++
                                          (case head elems of
                                            (_, I x) -> "int"
                                            (_, D x) -> "double") 
                                          ++ ">v" 
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

type Stmt = Free Statement

type Func = StateT Int Stmt
