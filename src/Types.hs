{-# LANGUAGE GADTs , DeriveFunctor #-}

module Types where

import Data.List
import Control.Monad.State
import Control.Monad.Free


{-- BEGIN Datatypes ----------------------------------------------------}
data Expr = Lit Integer 
        | Add Expr Expr 
        | Sub Expr Expr 
        | Mult Expr Expr 
        | Var String

data Args = Args [(ElemType, ID)]

data CFunctor = CFunctor ID ElemType Args Expr

data HVector = Vec Int Int [(Int, Expr)]
  deriving Show
  
data ElemType where
  CInt :: ElemType

data Statement next = Decl HVector next 
                    | Trans Expr HVector next
  deriving (Functor)

newtype CFunc = CFunc CFunctor 

-- Synonyms
type Stmt = Free Statement

type Func = StateT Int Stmt

type ID = String

{-- END Datatypes -------------------------------------------------------}

{-- BEGIN Show Instances ------------------------------------------------}
instance Show Expr where
  show (Add e1 e2)  = "(" ++ show e1 ++ " + " ++ show e2 ++ ")" 
  show (Sub e1 e2)  = "(" ++ show e1 ++ " + " ++ show e2 ++ ")" 
  show (Mult e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")" 
  show (Lit n)      = "(" ++ show n ++ ")"
  show (Var s)      = "(" ++ s ++ ")"

-- TODO add binary_function inheritance
-- Relies on the CFunc Show instance, just replacing
-- the id with operator()
instance Show Args where
  show (Args [])   = ""
  show (Args args) = tail $ foldl (\x y -> x ++ "," ++ y) "" args'
      where args' = map (\(x,y) -> x ++ " " ++ y) cmap
            cmap  = map (\(x,y) -> ("const " ++ x ++ "& ",y)) conv
            conv  = map (\(x,y) -> (show x, y)) args  


instance Show CFunctor where
  show c@(CFunctor id ret args expr) = "struct " ++ id ++ " { " 
                                          ++ show tmp  ++ "};\n"
    where tmp = CFunc (CFunctor "operator()" ret args expr)

instance Show CFunc where
  show (CFunc (CFunctor id ret args expr)) = hostDevDecl
                                             ++ show ret 
                                             ++ id
                                             ++ ("(" ++ show args ++ ")")
                                             ++ ("{\n" 
                                                 ++ "return " 
                                                 ++ show expr ++ ";"
                                                 ++ "}\n")


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


{-- END Show Instances --------------------------------------------------}

instance Num Expr where
  fromInteger n = Lit n
  e1 + e2 = Add e1 e2
  e1 * e2 = Mult e1 e2
  e1 - e2 = Sub e1 e2


hostFuncDecl :: String
hostFuncDecl = "__host__"

devFuncDecl :: String
devFuncDecl = "__device__"

hostDevDecl :: String
hostDevDecl = hostFuncDecl ++ " " ++ devFuncDecl ++ "\n"

