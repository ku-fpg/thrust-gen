{-# LANGUAGE GADTs , DeriveFunctor, StandaloneDeriving, FlexibleInstances,ExistentialQuantification #-}

module Types where

import Data.List
import Control.Monad.State
import Control.Monad.Free

data Expr a where
  B     :: Bool     -> Expr Bool
  F     :: Float    -> Expr Float
  C     :: Char     -> Expr Char
  D     :: Double   -> Expr Double
  I     :: Int      -> Expr Int
  Add   :: Expr a   -> Expr a     -> Expr a
  Mult  :: Expr a   -> Expr a     -> Expr a
  Sub   :: Expr a   -> Expr a     -> Expr a
  Var   :: String   -> Expr a

type Name = String
type ReturnType = String

data CFunc a = CFunc { name     :: Name
                     , body     :: Expr a
                     , loc      :: LocationDecl
                     , inherit  :: InheritDecl 
                     , funcType :: FuncType
                     }

-- Declares whether a functor
-- is to be executed on the GPU or CPU
data LocationDecl = HostDecl | DeviceDecl | Both | Neither
data InheritDecl  = None
data FuncType     = Regular | StructBased

retType :: (Expr a) -> String
retType (I _)       = "int"
retType (F _)       = "float"
retType (C _)       = "char"
retType (B _)       = "bool"
retType (Add a _)   = retType a
retType (Mult a _)  = retType a
retType (Sub a _)   = retType a
retType _           = error ""

args :: (CFunc a) -> String
args c = "const " 
         ++ (retType $ (body c))
         ++ "& a," 
         ++ "const "
         ++ (retType $ (body c))
         ++ "& b"

-- TODO lookup thrust decl types
instance Show InheritDecl where
  show l = case l of 
            None -> ""

instance Show LocationDecl where
  show l = case l of 
            HostDecl   -> "__host__"
            DeviceDecl -> "__device__"
            Both       -> "__host__ __device__"
            Neither    -> ""

instance Show (CFunc a) where
  show func = preamble 
              ++ "return " ++ (show $ body func) ++ ";"
              ++ closing
   
    where preamble = case funcType func of
                      StructBased -> "struct " 
                                     ++ (name func)  
                                     ++ (case inherit func of
                                           None -> "")
                                     ++ " " ++ (retType $ body func)
                                     ++ "{\n\toperator()(" 
                                     ++ (args func) 
                                     ++ ") const{\n\t\t"
                      
                      Regular     -> (retType $ body func) 
                                     ++ "("
                                     ++ (args func)
                                     ++ ")" 
                                     ++ " " ++ (name func) ++ "{\n\t"
                                      
          closing =  case funcType func of
                      StructBased -> "\n\t}\n }\n"
                      Regular     -> "\n}\n"


--instance Eq (Expr Bool) where

--instance Ord (Expr Bool) where


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

instance Fractional (Expr Float) where
  fromRational = F . realToFrac 


instance Show (Expr a) where
  show (Add e1 e2)  = "(" ++ show e1 ++ " + " ++ show e2 ++ ")" 
  show (Sub e1 e2)  = "(" ++ show e1 ++ " + " ++ show e2 ++ ")" 
  show (Mult e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")" 
  show (I n)        = show n
  show (D n)        = show n
  show (Var s)      = "(" ++ s ++ ")"

data HVector a where 
  Vec :: Int -> Int -> [(Int, Expr a)] -> HVector a

deriving instance Show (HVector a)

data Statement next where 
  Decl :: HVector a -> next -> Statement next 
  Trans :: CFunc a -> HVector a -> next -> Statement next

instance Functor Statement where
  fmap f (Decl vec next) = Decl vec (f next)
  fmap f (Trans cfunc vec next) = Trans cfunc vec (f next)

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
                                          ++ (show $ body fun)
                                          ++ ");"

type Stmt = Free Statement

type Func = StateT Int Stmt
