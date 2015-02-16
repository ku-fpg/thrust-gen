{-# LANGUAGE GADTs #-} 
{-# LANGUAGE DeriveFunctor #-} 
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where

import Data.Char
import Data.List
import Control.Monad.State
import Control.Monad.Free

{- Synonyms ---------------------------------------------------}
type Name = String
type ReturnType = String
type Stmt = Free Statement
type Func = StateT Int Stmt 
type LibName = String

{- Data Types -------------------------------------------------}
data Expr a where
  -- Supported operation types
  B     :: Bool     -> Expr Bool
  F     :: Float    -> Expr Float
  C     :: Char     -> Expr Char
  D     :: Double   -> Expr Double
  I     :: Int      -> Expr Int

  -- Supported operations
  And   :: Expr a   -> Expr a     -> Expr a
  Or    :: Expr a   -> Expr a     -> Expr a
  Add   :: Expr a   -> Expr a     -> Expr a
  Mult  :: Expr a   -> Expr a     -> Expr a
  Sub   :: Expr a   -> Expr a     -> Expr a
  Var   :: String   -> Expr a

data CFunc a = CFunc { name     :: Name
                     , body     :: Expr a
                     , loc      :: LocationDecl
                     , inherit  :: InheritDecl 
                     , funcType :: FuncType
                     }

data Vector a = HVector { label :: Int
                        , size  :: Int
                        , elems :: [(Int, Expr a)]
                        }
                | DVector { _label :: Int
                          , _size  :: Int
                          , _elems :: [(Int, Expr a)]
                          }
  deriving Show

data Statement next where 
  Decl  :: Vector a -> next -> Statement next 
  Trans :: CFunc a -> Vector a -> next -> Statement next
  Fold  :: CFunc a -> Vector a -> a -> next -> Statement next

-- Declares whether a functor
-- is to be executed on the GPU or CPU
data LocationDecl = HostDecl | DeviceDecl | Both | Neither
data InheritDecl  = None
data FuncType     = Regular | StructBased
data ImportDecl   = Stdlib | Thrust

{- Show Instances -------------------------------------------------}
{- Used for emitting C++ code. Could perhaps be parameterized in
   the future to output code in other languages such as Rust      -}

instance Show (Expr a) where
  show (Add e1 e2)  = "(" ++ show e1 ++ " + " ++ show e2 ++ ")" 
  show (Sub e1 e2)  = "(" ++ show e1 ++ " - " ++ show e2 ++ ")" 
  show (Mult e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")" 
  show (Or e1 e2)   = "(" ++ show e1 ++ " || " ++ show e2 ++ ")"
  show (And e1 e2)  = "(" ++ show e1 ++ " && " ++ show e2 ++ ")"
  show (Var s)      = s
  show (I n)        = show n
  show (B b)        = map toLower $ show b
  show (C c)        = show c
  show (F f)        = show f
  show (D d)        = show d
 
instance Show ImportDecl where
  show Thrust = "thrust/"
  show Stdlib = ""

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
                                     ++ " {\n\t "
                                     ++ (retType $ body func) ++ " operator()(" 
                                     ++ (args2 func) 
                                     ++ ") const{\n\t\t"

                      Regular     -> (retType $ body func) 
                                     ++ "("
                                     ++ (args2 func)
                                     ++ ")" 
                                     ++ " " ++ (name func) ++ "{\n\t"

          closing =  case funcType func of
                      StructBased -> "\n\t}\n };\n"
                      Regular     -> "\n}\n"


instance Show (Statement next) where
  show (Decl (HVector ident sz elems) next) = "\tthrust::host_vector<" ++
                                          (case head elems of
                                            (_, I x) -> "int"
                                            (_, D x) -> "double") 
                                          ++ "> v" 
                                          ++ (show ident) 
                                          ++ "(" ++ show sz ++ ")"
                                          ++ ";\n\t"
                                          ++ concatMap (\(ind, val) -> "v" 
                                            ++ (show ident) 
                                            ++ "[" 
                                            ++ (show ind)
                                            ++ "] = "
                                            ++ (show val) 
                                            ++ ";\n\t") elems

  show (Trans fun (HVector ident _ _) next) = "\tthrust::transform(v" 
                                          ++ show ident 
                                          ++ ".begin(), v" 
                                          ++ show ident 
                                          ++ ".end(), " 
                                          ++ (name fun)
                                          ++ "());\n"
                                          {-++ "[]("
                                          ++ (args fun)
                                          ++ ") { return "
                                          ++ (show $ body fun)
                                          ++ ";}"
                                          ++ ");"-}


{- Num, Ord, Frac Instances -------------------------------------}
{- This allows the Expr types to utilize regular arithmetic and
   boolean operators for a more natural syntax -}
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

instance Eq (Expr Bool) where
  (B b1) == (B b2) = b1 == b2

instance Ord (Expr Bool) where
  (B b1) `compare` (B b2) = b1 `compare` b2

(.&&) :: Expr Bool -> Expr Bool -> Expr Bool
b1 .&& b2 = And b1 b2

instance Fractional (Expr Double) where
  fromRational = D . realToFrac

instance Fractional (Expr Float) where
  fromRational = F . realToFrac 
 
instance Functor Statement where
  fmap f (Decl vec next) = Decl vec (f next)
  fmap f (Trans cfunc vec next) = Trans cfunc vec (f next)

{- Helper functions ---------------------------------------------}
{- Only for use in show instance, not to be exported It may be 
   better to work these into the type more naturally later on -}
retType :: (Expr a) -> String
retType (I _)       = "int"
retType (F _)       = "float"
retType (C _)       = "char"
retType (B _)       = "bool"
retType (Add a b)   = concat $ nub $ [retType a] ++ [retType b]
retType (Mult a b)  = concat $ nub $ [retType a] ++ [retType b]
retType (Sub a b)   = concat $ nub $ [retType a] ++ [retType b]
retType (And a b)   = concat $ nub $ [retType a] ++ [retType b]
retType (Or a b)    = concat $ nub $ [retType a] ++ [retType b]
retType (Var a)     = ""
retType _           = error "Unknown expr value"

-- TODO work on this
-- Need to clean up multi arg lambdas
args2 :: (CFunc a) -> String
args2 fn = "const " ++ retType b ++ ", "
           ++ "const " ++ retType b 
  where b = body fn
