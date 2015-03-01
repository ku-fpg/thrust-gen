{-# LANGUAGE GADTs #-} 
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE DeriveFunctor #-} 
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Ion.Private.Types where

import Data.Char
import Data.List
import Data.Complex
import Data.Boolean
import Control.Monad.State
import Control.Monad.Free

{- Synonyms ---------------------------------------------------}
type Name = String
type ReturnType = String
type Stmt = Free Statement
type Ion = StateT Int Stmt 
type LibName = String

{- Data Types -------------------------------------------------}
data Expr a where
  -- Supported primitive types
  B     :: Bool           -> Expr Bool
  F     :: Float          -> Expr Float
  C     :: Char           -> Expr Char
  D     :: Double         -> Expr Double
  I     :: Int            -> Expr Int
  Cx    :: Complex Double -> Expr (Complex Double)

  -- Supported operations
  And   :: Expr a   -> Expr a -> Expr a
  Or    :: Expr a   -> Expr a -> Expr a
  Not   :: Expr a   -> Expr a
  Add   :: Expr a   -> Expr a -> Expr a
  Mult  :: Expr a   -> Expr a -> Expr a
  Sub   :: Expr a   -> Expr a -> Expr a
  Var   :: String   -> Expr a

  -- Conditional Logic
  While :: Expr Bool -> Expr a -> Expr a
  If    :: Expr Bool -> Expr a -> Expr a -> Expr a
  
data CFunc a = CFunc { name     :: Name
                     , body     :: Expr a
                     , loc      :: LocationDecl
                     , inherit  :: InheritDecl 
                     , funcType :: FuncType
                     , numArgs  :: Int
                     }

data Vector a = HVector   { label :: String
                          , size  :: Int
                          , elems :: [(Int, Expr a)]
                          }

               | DVector  { _label :: String
                          , _size  :: Int
                          , _elems :: [(Int, Expr a)]
                          }
  deriving Show

data Statement next where 
  Decl  :: Vector a -> next -> Statement next 
  Trans :: CFunc a -> Vector a -> next -> Statement next
  Cout  :: Vector a -> next -> Statement next
  Fold  :: (Show a) => Name -> CFunc a -> Vector a -> Expr a -> next -> Statement next
  Load  :: Vector a -> next -> Statement next
  FoldD :: (Show a) => Name -> CFunc a -> CFunc a -> Vector a -> next -> Statement next
  Sort  :: Vector a -> next -> Statement next
  AdjDiff :: Vector a -> next -> Statement next

-- Declares whether a functor
-- is to be executed on the GPU or CPU
data LocationDecl = HostDecl | DeviceDecl | Both | Neither
data InheritDecl  = None
data FuncType     = Regular | StructBased
data ImportDecl   = Stdlib | Thrust | Cuda

{- Conversion Instances ------------------------------------------}
class ToExpr a where
  toExpr :: a -> Expr a

instance ToExpr Bool where
  toExpr = B 

instance ToExpr Int where
  toExpr = I

instance ToExpr Float where
  toExpr = F

instance ToExpr Double where
  toExpr = D

instance ToExpr (Complex Double) where
  toExpr = Cx


{- Show Instances -------------------------------------------------}
{- Used for emitting C++ code. Could perhaps be parameterized in
   the future to output code in other languages such as Rust      -}

instance  Show (Expr a) where
  show (Add e1 e2)  = "(" ++ show e1 ++ " + " ++ show e2 ++ ")" 
  show (Sub e1 e2)  = "(" ++ show e1 ++ " - " ++ show e2 ++ ")" 
  show (Mult e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")" 
  show (Or e1 e2)   = "(" ++ show e1 ++ " || " ++ show e2 ++ ")"
  show (And e1 e2)  = "(" ++ show e1 ++ " && " ++ show e2 ++ ")"
  show (Not e1)     = "(!" ++ show e1 ++ ")"
  show (Var s)      = s
  show (I n)        = show n
  show (B b)        = map toLower $ show b
  show (C c)        = show c
  show (F f)        = show f
  show (D d)        = show d
  show (Cx c)       = "Complex(" 
                      ++ (show $ realPart c) ++ "," 
                      ++ (show $ imagPart c) ++ ")"
 
instance Show ImportDecl where
  show Thrust = "thrust/"
  show Stdlib = ""
  show Cuda   = "" 

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
                                     ++ " {\n\t"
                                     ++ (retType $ body func) ++ " operator()(" 
                                     ++ (case numArgs func of 
                                          1 -> args func
                                          2 -> args2 func
                                          3 -> args3 func) 
                                     ++ ") const{\n\t\t"

                      Regular     -> (retType $ body func) 
                                     ++ "("
                                     ++ (case numArgs func of
                                          1 -> args func
                                          2 -> args2 func
                                          3 -> args3 func) 
                                     ++ ")" 
                                     ++ " " ++ (name func) ++ "{\n\t"

          closing =  case funcType func of
                      StructBased -> "\n\t}\n};\n"
                      Regular     -> "\n}\n"


iters :: String -> (String,String)
iters ident = (ident ++ ".begin()", ident ++ ".end()")

instance Show (Statement next) where
  show (Decl (HVector ident sz elems) next) = "\tthrust::host_vector<"
                                          ++ (retType $ snd $ head elems) 
                                          ++ "> " 
                                          ++ ident 
                                          ++ "(" ++ show sz ++ ")"
                                          ++ ";\n\t"
                                          ++ concatMap (\(ind, val) -> ident
                                            ++ "[" 
                                            ++ (show ind)
                                            ++ "] = "
                                            ++ (show val) 
                                            ++ ";\n\t") elems

  show (Load (DVector ident sz elems) next) = "\tthrust::device_vector<"
                                          ++ (retType $ snd $ head elems)
                                          ++ "> "
                                          ++ ident
                                          ++ " = v"
                                          ++ drop 1 ident
                                          ++ ";\n"

  show (Sort (HVector ident sz elems) next) = "\tthrust::sort("
                                              ++ (concat $ intersperse "," $
                                                 [ (fst $ iters ident),
                                                   (snd $ iters ident)]) 
                                              ++ ");"

  show (Sort (DVector ident sz elems) next) = show $ Sort (HVector ident sz elems) next

  show (AdjDiff (HVector ident sz elems) next) = "\tthrust::adjacent_difference("
                                                 ++ (concat $ intersperse "," $
                                                 [ (fst $ iters ident),
                                                   (snd $ iters ident),
                                                   (fst $ iters ident)]) 

  show (Trans fun (HVector ident _ _) next) = "\tthrust::transform(" 
                                              ++ (concat $ intersperse "," $
                                                 [ (fst $ iters ident),
                                                   (snd $ iters ident),
                                                   (fst $ iters ident)])  
                                              ++ "," ++ (name fun)
                                              ++ "());"

  show (Trans f (DVector i a b) next) = show $ Trans f (HVector i a b) next

  show (Cout (HVector ident sz elems) next) = "\n\tfor (int i = 0; i < " 
                                                ++ show sz 
                                                ++ "; ++i){std::cout << "
                                                ++ ident
                                                ++ "[i] << \" \";}\n"
                                                ++ "\tstd::cout << std::endl;"

  show (Fold to fun (HVector ident _ elems) init _) =  "\t" ++ (retType $ snd $ head elems)
                                                          ++ " "
                                                          ++ to 
                                                          ++ " = "
                                                          ++ "thrust::reduce("
                                                          ++ (fst $ iters ident) ++ ", "
                                                          ++ (snd $ iters ident) ++ ", "
                                                          ++ (show init) ++ ", "
                                                          ++ (name fun) ++ "());"

{- Num, Ord, Frac Instances -------------------------------------}
{- This allows the Expr types to utilize regular arithmetic and
   boolean operators for a more natural syntax -}
instance Num (Expr Int) where
  fromInteger = I . fromIntegral
  lhs + rhs = Add lhs rhs
  lhs * rhs = Mult lhs rhs
  lhs - rhs = Sub lhs rhs
  signum (I v) = (I . signum) v
  abs (I v)   = (I . abs) v

instance Num (Expr Double) where
  fromInteger = D . fromInteger
  lhs + rhs = Add lhs rhs
  lhs * rhs = Mult lhs rhs
  lhs - rhs = Sub lhs rhs
  signum (D v) = (D . signum) v
  abs (D v)   = (D . abs) v

instance Num (Expr Float) where
  fromInteger = F . fromInteger
  lhs + rhs = Add lhs rhs
  lhs * rhs = Mult lhs rhs
  lhs - rhs = Sub lhs rhs
  signum (F v) = (F . signum) v
  abs (F v)   = (F . abs) v

instance Eq (Expr a) where
  (Add a1 b1) == (Add a2 b2) = a1 == a2 && b1 == b2
  (Sub a1 b1) == (Sub a2 b2) = a1 == a2 && b1 == b2
  (I i1) == (I i2) = i1 == i2
  (B b1) == (B b2) = b1 == b2
  _ == _ = False

instance Ord (Expr Bool) where
  (B b1) `compare` (B b2) = b1 `compare` b2

instance Ord (Expr Int) where
  (I i1) `compare` (I i2) = i1 `compare` i2

instance Fractional (Expr Double) where
  fromRational = D . realToFrac
  recip = error "Undefined operation" 
  (/)   = error "Undefined operation"

instance Fractional (Expr Float) where
  fromRational = F . realToFrac 
  recip = error "Undefined operation" 
  (/)   = error "Undefined operation"

instance Functor Statement where
  fmap f (Decl vec next) = Decl vec (f next)
  fmap f (Load vec next) = Load vec (f next)
  fmap f (Trans cfunc vec next) = Trans cfunc vec (f next)
  fmap f (Cout v next) = Cout v (f next) 
  fmap f (Fold to cfunc vec val next) = Fold to cfunc vec val (f next)
  fmap f (Sort v next) = Sort v (f next)
  fmap f (AdjDiff v next) = AdjDiff v (f next)

{- Convenience operators for (Expr) bool's  -}
instance Boolean (Expr Bool) where
  b1 &&* b2 = And b1 b2
  b1 ||* b2 = Or b1 b2
  notB b1 = Not b1
  true = B True
  false = B False

{--
(<*) :: Expr Int -> Expr Int -> Expr Bool
(I i1) .< (I i2) = B $ i1 < i2

(.>) :: Expr Int -> Expr Int -> Expr Bool
(I i1) .> (I i2) = B $ i1 > i2

(.<=) :: Expr Int -> Expr Int -> Expr Bool
(I i1) .<= (I i2) = B $ i1 <= i2

(.=>) :: Expr Int -> Expr Int -> Expr Bool
(I i1) .=> (I i2) = B $ i1 >= i2
--}

{- Helper functions ---------------------------------------------}
{- Only for use in show instance, not to be exported It may be 
   better to work these into the type more naturally later on -}
retType :: (Expr a) -> String
retType (I _)       = "int"
retType (F _)       = "float"
retType (D _)       = "double"
retType (C _)       = "char"
retType (B _)       = "bool"
retType (Cx _)      = "complex<double> " -- Add space for C++98 compilers
retType (Add a b)   = concat $ nub $ [retType a] ++ [retType b]
retType (Mult a b)  = concat $ nub $ [retType a] ++ [retType b]
retType (Sub a b)   = concat $ nub $ [retType a] ++ [retType b]
retType (And a b)   = concat $ nub $ [retType a] ++ [retType b]
retType (Or a b)    = concat $ nub $ [retType a] ++ [retType b]
retType (Not a)     = concat $ nub $ [retType a]
retType (Var a)     = ""


idents :: (Expr a) -> [String]
idents body = case body of 
                (Var a)     -> [a]
                (Add a b)   -> idents a ++ idents b
                (Or  a b)   -> idents a ++ idents b
                (And a b)   -> idents a ++ idents b
                (Not a )    -> idents a
                (Mult a b)  -> idents a ++ idents b
                (Sub a b)   -> idents a ++ idents b
                _           -> []

args :: (CFunc a) -> String
args fn = "const " ++ retType b ++ " " ++ (idents b !! 0)
  where b = body fn

args2 :: (CFunc a) -> String
args2 fn = "const " ++ retType b ++ " " ++ (idents b !! 0) ++ ", "
           ++ "const " ++ retType b ++ " " ++ (idents b !! 1) 
  where b = body fn

args3 :: (CFunc a) -> String
args3 fn = "const " ++ retType b ++ " " ++ (idents b !! 0) ++ ", "
           ++ "const " ++ retType b ++ " " ++ (idents b !! 1) ++ ", "
           ++ "const " ++ retType b ++ " " ++ (idents b !! 2)
  where b = body fn

