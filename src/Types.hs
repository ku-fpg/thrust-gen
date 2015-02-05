{-# LANGUAGE GADTs, KindSignatures, StandaloneDeriving, DeriveFunctor #-}

module Types where

import Control.Monad.State
import Control.Monad.Free

data HVector = Vec Int Int [(Int, Int)]

data ElemType where
    CInt :: ElemType

data Statement next = Decl HVector next  
                      | Trans String HVector next 
  deriving (Functor)

data Fun next = Proc ElemType Name [(ElemType, Name)] (Body next) next
  deriving (Functor)

instance Show (Statement next) where
    show (Decl (Vec ident sz elems) next) = "\tthrust::host_vector<type>v" 
                                            ++ (show ident) 
                                            ++ ";"
    show (Trans fun (Vec ident sz elems) next) = "\tthrust::transform(v" 
                                                  ++ show ident 
                                                  ++ ".begin(), v" 
                                                  ++ show ident 
                                                  ++ ".end(), " 
                                                  ++ fun 
                                                  ++ ");"

instance Show ElemType where
    show (CInt) = "int"    

instance Show (Fun next) where
    show (Proc t name args body next) = show t 
                                        ++ " " 
                                        ++ name 
                                        ++ "(" 
                                        ++ (foldr (\(t, name) acc -> 
                                              (show t 
                                                ++ " " 
                                                ++ name) 
                                              ++ case acc of
                                                ")" -> acc
                                                _ -> ", " ++ acc)
                                                ")" args) 
                                        ++ "{"

type Name = String

type Stmt = Free Statement

type Body = StateT Int Stmt 

type Prog = Free Fun

