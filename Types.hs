{-# LANGUAGE GADTs, StandaloneDeriving, DeriveFunctor, DeriveDataTypeable #-}

module Types where


import Control.Monad.Free
import Data.Data
import Data.Char

data ElemType where
    CInt :: ElemType

data HVector where
    Vec :: (Show a, Num a) => ElemType -> Int -> [(Int, a)] -> HVector
    
data Language next = 
   Assign Name HVector next 
    | Return 
    | Trans Name Name next
    | Proc ElemType Name [(ElemType, Name)] (Lang next) 
    deriving (Functor)

type Lang = Free Language

type Name = String

type Main = Lang

--data Predicate =
   
    

data Program = Program {
    main_ :: Lang (), 
    procs_ :: [Lang ()],
    libs_ :: [Library]
}

data Library = 
    AdjacentDifference
    | Advance
    | BinarySearch
    -- | ConstantIterator
    | Copy
    | Count
    -- | CountingIterator
    | CudaError
    | DeviceAllocator
    | DeviceDelete
    | DeviceFree
    | DeviceMalloc
    | DeviceMallocAllocator
    | DeviceNew
    | DeviceNewAllocator
    | DevicePtr
    | DeviceReference
    | DeviceVector
    | DeviceBlockEngine
    | DiscardIterator
    | Distance
    | Equal
    | Error
    | ErrorCode
    | Extrema
    | Fill
    | Find
    | ForEach 
    | Functional 
    | Gather 
    | Generate 
    | HostVector
    | InnerProduct
    | IteratorAdaptor
    | IteratorCategories
    | IteratorFacade
    | IteratorTraits
    | LinearCongruentialEngine
    | LinearFeebackShiftEngine
    | Logical
    | Memory
    | Merge
    | NormalDistribution
    | Pair
    | Partition
    -- | PermuationIterator
    | PinnedAllocator
    | Random
    | Reduce
    | Remove 
    | Replace 
    | Retag 
    | Reverse
    | ReverseIterator
    | Scan
    | Scatter 
    | Sequence 
    | SetOperations
    | Sort
    | SubtractWithCarryEngine
    | Swap
    | SystemError
    | Transform
    -- | TransformIterator
    | TransformReduce
    | TransformScan
    | Tuple
    | UniformIntDistribution
    | UniformRealDistribution
    | UninitializedCopy
    | UninitializedFill
    | Unique
    | Version
    | XorCombineEngine
    | ZipIterator
    deriving (Data, Typeable)

instance Show Library where
    show l = [toLower $ head $ show $ toConstr l]
             ++ concatMap (\c -> if (isUpper c) 
                                 then ("_" ++ [toLower c]) 
                                 else ([c]))
                          (tail $ show $ toConstr l)

instance Show ElemType where
    show (CInt) = "int"

instance Show (Language next) where
    show (Assign name (Vec t sz e) next) = "\tthrust::host_vector<" 
                                           ++ show t 
                                           ++ "> " 
                                           ++ name 
                                           ++ "(" 
                                           ++ show sz 
                                           ++ "); \n" 
                                           ++ 
                                           foldr (\(ind, val) acc -> 
                                            ("\t" 
                                             ++ name 
                                             ++ "[" 
                                             ++ show ind 
                                             ++ "] = " 
                                             ++ show val 
                                             ++ "; \n")  
                                             ++ acc) "" e
    show (Return) = "\treturn ARG; \n }\n"
    show (Trans fun arg next) = "\tthrust::transform(" ++ arg ++ ".begin(), " ++ arg ++ ".end(), " ++ fun ++ ");\n"
    show (Proc t name args body)         = show t 
                                           ++ " " 
                                           ++ name 
                                           ++ "(" 
                                           ++ (foldr (\(t, name) acc -> 
                                                 (show t ++ " " ++ name) ++ 
                                                     case acc of
                                                     ")" -> acc
                                                     _ -> ", " ++ acc)
                                                     ")" args) 
                                           ++ "{"


int :: ElemType
int = CInt

