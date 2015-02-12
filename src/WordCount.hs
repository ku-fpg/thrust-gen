module WordCount where

import Types
import Lang

-- wordCountEx
--

isAlpha :: Expr -> Expr
isAlpha = (\x -> (x > 'A') && (x < 'z')) 

delim :: Expr -> Expr -> Expr
delim = (\x y -> isAlpha x && isAlpha y)

accum :: Expr -> Expr 
accum :: (\x -> x + 1)

wordCount :: Func ()
wordCount = do res   <- Func DVector
               count <- foldD delim accum 0 res
               putStrLn $ show count
