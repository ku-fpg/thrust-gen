module WordCount where

import Types
import Lang

wordCount :: Func ()
wordCount = do res   <- Func DVector
               count <- foldD (\x y -> isAlpha x && isAlpha y) add 0 res
               putStrLn $ show count
