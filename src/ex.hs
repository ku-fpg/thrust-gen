import Ion.Base
import Ion.Prelude

main :: IO ()
main = toThrust $ do --a <- vector int [0..10]
                     f <- vector int [0,2,3,5]
                     cout f
                     z <- vector int (take 6 $ cycle [0])
                     --d <- load f
                     {-sort f 
                     cout f
                     sort d
                     adjdiff f
                     cout f
                     adjdiff d-}
                     a <- countingiterator int 0
                     res <- upperbound f z a
                     res' <- adjdiff res
                     cout res'
                     {-b <- vector bool [False, True]
                     c <- vector complex [2 :+ 3, 3 :+ 5]
                     d <- load a 
                     a # transform <#> (\x -> x + 2)
                     b # transform <#> (\x -> x &&* true)
                     a # transform <#> (\x -> x + 2)
                     g <- reduce all_ b-}
                     return f
