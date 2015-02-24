import Lang
import Types
import Data.Complex
import Data.Boolean

main = toThrust $ do a <- vector int [0..10]
                     f <- vector int [0,2,3,5] 
                     b <- vector bool [False, True]
                     c <- vector complex [2 :+ 3, 3 :+ 5]
                     d <- load a 
                     a # transform <#> (\x -> x + 2)
                     b # transform <#> (\x -> x &&* true)
                     a # transform <#> (\x -> x + 2)
