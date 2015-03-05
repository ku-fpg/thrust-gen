import Ion.Base
import Ion.Prelude

main :: IO ()
main = toThrust $ do a <- vector int [0..10]
                     d <- load a
                     d # transform <#> (\x -> x + 2)
                     return a
                     {-- Fix 
                     -  This is now caught by our types 
                     -  a <- vector int [0..10]
                     -  a # transform <#> (\x -> x + 2)
                     -}
