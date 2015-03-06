import Ion.Base
import Ion.Prelude

main :: IO ()
main = toThrust $ do a <- initVector int 50000
                     d <- load a
                     d # transform <#> (\x -> x + 2)
                     c <- unload d
                     cout c
                     return c 
