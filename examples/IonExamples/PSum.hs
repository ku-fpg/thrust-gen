import Ion.Base
import Ion.Prelude

main = toThrust $ do v <- vector int [0..100]
                     v  # random <#> (0,9999)
                     sum <- reduce (0,(\x y -> 0 + x + y)) v
                     return v
