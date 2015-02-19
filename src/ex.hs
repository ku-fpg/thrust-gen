import Lang
import Types

-- example of defining useful libs in terms of exp
all_ x = reduce x true (\x y -> x .&& y)
any_ x = reduce x false (\x y -> x .|| y)
or_ b x = transform x (\x -> x .&& b)

infixr 4 #
v # (fn,expr) = fn v expr  

infixr 6 <#>
(<#>) a b = (a,b)

main = toThrust $ do a <- vector 3 [(0, 5), (1, 20), (2, 4)] :: Ion(Vector Int)
                     b <-  vector 2 [(0,false), (1, true)]
                     d <- load a
                     d # transform <#> (\x -> x + 2)
                     b # transform <#> (\x -> x .&& true)
                     a # transform <#> (\x -> x + 2)
                     return a


{-main = toThrust $ do a <- vector 20 [(0,I 5), (1, I 20), (2, I 4)]
                     b <- vector 20 [(0,false), (1, true)]
                     c # transform b (\x -> x .&& true)
                     d # transform a (\x -> x + 2)
                     val <- fold a (\x -> x + 1) 0
                     return a
                     -}
