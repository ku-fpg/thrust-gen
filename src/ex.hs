import Lang
import Types

-- example of defining useful libs in terms of exp
all x = reduce x true (\x y -> x .&& y)
any x = reduce x false (\x y -> x .|| y)

main = toThrust $ do a <- vector 20 [(0,5), (1, 20), (2, 4::Expr Int)]
                     b <- vector 20 [(0,false), (1, true)]
                     c <- transform b (\x -> x .&& true)
                     d <- transform a (\x -> x + 2)
                     check <- any x
                     cout d
                     return a


{-main = toThrust $ do a <- vector 20 [(0,I 5), (1, I 20), (2, I 4)]
                     b <- vector 20 [(0,false), (1, true)]
                     c # transform b (\x -> x .&& true)
                     d # transform a (\x -> x + 2)
                     val <- fold a (\x -> x + 1) 0
                     return a
                     -}
