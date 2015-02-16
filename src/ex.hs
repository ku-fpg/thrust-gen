import Lang
import Types

main = generate $ do a <- vector 3 [(0,I 5), (1, I 20), (2, I 4)]
                     b <- vector 20 [(0,false), (1, true)]
                     c <- transform b (\x -> x .&& true)
                     cout a
                     d <- transform a (\x -> x + 2)
                     cout d
                     val <- reduce a 0 (\x y -> x + y)
                     return a


{-main = generate $ do a <- vector 20 [(0,I 5), (1, I 20), (2, I 4)]
                     b <- vector 20 [(0,false), (1, true)]
                     c # transform b (\x -> x .&& true)
                     d # transform a (\x -> x + 2)
                     val <- fold a (\x -> x + 1) 0
                     return a
                     -}
