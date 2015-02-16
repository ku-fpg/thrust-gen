import Lang
import Types

true :: Expr Bool
true = B True

false :: Expr Bool
false = B False

main = generate $ do a <- vector 20 [(0,I 5), (1, I 20), (2, I 4)]
                     b <- vector 20 [(0,false), (1, true)]
                     c <- transform b (\x -> x .&& true)
                     d <- transform a (\x -> x + 2)
                     return a
