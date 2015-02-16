import Lang
import Types

main = generate $ do a <- vector 20 [(0,I 5), (1, I 20), (2, I 4)]
                     b <- vector 20 [(0,B False), (1, B True)]
                     c <- transform b (\x -> x .&& B True)
                     return a
