import Lang
import Types

main = generate $ do a <- vector 20 [(0,I 5), (1, I 20), (2, I 4)]
                     return a

