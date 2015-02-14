import Lang
import Types

prog = do a <- vector 20 [(0,5), (1, 20), (2, 4)]
          return a

main = run (prog 0 1)
