import Ion.Base
import Ion.Prelude

main :: IO ()
main = toThrust $ do f    <- vector int [1..5]
                     cout f
                     
                     --empty vector to store histogram counts in
                     z    <- vector int (take 6 $ cycle [0])
                     a    <- countingiterator int 0
                     res  <- upperbound f z a

                     --actual histogram value
                     res' <- adjdiff res
                     cout res'
                     return f
