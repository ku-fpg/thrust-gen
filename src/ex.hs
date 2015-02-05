import Lang
import Types
import Data.List

test :: Prog ()
test = do proc int "test" [(int, "x")] $ do a <- vector 5 []
                                            b <- vector 10 [(5,5)]
                                            transform (\x -> x + 1) a
                                            return ()
          proc int "main" [] $ do c <- vector 1 []
                                  return ()
          return ()

          proc int "main" [] 
            $ do  c <- vector 1 []
                  return ()
          
          return ()

main = run test
