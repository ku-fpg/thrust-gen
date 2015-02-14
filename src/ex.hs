import Prelude hiding ((>))
import Lang
import Types
import Control.Monad.Free

foo :: Func (HVector Int)
foo = do a <- vector 20 [(0,5), (1, 20), (2, 4)]
         return a

main = run (prog 0 1)
