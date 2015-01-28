{-# LANGUAGE GADTs, StandaloneDeriving, DeriveFunctor, DeriveDataTypeable, PatternSynonyms #-}

import Types
import Control.Monad.Free

ret :: Lang()
ret = liftF $ Return

proc :: ElemType -> Name -> [(ElemType, Name)] -> Lang () -> Lang()
proc t str args body = liftF $ Proc t str args body 

(=:) :: Name -> HVector -> Lang ()
(=:) str vec = liftF $ Assign str vec ()

transform :: Name -> Name  -> Lang()
transform fun col = liftF $ Trans fun col ()

unusedImports :: Program -> Bool
unusedImports = error ""

badVarRef :: Program -> Bool
badVarRef = error ""

preamble :: [Library] -> IO ()
preamble = mapM_ (\l -> putStrLn $ "#include <" ++ show l ++ ".h>\n")

libraries :: [Library]
libraries = [DeviceVector, HostVector, Transform]

mainTest :: Lang()
mainTest = do proc int "main" [(int, "x"), (int, "y")]
             $ do "v" =: Vec int 2 [(0,1), (1,2)]
                  "y" =: Vec int 3 []
                  transform "fun" "v"
                  ret

procATest :: Lang ()
procATest = do proc int "procA" [(int, "z")]
             $ do "q" =: Vec int 2 [(0,1), (1,2)]
                  ret

progTest :: Program
progTest = Program { main_ = mainTest, procs_ = [procATest], libs_ = libraries } 

interp :: Program -> IO ()
interp p = do preamble $ libs_ p 
              mapM_ interp_ $ procs_ p 
              interp_ $ main_ p
              return ()

interp_ :: Lang a -> IO()
interp_ (Free a@(Assign _ _ next)) = putStrLn (show a) >> interp_ next
interp_ (Free p@(Proc _ _ _ body)) = putStrLn (show p) >> interp_ body
interp_ (Free t@(Trans _ _ next))  = putStrLn (show t) >> interp_ next
interp_ (Free Return)              = putStrLn $ show Return  
interp_ (Pure _)    = error "This shouldn't happen unless return is omitted"

main :: IO ()
main = do interp progTest
