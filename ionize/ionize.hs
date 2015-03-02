{-# LANGUAGE OverloadedStrings #-}

module Main where

import Ion.Base
import Ion.Prelude
import Turtle
import Data.Char
import Data.Text (pack)
import System.Environment (getArgs)

helpMessage = echo $ "Copyright 2015 (c), University of Kansas\n"
                   <> "A CLI tool for using the Ion Library\n"
                   <> "Usage: ./ionize [mode] file\n" 
                   <> "where [mode] is run, build or help" 

data Mode = Run | Build | Help

toMode s = case map toLower s of
             "run"     -> Run
             "build"   -> Build
             "help"    -> Help
             otherwise -> error "Invalid mode selected, type either run or build"

nvcc dir file = proc "nvcc" [(dir <> "/" <> file)] empty 

runhs dir file = proc "runhaskell" [(dir <> "/" <> file)] ""

getFext = reverse . takeWhile (/= '.') . reverse

dropFext = takeWhile (/= '.') 

action Run   = shell ("./ionize-build/ion-gen") empty
action Build = shell "echo \"generated files in ionize-build\"" empty 

main = do args <- getArgs
          let hsFile'  = args !! 1 
              hsFile   = pack hsFile'
              baseFile = dropFext hsFile'
              cuFile'  = baseFile <> ".cu"
              cuFile   = pack cuFile'
              runMode  = args !! 0
              ionDir   = "ionize-build"
              ionExe   = "ion-gen"
              runhs    = "runhaskell " <> hsFile <> " > " <> ionDir <> "/" <> cuFile 
          
          case (toMode $ runMode) of
            Help  -> do helpMessage
                        die ""

            md    -> do case (getFext $ hsFile') of
                          "hs"  -> liftIO $ echo "Compiling hs file"
                          _     -> die "Error, arg1 must be haskell file"

                        mktree "ionize-build"
                        shell runhs empty
                        echo "Compiling cu file"
                        nvcc ionDir cuFile 
                        mv "a.out" "./ionize-build/ion-gen"
                        action md 
                          
