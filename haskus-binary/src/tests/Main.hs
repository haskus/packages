{-# LANGUAGE LambdaCase #-}

import Test.Tasty
import Test.DocTest (mainFromCabal)
import System.Environment (getArgs)

import Control.Exception
import System.Exit


import Haskus.Tests.Format.Binary

main :: IO ()
main = wrapTests
   [ title "TASTY"   $ defaultMain testsBinary
   , title "DOCTEST" $ mainFromCabal "src/lib" =<< getArgs
   ]

title :: String -> IO () -> IO ()
title s m = do
   putStrLn ""
   putStrLn (replicate 30 '=')
   putStrLn s
   putStrLn (replicate 30 '=')
   m

wrap :: IO () -> IO Bool
wrap m = (m >> return True) `catch` (\e -> return (e == ExitSuccess))

wrapTests :: [IO ()] -> IO ()
wrapTests ts = (and <$> traverse wrap ts) >>= \case
   True  -> title "SUMMARY" exitSuccess
   False -> title "SUMMARY" exitFailure
