{-# LANGUAGE LambdaCase #-}
import Test.Tasty
import Test.DocTest

import Control.Exception
import System.Exit

import Variant
import EADT

main :: IO ()
main = wrapTests
   [ title "TASTY"   $ defaultMain tastyTests
   , title "DOCTEST" $ doctest ["src/"]
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

tastyTests :: TestTree
tastyTests = testGroup "utils-variant"
   [ testsVariant
   , testsEADT
   ]

