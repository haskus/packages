{-# LANGUAGE LambdaCase #-}
import Test.DocTest (mainFromCabal)
import System.Environment (getArgs)

import Control.Exception
import System.Exit

main :: IO ()
main = wrapTests
   [ title "DOCTEST" $ mainFromCabal "haskus-utils-types" =<< getArgs
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
