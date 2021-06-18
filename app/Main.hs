{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main where

import Language.Haskell.TH (runQ)

import Numeric.AD.TH (decode)

main :: IO ()
main = putStrLn "hello!"

ex1 :: Double -> Double
ex1 = [decode| \x -> sin x |]

ex2 :: Maybe (Integer, Char)
ex2 = [decode| (,) <$> Just 3 <*> Just 'x' |]

ex3 :: Foldable t => t a -> Int
ex3 = [decode| length |]
