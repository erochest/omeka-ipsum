{-# LANGUAGE OverloadedStrings #-}


module Main where


import           OmekaIpsum.Options
import           OmekaIpsum.Types


main :: IO ()
main = print =<< execParser options
