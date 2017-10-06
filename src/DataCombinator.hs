{-# LANGUAGE OverloadedStrings #-}
module DataCombinator where

import Data.Text as Text
import Data.Function ((&))

import System.Directory

import DataCombinator.Audio
import DataCombinator.Numeric

combineAll :: String -> IO ()
combineAll datadir = do
    files <- listDirectory datadir
    let headers = files
                & fmap (getPrefix . Text.pack)
                & Prelude.filter (/= "")
                & fmap Text.unpack
    Prelude.mapM_ (combineCSV datadir) headers
    Prelude.mapM_ (combineFLAC datadir) headers
  where
    getPrefix t
        = case Text.breakOn "acc" t of
            (x , "acc00.csv") -> x
            (_, _) -> ""
