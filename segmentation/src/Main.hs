module Main where

{-# LANGUAGE QuasiQuotes #-}
import Python

data NPArray

pyPrefilter :: [Double] -> IO (PyObject NPArray)
pyPrefilter = defVO [str|
import sys
import os
sys.path.append(os.getcwd())

from pybits.segmentation import prefilter

def export(noisy_sound):
    print sys.path
    return prefiter(noisy_sound)
|]

main :: IO ()
main = do
    print "Hello world"
