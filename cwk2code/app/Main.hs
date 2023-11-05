module Main (main) where

import Lib

testTree::Tree Int String
testTree = empty

main :: IO ()
main = do
    let x = isEmpty testTree in print x