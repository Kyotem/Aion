{-|
Module      : Fractal Renderer
Description : Converts a matrix(2D) of data to characters and prints it out
Copyright   : (c) Finn Panhuijsen, 2025
License     : BSD-3-Clause
Maintainer  : FB.Panhuijsen@student.han.nl
Stability   : experimental

Last Edited: 2025-09-08
This module will be adjusted in a later version to implement different kind of rendering methods.
function convertToStar will change in the upcoming version.

-}
module Renderer (convertToStar, printMatrix) where

-- TODO: (for v2) adjust so it can print values from 0 to 1 (float/double) value to measure how far it escaped
-- Or something like 0 - 255 to measure the LPP (Light-Per-Pixel) and map out white to black image

-- Converts the hard values for 0-1 to another character for rendering
-- ! Keep in mind that this will have to be adjusted to support a larger range whenever that's implemented
convertToStar :: Int -> Char
convertToStar x  
    | x == 0 = '*'
    | x == 1 = '-'
    | otherwise = '?'  -- non-mapped values

printMatrix :: [[Int]] -> IO ()
-- Working with monads here? Do some more digging here: https://hoogle.haskell.org/?hoogle=mapM_, need to understand how the function works properly.
printMatrix = mapM_ (putStrLn . map convert)
