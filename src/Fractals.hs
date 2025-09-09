{-|
Module      : Fractal Calculator
Description : Used to calculate Julia & Mandelbrot sets
Copyright   : (c) Finn Panhuijsen, 2025
License     : BSD-3-Clause
Maintainer  : FB.Panhuijsen@student.han.nl
Stability   : experimental

Last Edited: 2025-09-08
Module has to be adjusted for:
- Optimization
- Less duplicate code

-}
module Fractals (hasEscaped, calcDelta, genComplex, generateMandelbrot, generateJulia) where
import Data.Complex (Complex((:+)), magnitude)

calcDelta :: Int -> Double -> Double -> Double
calcDelta u n_min n_max = (n_max - n_min) / fromIntegral (u - 1)

-- Function to check if the point has escaped or not (Mandlebrot or Julia)
-- TODO: Convert to escape-time or escape-iteration checker so we can map values over a range
-- TODO: Calculate manually to confirm it's working properly (It's giving good results but precision is questionable)
hasEscaped :: Complex Double -> Complex Double -> Int -> Int
hasEscaped c z0 maxIter = iterate z0 0 -- Defining another function inside of this one just for clarity & tracking the iteration
    where
        iterate z iter
            | magnitude z > 2 = iter  -- Escape condition
            | iter >= maxIter = maxIter     -- If we reach max iterations, assume it hasn't escaped
            | otherwise = iterate (z*z + c) (iter + 1)  -- TODO: I feel like this isn't calculating per the equations i built before, please cross-ref!!!

-- ! Is there any use in x_min/max etc here if I'm going to scan over a grid regardless? (The generated grid would define steps, etc)
genComplex :: Int -> Int -> Double -> Double -> Double -> Double -> Int -> Int -> Complex Double
genComplex w h x_min x_max y_min y_max px py =
    (x_min + fromIntegral px * calcDelta w x_min x_max)
    :+ (y_max - fromIntegral py * calcDelta h y_min y_max)
-- Using fromIntegral for conversion: https://wiki.haskell.org/Converting_numbers
-- Resolve issues when inputting the parameters (Negative numbers can be seen as numeric litearls whilst it is expecting a double)
-- TODO: Improve type signature & calculations (Somewhere later)

-- Generate the grid of escape values
-- ? Should this be moved to Renderer.hs?
generateMandelbrot :: Int -> Int -> Double -> Double -> Double -> Double -> Int -> (Complex Double -> Complex Double -> Int -> Int) -> [[Int]]
generateMandelbrot w h x_min x_max y_min y_max maxIter f = 
    [ [ f (genComplex w h x_min x_max y_min y_max px py) 0  maxIter | px <- [0..w-1] ] | py <- [0..h-1] ]
    

-- FIXME: Function to generate Julia set grid (Duplicate code, please refactor!!)
generateJulia :: Complex Double -> Int -> Int -> Double -> Double -> Double -> Double -> Int -> (Complex Double -> Complex Double -> Int -> Int) -> [[Int]]
generateJulia c w h x_min x_max y_min y_max maxIter f =
    [ [ f c (genComplex w h x_min x_max y_min y_max px py) maxIter | px <- [0..w-1] ] | py <- [0..h-1] ]
{-
    f == Fractals.hasEscaped (Can later be converted to map to a specific range)
    - https://wiki.haskell.org/List_comprehension
    - https://www.youtube.com/watch?v=dYKQSWtJv-w

    px <- [0..w-1] (Inner list)
    py <- [0..h-1] (Outer list)

    0..w-1 = the range (Start at 0 until w - 1)
    
    Left side expression uses the function and generates the value to put INTO the list at that position
    Left side expression uses px and py generated on the right side of the expression in the list comprehension
-}