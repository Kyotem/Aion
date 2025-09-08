-- Fractal Generator (Renderer Module) made by Kyotem
-- Last Edit: 2025-09-08
module Mandlebrot (hasEscaped, calcDelta, genC) where
import Data.Complex (Complex((:+)), magnitude)

-- Function to check if the point has escaped or not (Mandlebrot or Julia)
-- TODO: Convert to escape-time or escape-iteration checker so we can map values over a range
hasEscaped :: Complex Double -> Complex Double -> Int -> Int
hasEscaped c z maxIter
    | magnitude z > 2 = 1  -- Escape condition
    | maxIter == 0 = 0     -- If we reach max iterations, assume it hasn't escaped
    | otherwise = hasEscaped c (z*z + c) (maxIter - 1)  -- TODO: I feel like this isn't calculating per the equations i built before, please cross-ref!!!



calcDelta :: Int -> Double -> Double -> Double
calcDelta u n_min n_max = (n_max - n_min) / fromIntegral (u - 1)

-- ! Is there any use in x_min/max etc here if I'm going to scan over a grid regardless? (The generated grid would define steps, etc)
genC :: Int -> Int -> Double -> Double -> Double -> Double -> Int -> Int -> Complex Double
genC w h x_min x_max y_min y_max px py =
    (x_min + fromIntegral px * calcDelta w x_min x_max)
    :+ (y_max - fromIntegral py * calcDelta h y_min y_max)
-- Using fromIntegral for conversion: https://wiki.haskell.org/Converting_numbers
-- Resolve issues when inputting the parameters (Negative numbers can be seen as numeric litearls whilst it is expecting a double)
-- TODO: Improve type signature & calculations (Somewhere later)
