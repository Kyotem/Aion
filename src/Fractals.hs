-- Fractal Generator (Renderer Module) made by Kyotem
-- Last Edit: 2025-09-08
module Fractals (hasEscaped, calcDelta, genC, generateGrid, generateJulia) where
import Data.Complex (Complex((:+)), magnitude)


calcDelta :: Int -> Double -> Double -> Double
calcDelta u n_min n_max = (n_max - n_min) / fromIntegral (u - 1)

-- Function to check if the point has escaped or not (Mandlebrot or Julia)
-- TODO: Convert to escape-time or escape-iteration checker so we can map values over a range
-- TODO: Calculate manually to confirm it's working properly (It's giving good results but precision is questionable)
hasEscaped :: Complex Double -> Complex Double -> Int -> Int
hasEscaped c z maxIter
    | magnitude z > 2 = 1  -- Escape condition
    | maxIter == 0 = 0     -- If we reach max iterations, assume it hasn't escaped
    | otherwise = hasEscaped c (z*z + c) (maxIter - 1)  -- TODO: I feel like this isn't calculating per the equations i built before, please cross-ref!!!

-- ! Is there any use in x_min/max etc here if I'm going to scan over a grid regardless? (The generated grid would define steps, etc)
genC :: Int -> Int -> Double -> Double -> Double -> Double -> Int -> Int -> Complex Double
genC w h x_min x_max y_min y_max px py =
    (x_min + fromIntegral px * calcDelta w x_min x_max)
    :+ (y_max - fromIntegral py * calcDelta h y_min y_max)
-- Using fromIntegral for conversion: https://wiki.haskell.org/Converting_numbers
-- Resolve issues when inputting the parameters (Negative numbers can be seen as numeric litearls whilst it is expecting a double)
-- TODO: Improve type signature & calculations (Somewhere later)




-- Generate the grid of escape values
generateGrid :: Int -> Int -> Double -> Double -> Double -> Double -> Int -> (Complex Double -> Complex Double -> Int -> Int) -> [[Int]]
generateGrid w h x_min x_max y_min y_max maxIter f = 
    [ [ f (genC w h x_min x_max y_min y_max px py) 0  maxIter | px <- [0..w-1] ] | py <- [0..h-1] ]
    

cJulia :: Complex Double
cJulia = (-0.7) :+ 0.27015

-- FIXME: Function to generate Julia set grid (Duplicate code, please refactor!!)
generateJulia :: Int -> Int -> Double -> Double -> Double -> Double -> Int -> (Complex Double -> Complex Double -> Int -> Int) -> [[Int]]
generateJulia w h x_min x_max y_min y_max maxIter f =
    [ [ f cJulia (genC w h x_min x_max y_min y_max px py) maxIter
        | px <- [0..w-1] ]
      | py <- [0..h-1] ]
{-
    f == Mandlebrot.hasEscaped (Can later be converted to map to a specific range)
    - https://wiki.haskell.org/List_comprehension
    - https://www.youtube.com/watch?v=dYKQSWtJv-w

    px <- [0..w-1] (Inner list)
    py <- [0..h-1] (Outer list)

    0..w-1 = the range (Start at 0 until w - 1)
    
    Left side expression uses the function and generates the value to put INTO the list at that position
    Left side expression uses px and py generated on the right side of the expression in the list comprehension
-}