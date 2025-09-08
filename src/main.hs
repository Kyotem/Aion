-- Fractal Generator made by Kyotem
-- Last Edit: 2025-09-08
import Mandlebrot
import Renderer

{- 
!!! Haskell has native support for complex numbers !!!
https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Complex.html
Can make your calculations MUCH more readable without needing to convert to plain arithm.
-}
-- Required to import functions next to the namespace import / (Module)
import Data.Complex (Complex((:+)), realPart, imagPart, magnitude)


-- Signature works like: Param1 -> Param2 -> Return type
escapesToInf :: Complex Double -> Complex Double -> Int -> Complex Double

-- TODO: calculate equation manually and check if function is being calculated properly (Seems to return 3 in some cases where it LIKELY shouldn't but unsure)
-- Based on: https://www.futurelearn.com/info/courses/functional-programming-haskell/0/steps/27226 (Reference)
-- ! TODO: Can this singular function be used for both Julia & Mandlebrot sets? (Figure out)
-- n = z_0 | c = constant | d = iteration depth
escapesToInf n c d -- C1: Recursion
    | d < 0 = n -- Base Case
    | magnitude n > 2 = n -- If escapes into infinity, return 3 (TODO: Create better handling for this)
    | otherwise = escapesToInf (n*n + c) c (d-1) -- Recursive iteration to check whether or not n(z_n) escapes into infinity

-- Recommended depth is around d=500 to check for infinity-escape (.. Source? Read it somewhere... TODO: Cite source)


-- -- Created separate func, check if z > 2 in separate section
-- genZ :: Complex Double -> Complex Double -> Int -> Complex Double
-- genZ n c d -- C1: Recursion
--     | d < 0 = n -- Base Case
--     | otherwise = escapesToInf (n*n + c) c (d-1) -- Recursive iteration to check whether or not n(z_n) escapes into infinity

-- Generate the grid of escape values
generateGrid :: Int -> Int -> Double -> Double -> Double -> Double -> Int -> (Complex Double -> Complex Double -> Int -> Int) -> [[Int]]
generateGrid w h x_min x_max y_min y_max maxIter f = 
    [ [ f (genC w h x_min x_max y_min y_max px py) 0  maxIter | px <- [0..w-1] ] | py <- [0..h-1] ]
    

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

main :: IO ()
main = do
    let w = 600    -- Width of the grid (number of pixels)
        h = 400    -- Height of the grid (number of pixels)
        x_min = -2.0
        x_max = 1.0
        y_min = -1.5
        y_max = 1.5
        maxIter = 1000 -- Maximum number of iterations

    -- Generate the Mandelbrot set grid
    let grid = generateGrid w h x_min x_max y_min y_max maxIter hasEscaped

    -- Print the grid
    printMatrix grid