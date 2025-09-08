-- Fractal Generator made by Kyotem
-- Last Edit: 2025-09-08
module Main where

import Fractals(generateJulia, hasEscaped)
import Renderer

{- 
!!! Haskell has native support for complex numbers !!!
https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Complex.html
Can make your calculations MUCH more readable without needing to convert to plain arithm.
-}
-- Required to import functions next to the namespace import / (Module)
-- import Data.Complex (Complex((:+)), realPart, imagPart, magnitude)



main :: IO ()
main = do
    let w = 1000    -- Width of the grid (number of pixels)
        h = 1000    -- Height of the grid (number of pixels)
        x_min = -1.5
        x_max =  1.5
        y_min = -1.5
        y_max =  1.5    
        maxIter = 1000 -- Maximum number of iterations

    -- Generate the Mandelbrot set grid
    let grid = generateJulia w h x_min x_max y_min y_max maxIter hasEscaped

    printMatrix grid