-- Fractal Generator made by Kyotem
-- Last Edit: 2025-09-08
module Main where

import Fractals(generateMandelbrot, hasEscaped)
import Renderer

-- TODO: Add user input to decide what to do
main :: IO ()
main = do
    let w = 1000    -- Width of the grid (number of pixels)
        h = 1000    -- Height of the grid (number of pixels)
        x_min = -1.5 -- x to map the grid to
        x_max =  1.5
        y_min = -1.5 -- y to map the grid to
        y_max =  1.5    
        maxIter = 1000 -- Maximum number of iterations

    -- Generate the Mandelbrot set grid
    let grid = generateMandelbrot w h x_min x_max y_min y_max maxIter hasEscaped

    printMatrix grid