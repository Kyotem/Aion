-- Fractal Generator made by Kyotem
-- Last Edit: 2025-09-08
module Main where

import System.IO (hFlush, stdout)

import Fractals(generateMandelbrot, hasEscaped)
import Renderer

{-
IO:

Step 1:
Ask for Julia or Mandelbrot selection

Step 2:
Ask for image width
Ask for image height
Ask for x_min
Ask for x_max
Ask for y_min
Ask for y_max

Step 3:
    If Julia:
    Ask for complex z (for Julia)

    If Mandelbrot:
    Ask for complex c (px, py)

Step 4:
Ask for rendering type

Step 5:
Ask for 'precision' (hasEscaped, is it 0 to 1, or 0 to 255, etc)

Step 6:
Perform calculations & Renders

Step 7:
Exit code

-}


-- step1 :: IO ()
-- step1 = do
--     putStrLn "Welcome to my beautiful Fractal Generator"
--     putStrLn "Please choose which set you want to generate"
--     putStrLn "1 -> Mandelbrot set"
--     putStrLn "2 -> Julia set"
--     putStr "Enter your choice: "

-- step2 :: IO ()
-- step2 = do
--     putStrLn "Newchoice:"
--     c <- getLine
--     putStrLn("c: " ++ c)


-- main :: IO ()
-- main = do
--     -- displayChoices
--     putStr "Enter your choice: "
--     hFlush stdout -- This is so dumb. I have to flush ze buffer because..? I'm sure there is a better way, oh well!
--     t <- getLine
--     putStrLn("t: " ++ t)
--     step2
--     putStrLn("c: " ++ c)


-- TODO: Add user input to decide what to do
main :: IO ()
main = do
    let w = 4000    -- Width of the grid (number of pixels)
        h = 4000   -- Height of the grid (number of pixels)
        x_min = -2.0 -- x to map the grid to
        x_max =  1.0
        y_min = -1.5 -- y to map the grid to
        y_max =  1.5    
        maxIter = 1000 -- Maximum number of iterations

    -- Generate the Mandelbrot set grid
    let grid = generateMandelbrot w h x_min x_max y_min y_max maxIter hasEscaped

    -- printMatrix grid
    writeMatrixToFile "C:/Users/finnp/Downloads/test.txt" grid

    putStrLn "done"