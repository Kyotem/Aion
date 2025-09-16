    {-|
    Module      : Fractal Calculator
    Description : Used to calculate Julia & Mandelbrot sets
    Copyright   : (c) Finn Panhuijsen, 2025
    License     : BSD-3-Clause
    Maintainer  : FB.Panhuijsen@student.han.nl
    Stability   : experimental

    Last Edited: 2025-09-15
    -}
    module Fractals (hasEscaped, calcDelta, genComplex, generateMandelbrot, generateJulia) where
    
    -- Local modules
    import Data.Complex (Complex((:+)), realPart, imagPart)
    import Control.Parallel.Strategies (parMap, rdeepseq) -- I'm using rdeepseq to make sure it's fully evaluated before moving on

    {-
    Function used to calculate the step size between each pixel. (Calculate separately between x and y if the resolution isn't 1:1)
    So if we have an x-y axis of x(-1;1) and y(-1,5;1), we can calculate the step size of x and y for each pixel. 
    ! Can add some overhead since I'm calling this multiple times, might be better to move directly into the generate functions themselves
    Takes:
    n = width or height
    n_min = min value of the x- or y-axis
    n_max = max value of the x- or y-axis
    -}
    calcDelta :: Int -> Double -> Double -> Double
    calcDelta u n_min n_max = (n_max - n_min) / fromIntegral (u - 1)

    {- 
    Recursive function used to calculate if a certain pixel has escaped into infinity or has been contained after a set of iteration.
    (Can be used for both Mandelbrot & Julia sets)

    z_{n+1} = z_n^2 + c
    Takes:
    c = The constant c (Must be a complex number)
    z0 = The starting value of z
    maxIter = The max amount of iterations to check if z escapes into infinity
    -}
    hasEscaped :: Complex Double -> Complex Double -> Int -> Int
    hasEscaped c z0 maxIter = step z0 0 -- Defining another function inside of this one just for clarity & tracking the iteration
        where
            step (x :+ y) iter
                | x*x + y*y > 4 = iter  -- Escape condition
                | iter >= maxIter = maxIter     -- If we reach max iterations, assume it hasn't escaped
                | otherwise = -- Recurse again and calculate new parts
                    let nextX = x*x - y*y + realPart c
                        nextY = 2*x*y + imagPart c
                    in step (nextX :+ nextY) (iter + 1)


    {- 
    Returns a complex number based on the axis & resolution size (Used for Mandelbrot because it has a changing complex value for c)

    Takes:
    x_min = x_min of the x-axis
    y_max = y_min of the y-axis
    dx = delta x
    dy = delta y
    px = selected pixel x
    py = selected pixel y
    -}
    genComplex :: Double -> Double -> Double -> Double -> Int -> Int -> Complex Double
    genComplex x_min y_max dx dy px py =
        (x_min + fromIntegral px * dx) -- Real part
        :+ (y_max - fromIntegral py * dy) -- Imag part
    -- Using fromIntegral for conversion: https://wiki.haskell.org/Converting_numbers
    -- TODO: Resolve issues when inputting the parameters (Negative numbers can be seen as numeric litearls whilst it is expecting a double)

    {- 
    Generates a matrix based on pixel-size & axis-size, inserts per pixel the value returned by argument f
    (Specifically for Mandelbrot sets)
    ! In parallel per row

    Takes:
    w = image width
    h = image height
    x_min = x_min of the x-axis
    x_max = x_min of the x-axis
    y_min = y_min of the y-axis
    y_max = y_min of the y-axis
    maxIter = max number of iterations used in hasEscaped
    f = function to calculate if a pixel escapes into infinity (Current: hasEscaped)
    -}
    generateMandelbrot :: Int -> Int -> Double -> Double -> Double -> Double -> Int -> (Complex Double -> Complex Double -> Int -> Int) -> [[Int]]
    generateMandelbrot  w h x_min x_max y_min y_max maxIter f =
        let
            dx = calcDelta w x_min x_max
            dy = calcDelta h y_min y_max

            row py = [ f (genComplex x_min y_max dx dy px py) 0 maxIter | px <- [0..w-1] ] -- Computes one row left to right
        in 
            parMap rdeepseq row [0..h-1] -- Splits each row between cores to compute paralell (so using row py)

    -- I know this is pretty much duplicated code, not plans to scale further so I'm keeping this like so. Sorry.
    {-
    Generates a matrix based on pixel-size & axis-size, inserts per pixel the value returned by argument f
    (Specifically for Julia sets)
    ! In parallel per row
    
    Takes:
    c = complex c used for Julia Sets 
    w = image width
    h = image height
    x_min = x_min of the x-axis
    x_max = x_min of the x-axis
    y_min = y_min of the y-axis
    y_max = y_min of the y-axis
    maxIter = max number of iterations used in hasEscaped
    f = function to calculate if a pixel escapes into infinity (Current: hasEscaped)
    -}
    generateJulia :: Complex Double -> Int -> Int -> Double -> Double -> Double -> Double -> Int -> (Complex Double -> Complex Double -> Int -> Int) -> [[Int]]
    generateJulia c w h x_min x_max y_min y_max maxIter f =
        let -- Probably losing a decent bit of performance by calling different functions instead of calculating it directly...
            dx = calcDelta w x_min x_max
            dy = calcDelta h y_min y_max
            gen = genComplex x_min y_max dx dy

            row py = [ f c (gen px py) maxIter | px <- [0..w-1] ]
        in
            parMap rdeepseq row [0..h-1]



