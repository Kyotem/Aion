{-|
Module      : Fractal Renderer
Description : Converts a matrix(2D) of data to characters and prints it out
Copyright   : (c) Finn Panhuijsen, 2025
License     : BSD-3-Clause
Maintainer  : FB.Panhuijsen@student.han.nl
Stability   : experimental

Last Edited: 2025-09-14
This module will be adjusted in a later version to implement different kind of rendering methods.
function convertToStar will change in the upcoming version.

! - Rendering isn't done in an efficient manner (Can be done parallel or per vectors), I'm not improving this anymore, though to match with Fractals.hs it might be worth to update later down the line. 

-}
module Renderer (printMatrix, writeMatrixToFile, toGrayPixels, toColoredPixels, renderMatrixGeneric) where

import Codec.Picture
import Codec.Picture.Types (generateImage)


-- TODO: (for v2) adjust so it can print values from 0 to 1 (float/double) value to measure how far it escaped
-- Or something like 0 - 255 to measure the LPP (Light-Per-Pixel) and map out white to black image

-- Converts the hard values for 0-1 to another character for rendering
-- ! Keep in mind that this will have to be adjusted to support a larger range whenever that's implemented

-- Character set defining the "density" of the current pixel (Can be dynamically adjusted) Left side = escaped quickly, right side = didn't escape
charset :: String
charset = " .:-=+*#%@"

iterationToChar :: Int -> Int -> Char
iterationToChar iter maxIter =
    -- Define length of charset
    let n = length charset

        -- iter * (n - 1) = Scale iteration count to charset
        -- Using infix `div` to use INTEGER division (prevent floating division)
        -- Divide by maxiteration to map the current iteration within the charset
        idx = iter * (n - 1) `div` maxIter
    in charset !! idx -- charset[idx]

-- Working with monads here? Do some more digging here: https://hoogle.haskell.org/?hoogle=mapM_, need to understand how the function works properly.
printMatrix :: Int ->[[Int]] -> IO ()
printMatrix maxIter matrix = mapM_ putStrLn rowStrings
  where
    rowStrings = map (map (`iterationToChar` maxIter)) matrix


-- Separate to different module?
-- Function to write the matrix to a file
writeMatrixToFile :: FilePath -> Int -> [[Int]] -> IO ()
writeMatrixToFile filePath maxIter matrix = do
    let matrixStr = unlines [ map (`iterationToChar` maxIter) row | row <- matrix ]
    writeFile filePath matrixStr

-- Grid = type of Pixel p with elements of a (Convert a to pixel elements) and return image
renderMatrixGeneric :: Int -> Int -> Pixel p => [[a]] -> (a -> p) -> Image p
-- W, H, 
renderMatrixGeneric width height matrix f =
    --matrix !! y (Select y-th row)
    -- (matrix 11 y) !! x select the x-th element of the row
    -- f ((matrix !! y) !!x) convert the element value per the given func
    -- Lambda function indicates what to do with each respective pixel (Which is using the function @ f to convert to the correct pixel type)
    generateImage (\x y -> f ((matrix !! y) !! x)) width height

-- Using logarithmic scaling, this way the lower iteration counts get stretched out more (Giving more detail there), whilst compressing higher iteration counts (Which usually don't need a lot of detail. -> Would rather see a greater diff between 1, 20 and 50 than 250 200 and 180

-- Pixel8 / Word8 (8-bit Grayscale value) -> (0-255)
toGrayPixels :: Int -> Int -> Pixel8
toGrayPixels maxIter n
    | n == 0    = 255  -- If escaped after first iteration, then just plain white
    | otherwise = 
        let t = logBase (fromIntegral maxIter + 1) (fromIntegral n + 1)   -- Convert iteration (int) to floating point; (lox(n+1) / log(maxIter+1)) -> Map range 0-1
        in 255 - round (255 * t) -- 255 * 255 = 65025; 65025 / 255 = 255 (Mapping), then round it to the nearest number
    
toColoredPixels :: Int -> Int -> PixelRGB8
toColoredPixels maxIter n
    | n == 0    = PixelRGB8 255 255 255 -- If instant escape, convert to white
    | otherwise =
        let t = logBase (fromIntegral maxIter + 1) (fromIntegral n + 1) -- Convert iteration (int) to floating point; (lox(n+1) / log(maxIter+1)) -> Map range 0-1
        -- Implemented per: https://en.wikipedia.org/wiki/Polynomial
            r = round (9*(1-t)*t*t*t * 255) -- Lower n (So more reddish)
            g = round (15*(1-t)*(1-t)*t*t * 255) -- Middle n (More green)
            b = round (8.5*(1-t)*(1-t)*(1-t)*t * 255) -- Higher n = (More blue/purple-ish)
        in PixelRGB8 r g b

