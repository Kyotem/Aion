-- Fractal Generator (Renderer Module) made by Kyotem
-- Last Edit: 08/09/2025

-- TODO: (for v2) adjust so it can print values from 0 to 1 (float/double) value to measure how far it escaped

-- Read value, convert to character for rendering
convert :: Int -> Char
convert x  
    | x == 0 = '*'
    | x == 1 = '-'
    | otherwise = '?'  -- non-mapped values

printMatrix :: [[Int]] -> IO ()
-- Working with monads here? Do some more digging here: https://hoogle.haskell.org/?hoogle=mapM_, need to understand how the function works properly.
printMatrix = mapM_ (putStrLn . map convert)

-- Example render
main :: IO ()
main = do
    let matrix = [[1, 1, 0, 1, 1, 0, 1, 1],[1, 1, 0, 1, 1, 0, 1, 1],[1, 1, 0, 1, 1, 0, 1, 1],[1, 1, 0, 1, 1, 0, 1, 1],[0, 1, 1, 1, 1, 1, 1, 0],[1, 0, 0, 0, 0, 0, 0, 1],[1, 1, 1, 1, 1, 1, 1, 1]]
    printMatrix matrix
