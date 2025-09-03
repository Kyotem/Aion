-- Fractal Generator made by Kyotem
-- Last Edit: 03/09/2025 =

-- TODO: Create separate functions for calculating Julia & Mandlebrot sets
-- Can this be condensed?
equation :: Float -> Float
-- c = 0.2 
equation n = n*n + 0.2

-- Look into executing Recursion properly (Base condition, etc)
-- equation n = equation(n*n + 0.2) 

main :: IO ()
main = do
    print(equation 1.9)
    