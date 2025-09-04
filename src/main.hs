-- Fractal Generator made by Kyotem
-- Last Edit: 03/09/2025 =

-- TODO: Create separate functions for calculating Julia & Mandlebrot sets
-- Can this be condensed?
-- equation :: Float -> Float
-- -- c = 0.2 
-- equation n = n*n + 0.2 
--             equation n

-- Look into executing Recursion properly (Base condition, etc)
-- equation n = equation(n*n + 0.2) 

-- test :: Float
-- test (n :: Float, d :: Int) = n*n + 0.2

test :: Float -> Float
test n =
    if n > 2 then 3
    else test(n*n + 0.2)



main :: IO ()
main = do
    print(test 1.2)
    