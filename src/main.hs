-- Fractal Generator made by Kyotem
-- Last Edit: 04/09/2025

-- Signature works like: Param1 -> Param2 -> Return type
test :: Float -> Float -> Int -> Float

-- TODO: calculate equation manually and check if function is being calculated properly (Seems to return 3 in some cases where it LIKELY shouldn't but unsure)
-- Based on: https://www.futurelearn.com/info/courses/functional-programming-haskell/0/steps/27226 (Reference)
-- ! TODO: Can this singular function be used for both Julia & Mandlebrot sets? (Figure out)
-- n = z_0 | c = constant | d = iteration depth
test n c d -- C1: Recursion
    | d < 0 = n -- Base Case
    | n > 2 = 3 -- If escapes into infinity, return 3 (TODO: Create better handling for this) 
    | otherwise = test (n*n + c) c (d-1) -- Recursive iteration to check whether or not n(z_n) escapes into infinity

-- Recommended depth is around d=500 to check for infinity-escape (.. Source? Read it somewhere... TODO: Cite source)

main :: IO ()
main = do
    print(test 1 4 500)
