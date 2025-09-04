-- Fractal Generator made by Kyotem
-- Last Edit: 04/09/2025 =

-- TODO: Create separate functions for calculating Julia & Mandlebrot sets

-- test (n :: Float, d :: Int) = n*n + 0.2


-- Signature works like: Param1 -> Param2 -> Return type
test :: Float -> Int -> Float

-- TODO: Convert to using guards for this: https://www.futurelearn.com/info/courses/functional-programming-haskell/0/steps/27226
-- Is much more readable & much better haskell-wise
test n d =
    -- Base case
    if d < 0 then n
    -- Prevent infinity
    else if n > 2 then 3
    else test (n*n + 0.2) (d - 1)


main :: IO ()
main = do
    print(test 0.3 5)
    