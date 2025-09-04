-- Fractal Generator made by Kyotem
-- Last Edit: 04/09/2025

-- Signature works like: Param1 -> Param2 -> Return type
escapesToInf :: Float -> Float -> Int -> Bool

-- TODO: calculate equation manually and check if function is being calculated properly (Seems to return 3 in some cases where it LIKELY shouldn't but unsure)
-- Based on: https://www.futurelearn.com/info/courses/functional-programming-haskell/0/steps/27226 (Reference)
-- ! TODO: Can this singular function be used for both Julia & Mandlebrot sets? (Figure out)
-- n = z_0 | c = constant | d = iteration depth
escapesToInf n c d -- C1: Recursion
    | d < 0 = False -- Base Case
    | n > 2 = True -- If escapes into infinity, return 3 (TODO: Create better handling for this)
    | otherwise = escapesToInf (n*n + c) c (d-1) -- Recursive iteration to check whether or not n(z_n) escapes into infinity

-- Recommended depth is around d=500 to check for infinity-escape (.. Source? Read it somewhere... TODO: Cite source)


-- grid = grid, z = z_0, c = c, t = type

-- calcGrid grid[] z c t


-- calcGrid grid[] z c t
--     | t == 1 = test z c 500 -- Julia X (TODO: Implement)
--     | t == 2 = test z c 500 -- Mandebrot?

-- Base case: Grid has been fully computed
 
 {-
 
 calcGrid grid[] s

    | check if all items gone through (Maybe do a lazy call with pre-set length and optimize from there?)
    | if not complete, calculate next one and save value in new arraygrid

 -}

--  calcGrid grid[] arraySize stepSize data[]
--     | arraySize < 0 = data[]
--     | otherwise = data[] = grid (escapesToInf grid[s] 0.1 500)


fillArray array[] arraySize
    | arraySize < 0 = array[]
    | otherwise fillArray (array[arraySize % arraySize + 1]) (arraySize -1)




main :: IO ()
main = do
    print(escapesToInf 0.2 0.2 500)
    emptyArray = Int[]
    print( )
