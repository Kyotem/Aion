-- Fractal Generator made by Kyotem
-- Last Edit: 08/09/2025


{- 
!!! Haskell has native support for complex numbers !!!
https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Complex.html
Can make your calculations MUCH more readable without needing to convert to plain arithm.
-}
-- Required to import functions next to the namespace import / (Module)
import Data.Complex (Complex((:+)), realPart, imagPart)


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


-- fillArray array[] arraySize
--     | arraySize < 0 = array[]
--     | otherwise fillArray (array[arraySize % arraySize + 1]) (arraySize -1)


{-

genC w h x_min x_max y_min y_max px py =

    dx = (x_max - x_min) / (w-1)
    dy = (y_max -y_min) / (h-1)

    c_r = x_min + (px * dx)
    c_i = y_max - (py * dy)

    c = c_r :+ c_i

---

genC w h x_min x_max y_min y_max px py =
    x_min + (px * ((x_max - x_min) / (w-1))) :+ y_max - (py * ((y_max -y_min) / (h-1)))

w = Image width (Pixels)
h = Image height (Pixels)

x_min, x_max (Bounds of x-axis to map to)
y_min, y_max (Bounds of y-axis to map to)

px py = selected pixel to map c to.

 -}




genC :: Int -> Int -> Double -> Double -> Double -> Double -> Int -> Int -> Complex Double
genC w h x_min x_max y_min y_max px py =
    (x_min + fromIntegral px * ((x_max - x_min) / fromIntegral (w - 1)))
    :+ (y_max - fromIntegral py * ((y_max - y_min) / fromIntegral (h - 1)))
-- Using fromIntegral for conversion: https://wiki.haskell.org/Converting_numbers
-- Resolve issues when inputting the parameters (Negative numbers can be seen as numeric litearls whilst it is expecting a double)
-- TODO: Improve type signature & calculations (Somewhere later)


main :: IO ()
main = do
    let x = genC 200 200 (-2) 1 (-1.5) 1.5 200 200
    putStrLn $ "Real part: " ++ show (realPart x)
    putStrLn $ "Imag part: " ++ show (imagPart x)
    -- https://hackage.haskell.org/package/base-4.21.0.0/docs/Text-Show.html
    -- Can convert a function to return it as a String to begin with. Maybe for rendering down the line? Ehh for now Text.Show is fine.
