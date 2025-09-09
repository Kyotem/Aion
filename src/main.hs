module Main where

import System.IO (hFlush, stdout)
import Fractals (generateMandelbrot, generateJulia, hasEscaped)
import Renderer (writeMatrixToFile, printMatrix)
import Data.Complex (Complex((:+)))

-- Get user-input(Prompt: x)
-- a = the type to convert the String to when using read. (e.g., prompt "Enter int: " :: IO Int)
-- ! Look a bit more into this! (Applying typeclass constraint, Polymorphism AND Monads), easy to use in this case, documentation might need a deeper dive for other applications / optimizations
prompt :: Read a => String -> IO a
prompt msg = do
    -- Main message
    putStr msg
    putStr " "
    -- Flush buffer so I can show the prompt msg on the same line as the input
    hFlush stdout
    -- Get input from user
    line <- getLine

    -- C2: Pattern Matching (Top to bottom)
    case reads line of -- Try parse 'line' into value of 'a' (Read instance)
        [(val, "")] -> return val -- If parse = OK, 'val' = parsed value
                                  -- 
        _ -> do -- If parse = NOT OK, _ == wildcard (*)
            putStrLn "Invalid input, please try again."
            prompt msg -- Recursive call to retry (Can this be optimized? Let's not get it wrong too many times!)


-- Prompt until we get 0 or 1
promptFractalChoice :: IO Int
promptFractalChoice = do

    putStrLn "Select which set to generate:"
    putStrLn "0 -> Mandelbrot"
    putStrLn "1 -> Julia"

    choice <- prompt "Enter your choice:" :: IO Int

    -- Using infix for readability, would normally be elem choice [0,1]
    -- Using elem infix: 'returns True if the list contains an item equal to the first argument'
    -- https://zvon.org/other/haskell/Outputprelude/elem_f.html
    if choice `elem` [0,1]
        then return choice -- If matches, return the selected choice
        else do            -- Else re-prompt
            putStrLn "Invalid choice, please enter 0 or 1."
            promptFractalChoice

mainLoop :: IO ()
mainLoop = do
    putStrLn "Welcome to my epic Fractal Generator!"
    choice <- promptFractalChoice

    -- Grid parameters
    w       <- prompt "Enter width:"      :: IO Int
    h       <- prompt "Enter height:"     :: IO Int
    x_min   <- prompt "Enter x_min:"      :: IO Double
    x_max   <- prompt "Enter x_max:"      :: IO Double
    y_min   <- prompt "Enter y_min:"      :: IO Double
    y_max   <- prompt "Enter y_max:"      :: IO Double
    maxIter <- prompt "Enter maximum iterations:" :: IO Int

    -- C2: Pattern Matching
    grid <- case choice of
        0 -> do
            -- Mandelbrot
            return $ generateMandelbrot w h x_min x_max y_min y_max maxIter hasEscaped
        1 -> do
            -- Julia (ask for c)
            realPart <- prompt "Enter real part of c:" :: IO Double
            imagPart <- prompt "Enter imaginary part of c:" :: IO Double
            let cJulia = realPart :+ imagPart
            return $ generateJulia cJulia w h x_min x_max y_min y_max maxIter hasEscaped
        _ -> error "This should never happen... good job!"

    -- TODO: Add choices for different render methods
    putStrLn "Generating Fractal (This may take some time depending on your selected values)..."

    -- Render result
    -- printMatrix maxIter grid
    writeMatrixToFile "C:/Users/Finn/Downloads/output.txt" maxIter grid

    -- Continue?
    putStrLn ""
    putStrLn "What do you want to do next?"
    putStrLn "0 -> Exit"
    putStrLn "1 -> Generate another fractal"
    next <- prompt "Enter your choice:" :: IO Int
    if next == 1
        then mainLoop   -- recursion (restart menu) -> Not really efficient if you want to render lots of diff ones.... right?
        else putStrLn "Goodbye!"

-- entry
main :: IO ()
main = mainLoop
