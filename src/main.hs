module Main where

import System.IO (hFlush, stdout)
import Fractals (generateMandelbrot, generateJulia, hasEscaped)
import Renderer (printMatrix, writeMatrixToFile, toGrayPixels, renderMatrixGeneric)
import Data.Complex (Complex((:+)))
import Codec.Picture (savePngImage, DynamicImage(ImageY8), Pixel8,)
import Codec.Picture.Types (generateImage)

-- Get user-input(Prompt: x)
-- a = the type to convert the String to when using read. (e.g., prompt "Enter int: " :: IO Int)
-- ! Look a bit more into this! (Applying typeclass constraint, Polymorphism AND Monads), easy to use in this case, documentation might need a deeper dive for other applications / optimizations
prompt :: Read a => String -> IO a
prompt msg = do
    -- Main message
    putStr msg
    putStrLn " "

    -- Flush buffer so I can show the prompt msg on the same line as the input
    -- FIXME: Temporarily disabled, causes lag on Laptop, not on Desktop. Hmmmm
    -- hFlush stdout

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

promptRenderChoice :: IO Int
promptRenderChoice = do

    -- TODO: ASCII, 

    putStrLn "Select which method you'd like to use to render:"
    putStrLn "0 -> ASCII Art (Print to console)"
    putStrLn "1 -> ASCII Art (Save to txt file)"
    putStrLn "2 -> ASCII Art (Save to .png)"
    putStrLn "3 -> Grayscale Image (Save as .png)"
    putStrLn "4 -> Color Image (Save as .png)"

    choice <- prompt "Enter your choice:" :: IO Int

    -- FIXME: Duplicate code, plsfix
    if choice `elem` [0..4]
        then return choice -- If matches, return the selected choice
        else do            -- Else re-prompt
            putStrLn "Invalid choice, please enter 0, 1, 2, 3 or 4."
            promptRenderChoice


mainLoop :: IO ()
mainLoop = do
    putStrLn "Welcome to my epic Fractal Generator!"
    fractalChoice <- promptFractalChoice

    -- Grid parameters
    w       <- prompt "Enter width:"      :: IO Int
    h       <- prompt "Enter height:"     :: IO Int
    x_min   <- prompt "Enter x_min:"      :: IO Double
    x_max   <- prompt "Enter x_max:"      :: IO Double
    y_min   <- prompt "Enter y_min:"      :: IO Double
    y_max   <- prompt "Enter y_max:"      :: IO Double
    maxIter <- prompt "Enter maximum iterations:" :: IO Int

    -- C2: Pattern Matching
    grid <- case fractalChoice of
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

    renderChoice <- promptRenderChoice


    case renderChoice of
        0 -> do -- ASCII to Console
            printMatrix maxIter grid
            putStrLn "Generating Fractal (This may take some time depending on your selected values)..."

        1 -> do -- ASCII to .txt file

            putStrLn "Enter the direct filepath where you want to save the .txt to (Must include filename & extension type)"
            putStrLn "Example: C:/Users/Admin/Downloads/output.txt"
            filePath <- getLine
            putStrLn "Generating Fractal (This may take some time depending on your selected values)..."


            -- TODO: Get filepath here
            writeMatrixToFile filePath maxIter grid
        -- 2 -> do -- ASCII Art to .png
        3 -> do -- Grayscale Image as .png file
            -- TODO: Add default path (e.g., Downloads)
            putStrLn "Enter the full filepath for the output PNG (including filename & extension):"
            filePath <- getLine

            putStrLn "Generating Grayscale Fractal..."
            let imgGray = renderMatrixGeneric w h grid (toGrayPixels maxIter) 

            savePngImage filePath (ImageY8 imgGray)
            putStrLn $ "Saved grayscale fractal to: " ++ filePath

        -- 4 -> do -- Color Image as .png file

        _ -> error "Shouldn't happen. nice"

    -- Render result
    -- printMatrix maxIter grid

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
