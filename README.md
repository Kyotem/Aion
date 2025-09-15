# Aion
![GHC 9.6.7](https://img.shields.io/badge/GHC-9.6.7-5e5086)
![GHC2021](https://img.shields.io/badge/GHC-2021-5e5086)
![Cabal](https://img.shields.io/badge/Cabal-3.12.1.0-6a6bd7)

Aion is a Haskell application designed to calculate and render simple Mandlebrot- and Julia set fractals.

![Mandelbrot Set Example](examples/0[-2;2][-2;2]1000.png)
![Julia Set Example](examples/1[-2;2][-2;2]1000[-0.8;0.156].png)
![Julia Set Example](examples/1[-2;2][-2;2]1000[0.285;0.01].png)

## Functionality:

Type of Fractals supported:
- Mandelbrot Sets
- Julia Sets

Available Render methods:
1. Print with ASCII characters to Console
2. Save with ASCII characters to a .txt file
3. Save as a grayscale image
4. Save as a colored image


## How to use
1. Load the .exe in a Terminal or Powershell window
2. Follow the steps provided by the application

## How to compile

1. Clone the project to any directory using: `git clone https://github.com/Kyotem/Aion.git`
2. Open a terminal in the main directory of this project
3. Resolve the dependencies: `cabal update`
4. Build the executable: `cabal build`

Once cabal has finished building the application, you can find the executable here (From the main directory): `./dist-newstyle/build/x86_64-windows/ghc-9.6.7/Aion-0.1.0.0/x/Aion/build/Aion/Aion.exe`


## Dependencies:
(Based upon the Aion.cabal file)
- JuicyPixels 3.3.9
- filepath
- parallel

## Authors

- [@Kyotem](https://github.com/Kyotem/)

