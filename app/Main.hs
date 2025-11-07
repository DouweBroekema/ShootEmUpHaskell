module Main where

import Graphics.Gloss

window :: Display
window = InWindow "Game window" (500, 500) (10, 10)

background :: Color
background = blue

drawing :: Picture
drawing = circle 80

main :: IO ()
main = display window background drawing