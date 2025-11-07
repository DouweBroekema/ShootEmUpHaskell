module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

--Custom gamestate datatype containing all gameworld info

data GameState = GameState
  { playerPos :: (Float, Float)
  , playerVel :: (Float, Float)
  , isPaused :: Bool
  , elapsedTime :: Float
  } deriving Show

--initialising gamestate

initialState :: GameState
initialState = GameState (0,0) (0,0) False 0.000

--handling input

handleInput :: Event -> GameState -> IO GameState
handleInput (EventKey (SpecialKey KeyUp) Down _ _) state = 
    let (vx,_) = playerVel state
    in return state { playerVel = (vx,200) }
handleInput (EventKey (SpecialKey KeyDown) Down _ _) state =
    let (vx,_) = playerVel state
    in return state { playerVel = (vx,-200) }
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) state = 
    let (_,vy) = playerVel state
    in return state { playerVel = (-200, vy) }
handleInput (EventKey (SpecialKey KeyRight) Down _ _) state =
    let (_,vy) = playerVel state
    in return state { playerVel = (200, vy) }
handleInput (EventKey (SpecialKey KeyUp) Up _ _) state = 
    let (vx,_) = playerVel state
    in return state { playerVel = (vx,0) }
handleInput (EventKey (SpecialKey KeyDown) Up _ _) state =
    let (vx,_) = playerVel state
    in return state { playerVel = (vx,0) }
handleInput (EventKey (SpecialKey KeyLeft) Up _ _) state = 
    let (_,vy) = playerVel state
    in return state { playerVel = (0, vy) }
handleInput (EventKey (SpecialKey KeyRight) Up _ _) state =
    let (_,vy) = playerVel state
    in return state { playerVel = (0, vy) }
handleInput _ state = return state

--updating game world

update :: Float -> GameState -> IO GameState
update dt state
  | isPaused state = return state
  | otherwise = 
      let (x,y)   = playerPos state
          (vx,vy) = playerVel state
      in return state { playerPos = (x + vx * dt, y + vy * dt)
                      , elapsedTime = elapsedTime state + dt }

--window settings

window :: Display
window = InWindow "Shoot em" (500, 500) (100, 100)

fps :: Int
fps = 60

--rendering world

render :: GameState -> IO Picture
render state = return $
  translate x y $
  color cyan $
  rectangleSolid 50 20
  where (x,y) = playerPos state

background :: Color
background = blue

main :: IO ()
main = playIO window background fps initialState render handleInput update