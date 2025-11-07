module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Environment (getScreenSize)
import System.Random (StdGen, randomR, mkStdGen)
import System.Exit (exitSuccess)

--Custom gamestate datatype containing all gameworld info

data GameState = GameState
  { playerPos   :: (Float, Float)
  , playerVel   :: (Float, Float)
  , isPaused    :: Bool
  , elapsedTime :: Float
  , halfW       :: Float
  , halfH       :: Float
  , enemies     :: [Enemy]
  , spawnTimer  :: Float
  , rng         :: StdGen
  } deriving Show

--initialising gamestate

initialState :: GameState
initialState = GameState
  { playerPos   = (-300, 0)
  , playerVel   = (0, 0)
  , isPaused    = False
  , elapsedTime = 0
  , halfW       = 0
  , halfH       = 0
  , enemies     = [Enemy (300, 0) (-100, 0) (20, 20) 0]  
  , spawnTimer  = 0
  , rng         = mkStdGen 42
  }

--handling input

handleInput :: Event -> GameState -> IO GameState
handleInput (EventKey (SpecialKey KeyUp) Down _ _) state = 
    let (vx,_) = playerVel state
    in return state { playerVel = (vx,200) }
handleInput (EventKey (SpecialKey KeyDown) Down _ _) state =
    let (vx,_) = playerVel state
    in return state { playerVel = (vx,-200) }
handleInput (EventKey (SpecialKey KeyUp) Up _ _) state = 
    let (vx,_) = playerVel state
    in return state { playerVel = (vx,0) }
handleInput (EventKey (SpecialKey KeyDown) Up _ _) state =
    let (vx,_) = playerVel state
    in return state { playerVel = (vx,0) }
handleInput (EventKey (SpecialKey KeyEsc) Down _ _) state = do
    exitSuccess
handleInput (EventKey (Char 'p') Down _ _) state = do
    return state {isPaused = not (isPaused state) }
handleInput _ state = return state

--updating game world

update :: Float -> GameState -> IO GameState
update dt state
  | isPaused state = return state
  | otherwise = 
      let (x,y)   = playerPos state
          (vx,vy) = playerVel state
          movedEnemies = [ e { ePos = (ex + evx * dt, ey + evy * dt) }
                       | e <- enemies state
                       , let (ex, ey)   = ePos e
                       , let (evx, evy) = eVel e
                       ]
          
          timer = spawnTimer state - dt
          (randY,newGen) = randomR (-halfH state, halfH state) (rng state)
          newEnemy = Enemy (halfW state + 40, randY) (-100,0) (20,20) (elapsedTime state)

          (finalEnemies,finalTimer) =
            if timer <= 0
                then (newEnemy : movedEnemies, 2.0)
                else (movedEnemies, timer)
      in return state 
               { playerPos   = (x + vx * dt, y + vy * dt)
               , enemies     = finalEnemies
               , spawnTimer  = finalTimer
               , elapsedTime = elapsedTime state + dt 
               , rng = newGen
               }

--window settings

window :: Display
window = FullScreen

fps :: Int
fps = 60

--rendering world

render :: GameState -> IO Picture
render state = return $
  pictures $
    [ translate x y $ color cyan $ rectangleSolid 50 20 ] ++
    [ translate ex ey $ color red  $ rectangleSolid 40 40
    | Enemy (ex, ey) (evx, evy) (sx, sy) bornT <- enemies state ]
  where
    (x, y) = playerPos state

--enemy logic
data Enemy = Enemy
  { ePos   :: (Float, Float)
  , eVel   :: (Float, Float)
  , eSize  :: (Float, Float)
  , eBornT :: Float
  } deriving Show

background :: Color
background = blue

main :: IO ()
main = do
    (screenWidth, _screenHeight) <- getScreenSize
    let startX = fromIntegral (-screenWidth) / 2 + 50
        startState = initialState 
          { playerPos = (startX, 0)
          , halfW = fromIntegral screenWidth / 2
          , halfH = fromIntegral _screenHeight / 2 
          }
    playIO window background fps startState render handleInput update