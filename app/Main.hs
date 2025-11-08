module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Environment (getScreenSize)
import System.Random (StdGen, randomR, mkStdGen)
import System.Exit (exitSuccess)
import Data.Maybe (catMaybes)

-- Variables
enemyScoreIncrement :: Int
enemyScoreIncrement = 10

playerXOffset :: Float
playerXOffset = 50

--Custom gamestate datatype containing all gameworld info

data GameState = GameState
  { playerPos    :: (Float, Float)
  , playerVel    :: (Float, Float)
  , playerHealth :: (Float, Float)
  , elapsedTime  :: Float
  , halfW        :: Float
  , halfH        :: Float
  , enemies      :: [EnemyType]
  , spawnTimer   :: Float
  , rng          :: StdGen
  , bullets      :: [Bullet]
  , bspawnTimer  :: Float
  , score        :: Int
  , highScore    :: Int
  , playerState  :: PlayerState
  } deriving Show

-- Player state 
data PlayerState = Playing | Paused | Dead
  deriving (Show, Eq)


--initialising gamestate

initialState :: GameState
initialState = GameState
  { playerPos    = (-300, 0)
  , playerVel    = (0, 0)
  , playerHealth = (100, 100)
  , elapsedTime  = 0
  , halfW        = 0
  , halfH        = 0
  , enemies      = [Dumb (Enemy (300, 0) (-100, 0) (20, 20) 100 0)
                   , Smart (SmartEnemy (400, 100) (-80, 0) (25, 25) 100 0)
                   ]  
  , spawnTimer   = 0
  , rng          = mkStdGen 42
  , bullets      = [Bullet (-300, 0) (800, 0) (10, 20) 20 0]
  , bspawnTimer  = 0
  , score        = 0
  , highScore    = 0
  , playerState  = Playing
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
handleInput (EventKey (Char 'p') Down _ _) state = 
   case playerState state of
             Dead    -> return initialState { halfW = halfW state, halfH = halfH state, playerPos = (-(halfW state) + playerXOffset, 0)}
             Playing -> return state { playerState = Paused}
             Paused  -> return state { playerState = Playing}
handleInput _ state = return state

--updating game world

update :: Float -> GameState -> IO GameState
update dt state = case playerState state of
  Paused -> return state
  Dead -> return state
  Playing -> do
      let (x,y)   = playerPos state
          (vx,vy) = playerVel state

          -- Moving enemies
          movedEnemies = map (moveEnemy dt (x,y)) (enemies state)
          -- Moving bullets
          movedBullets = [ b { bPos = (bx + bvx * dt, by + bvy * dt) }
                         | b <- bullets state
                         , let (bx, by)   = bPos b
                         , let (bvx, bvy) = bVel b
                         ]
          bulletTimer = bspawnTimer state - dt

          -- Enemy Spawning
          timer = spawnTimer state - dt
          (randY, gen1)   = randomR (-halfH state, halfH state) (rng state)
          (rType, newGen) = randomR (0 :: Float, 1 :: Float) gen1
          spawned =
            if timer <= 0
              then (if rType < 0.5
                then Dumb (Enemy (halfW state + 40, randY) (-100, 0) (20, 20) 100 (elapsedTime state))
                else Smart (SmartEnemy (halfW state + 40, randY) (-120, 0) (25, 25) 100 (elapsedTime state))
                ) : movedEnemies
              else movedEnemies
          finalTimer = if timer <= 0 then 2.0 else timer

          allCurrentEnemies =
            [ e | Dumb e <- spawned ] ++ [ convertSmart se | Smart se <- spawned ]
          convertSmart (SmartEnemy p v s h b) = Enemy p v s h b

          -- Player Collision
          playerHit = any (playerCollision (x,y) (50,20)) spawned

          -- Spawning new bullets
          newBullet = Bullet(x,y) (800,0) (10, 20) 20 (elapsedTime state)
          (allCurrentBullets,finalBulletTimer) =
            if bulletTimer <= 0
                then (newBullet : movedBullets, 0.2)
                else (movedBullets, bulletTimer)


          -- Gathering collision data
          pendingDestroyedEntities = allBulletCollisions allCurrentBullets allCurrentEnemies
          destroyedBullets = map fst pendingDestroyedEntities
          

          -- Gathering all enemies and updating their health if needed.
          hitEnemies = map snd pendingDestroyedEntities
          nonHitEnemies = filter (\x -> x `notElem` hitEnemies) allCurrentEnemies
          hitEnemiesProcessed = map f pendingDestroyedEntities
            where f (Bullet _ _ _ d _, Enemy ep ev es hp et) = Enemy ep ev es (hp - d) et

          findWrapper :: Enemy -> EnemyType -> EnemyType
          findWrapper e (Dumb _) =
            Dumb e
          findWrapper e (Smart _) =
            Smart (SmartEnemy (ePos e) (eVel e) (eSize e) (health e) (eBornT e))  

          -- Updating score
          newScore = score state
          scoreProcessed = foldr f newScore hitEnemiesProcessed
           where f (Enemy _ _ _ health _) acc = if health <= 0  then acc + enemyScoreIncrement else acc

          -- Finalizing bullets and enemies
          finalEnemies =
            [ et
            | et <- spawned
            , let base = case et of
                           Dumb e  -> e
                           Smart s -> convertSmart s
            , base `notElem` hitEnemies
            ] ++
            [ findWrapper updated original
            | updated@(Enemy ep ev es hp et) <- hitEnemiesProcessed, hp > 0
            , original <- spawned
            , let origBase = case original of
                               Dumb e  -> e
                               Smart s -> convertSmart s
            , (ep,ev,es) == (ePos origBase, eVel origBase, eSize origBase)
            ]
          finalBullets = [ bullet| bullet <- allCurrentBullets, not (bullet `elem` destroyedBullets)]
          
          
    
       
           in 
             return state 
               { playerPos   = (x + vx * dt, y + vy * dt)
               , enemies     = finalEnemies
               , spawnTimer  = finalTimer
               , elapsedTime = elapsedTime state + dt 
               , rng         = newGen
               , bullets     = finalBullets
               , bspawnTimer = finalBulletTimer
               , score       = if playerHit then 0 else scoreProcessed
               , playerState = if playerHit then Dead else Playing
               }

--window settings
window :: Display
window = FullScreen

fps :: Int
fps = 144

-- Scale and offset settings
scoreTextScale :: Float
scoreTextScale = 0.25

scoreTextSpacing :: Float
scoreTextSpacing = 50

textXOffset :: Float
textXOffset = 50

highScoreTextXOffset :: Float
highScoreTextXOffset = 160

scoreTextXOffset :: Float
scoreTextXOffset = 110

enemySize :: Float
enemySize = 40

playerSize :: (Float, Float)
playerSize = (50, 20)
--rendering world
render :: GameState -> IO Picture
render state = return $
  pictures $
    [ translate x y $ color cyan $ uncurry rectangleSolid playerSize ] ++
    [ case e of
      Dumb  (Enemy (ex, ey) _ _ _ _)        -> translate ex ey $ color red   $ rectangleSolid enemySize enemySize
      Smart (SmartEnemy (sx, sy) _ _ _ _)   -> translate sx sy $ color green $ rectangleSolid enemySize enemySize
    | e <- enemies state ] ++
    [ translate bx by $ color yellow $ rectangleSolid 20 10
    | Bullet (bx, by) (bvx, bvy) (sx, sy) bD bornT <- bullets state] ++ 
    [translate (-halfW state + textXOffset + scoreTextXOffset) (halfH state - scoreTextSpacing) $ scale scoreTextScale scoreTextScale $ color white $  text $ show $ score state,
    translate (-halfW state + textXOffset) (halfH state - scoreTextSpacing) $ scale scoreTextScale scoreTextScale $ color white $  text "Score:", 
    translate (-halfW state + textXOffset) (halfH state - (2 * scoreTextSpacing)) $ scale scoreTextScale scoreTextScale $ color white $ text "Highscore:",
     translate (-halfW state + textXOffset + highScoreTextXOffset) (halfH state - (2 * scoreTextSpacing)) $ scale scoreTextScale scoreTextScale $ color white $ text $ show $ highScore state] ++ 
    ( case playerState state of 
      Paused -> [color (withAlpha 0.8 black) $ rectangleSolid (halfW state *2) (halfW state *2), translate 0 0 $ color cyan $ text "Paused"]
      Dead ->  [color (withAlpha 0.9 red) $ rectangleSolid (halfW state *2) (halfW state *2), translate 0 0 $  color black $ text "You died!",
                 translate 0 (-125) $ scale 0.5 0.5 $ color black $ text "Press 'p' to restart"]
      Playing -> [])
  where
    (x, y) = playerPos state

--enemy logic
data EnemyType= Dumb Enemy | Smart SmartEnemy 
  deriving (Show, Eq)

data Enemy = Enemy
  { ePos   :: (Float, Float)
  , eVel   :: (Float, Float)
  , eSize  :: (Float, Float)
  , health :: Float
  , eBornT :: Float
  } deriving (Show, Eq)

data SmartEnemy = SmartEnemy
  { sePos   :: (Float, Float)
  , seVel   :: (Float, Float)
  , seSize  :: (Float, Float)
  , seHealth :: Float
  , seBornT :: Float
  } deriving (Show, Eq)

data Obstacle = Obstacle
  { oPos   :: (Float, Float)
  , oVel   :: (Float, Float)
  , oSize  :: (Float, Float)
  , oBornT :: Float
  } deriving (Show, Eq)

moveEnemy :: Float -> (Float, Float) -> EnemyType -> EnemyType
moveEnemy dt (_, playerY) enemy =
  case enemy of
    Dumb e ->
      let (ex, ey)   = ePos e
          (evx, evy) = eVel e
      in Dumb e { ePos = (ex + evx * dt, ey + evy * dt) }

    Smart se ->
      let (ex, ey)   = sePos se
          (evx, _)   = seVel se
          smartVy    = if playerY > ey then 60 else if playerY < ey then -60 else 0
      in Smart se { sePos = (ex + evx * dt, ey + smartVy * dt) }

--bullet logic
data Bullet = Bullet
  { bPos    :: (Float, Float)
  , bVel    :: (Float, Float)
  , bSize   :: (Float, Float)
  , bDamage :: Float
  , bBornT  :: Float
  } deriving (Show, Eq)



-- Handling all collisions between all bullets and all enemies
allBulletCollisions :: [Bullet] -> [Enemy] -> [(Bullet, Enemy)]
allBulletCollisions bs es = [ (bullet, enemy)  | bullet <- bs, enemy <- es, bulletCollision bullet enemy]


-- Single collision between bullet and enemy
bulletCollision :: Bullet -> Enemy -> Bool
bulletCollision (Bullet (bposx, bposy) _ (bsx, bsy) _ _) (Enemy (eposx, eposy) _ (esx, esy) _ _) = 
  (bposx < eposx + esx) && 
  (bposx + bsx > eposx) &&
  (bposy < eposy + esy) &&
  (bposy + bsy > eposy)

-- Collision for player
playerCollision :: (Float, Float) -> (Float, Float) -> EnemyType -> Bool
playerCollision (px, py) (pw, ph) et =
  let overlap (px, py, pw, ph) (ex, ey, ew, eh) =
        (px < ex + ew) && (px + pw > ex) && (py < ey + eh) && (py + ph > ey)
  in case et of
      Dumb  (Enemy (ex,ey) _ (ew,eh) _ _)      -> overlap (px,py,pw,ph) (ex,ey,ew,eh)
      Smart (SmartEnemy (ex,ey) _ (ew,eh) _ _) -> overlap (px,py,pw,ph) (ex,ey,ew,eh)


background :: Color
background = black

main :: IO ()
main = do
    (screenWidth, _screenHeight) <- getScreenSize
    let startX = fromIntegral (-screenWidth) / 2 + playerXOffset
        startState = initialState 
          { playerPos = (startX, 0)
          , halfW = fromIntegral screenWidth / 2
          , halfH = fromIntegral _screenHeight / 2 
          }
    playIO window background fps startState render handleInput update