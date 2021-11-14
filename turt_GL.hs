
{-# LANGUAGE GADTs #-}
module Main where

import Control.Monad hiding (forever)
-- import Graphics.HGL
import Graphics.UI.GLUT

-- | Module Turtle, EDSL Deep Embedding

data Color' = Red' | Green' | Blue | Yellow | Cyan deriving Show

-- | Abstract data type Program', Turtle Language
data Program' where
    -- Combinators:
    Forward  :: Double -> Program'                  -- | Forward movement by n steps
    Backward :: Double -> Program'                  -- | Backward movement by n steps
    RightT   :: Angle -> Program'                   -- | Right turn, 0 - 360 degree
    LeftT    :: Angle -> Program'                   -- | Left turn, 0 - 360 degree
    ColorT   :: Color' -> Program'                  -- | Turtle trace color (from HGL)
    PenUp    :: Program'                            -- | No trace, stop drawing
    PenDown  :: Program'                            -- | Start drawing
    Times    :: Int -> Program' -> Program'         -- | Repeats Program'
    Lifespan :: Ttl -> Program'                     -- | Kills Turtle after time t
    Limited  :: Lim -> Program' -> Program'
    Die      :: Program'                            -- | Kill turtle, no drawing possible
    Idle     :: Program'                            -- | Do nothing during one time unit
    Forever  :: Program' -> Program'                -- | Repeats the Program' forever

    -- Operations:
    Bind    :: Program' -> Program' -> Program'     -- | Sequencing operator (>*>)
    Split   :: Program' -> Program' -> Program'     -- | Parallel composition, performs
                                                    -- | two Program's during one time unit

-- | Abstract data type Turtle
data Turtle where
    -- Constructors:
    Dead :: Turtle
    -- | Constructor for the Turtle State, keeps the current state of a Turtle
    T' :: TurtleState -> Turtle
    -- | Constructor for additional Turtles, being arisen when applying Split
    -- | operator (<|>)
    Ts :: [Turtle] -> Turtle
-- | Turtle State: Coordinates (X, Y); Orientation (Deg); Color (HGL); Pen State (Up/Down)
type TurtleState = (Coords, Angle, Color', Bool, Ttl, Lim)
type Coords = (Double, Double)
type Ttl = Int
type Lim = Int
--type Angle = Int
--data Color = Red | Green | Blue | Yellow deriving Show

-- | Combinator functions
forward :: Double -> Program'
forward = Forward

backward :: Double -> Program'
backward = Backward

rightT :: Angle -> Program'
rightT = RightT

leftT :: Angle -> Program'
leftT = LeftT

penup :: Program'
penup = PenUp

pendown :: Program'
pendown = PenDown

times :: Int -> Program' -> Program'
times = Times

lifespan :: Int -> Program'
lifespan = Lifespan

limited :: Int -> Program' -> Program'
limited = Limited

forever :: Program' -> Program'
forever = Forever

color' :: Color' -> Program'
color' = ColorT

die :: Program'
die = Die

idle :: Program'
idle = Idle

-- Higher-order commands:
circle :: Double -> Program'
circle radius = times 72 (forward st >*> rightT 5)
    where st = (2 * pi * radius) / 72

square :: Double -> Program'
square size = times 4 (forward size >*> rightT 90)

(>*>) :: Program' -> Program' -> Program'
(>*>) = Bind

(<|>) :: Program' -> Program' -> Program'
(<|>) = Split

-- | Initial Turtle, located on the screen center, oriented to the North, Red Pen, Pen is Down
initialTurtle = T' ((300,300),0,Red', True, -1, -1)
initialTurtleDbl = Ts [initialTurtle,(T' ((300,320), 45, Green', True, -1, -1))]


-- Main Program'
-- main = runGraph initialTurtle ex_tree_forever
--main = runGraph initialTurtle ex_limited
--main = runGraph initialTurtle ex_forever
--main = runGraph initialTurtleDbl ex_finiteSpiral
-- main = runGraph initialTurtle (ex_infSpiralTwistCol 5)
-- main = runGraph initialTurtle (ex_fracTree'' 150)
-- main = runGraph initialTurtleDbl ex_finiteSpiral
--main = runGraph initialTurtleDbl (ex_infSpiral 5)
--main = runGraph initialTurtle (ex_fracTree' 75)
--main = runGraph initialTurtle ex_finSpiral1
--main :: IO ()
main = do
    putStrLn $ "asddfasdf"
    runGraph initialTurtle ex_circle
    --return ()

runGraph :: Turtle -> Program' -> IO ()
-- runGraph tur prg = runGraphics $ do
runGraph tur prg = do
    -- w <- openWindowEx "Turtle!" Nothing (600, 600) DoubleBuffered (Just 0 {- delay, ms -})
    -- drawInWindow w (polygon [(0,0),(0,600),(600,600),(600,0)])
    -- runFunc w tur prg
    -- return ()
    (_progName, _args) <- getArgsAndInitialize
    --initialDisplayMode $= [ RGBAMode ]
    initialWindowSize $= Size 1024 1024
    putStrLn $ "::: ----> 1"
    _window <- createWindow "main window"
    displayCallback $= display _window tur prg
    mainLoop

display :: Window -> Turtle -> Program' -> DisplayCallback
display w tur prg = do 
  --clear [ColorBuffer]
  -- renderPrimitive LineLoop $
  --    mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints
  -- flush

  putStrLn $ "::: display before runFunc"
  runFunc w tur prg
  return ()


runFunc :: Window -> Turtle -> Program' -> IO (Turtle)
runFunc w t p = do
    case t of
        Dead    ->
            return t
        T' ((x,y),a,c,penSt, oldTtl, oldLim) -> do
            putStrLn $ "::: runFunc T'"
            renderPrimitive LineLoop $
                mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints
            flush
            --runFunc w t p
            return t
        Ts tts  ->
            fmap (\z -> Ts z) $ sequence [runFunc w t p | t <- tts]


ex_circleR radius = times 72  (forward radius >*> rightT 5)
ex_circle = ex_circleR 10 >*> color' Cyan >*> ex_circleR 15

myPoints :: [(GLfloat, GLfloat, GLfloat)]
myPoints = [(sin(2 * pi * k / 12), cos(2 * pi * k / 12), 0) | k <- [1..12]]


