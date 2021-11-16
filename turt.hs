{-# LANGUAGE GADTs #-}
module Main where

import Control.Monad hiding (forever)
import Graphics.HGL

-- | Module Turtle, EDSL Deep Embedding

-- | Abstract data type Program, Turtle Language
data Program where
    -- Combinators:
    Forward  :: Double -> Program                -- | Forward movement by n steps
    Backward :: Double -> Program                -- | Backward movement by n steps
    RightT   :: Angle -> Program                 -- | Right turn, 0 - 360 degree
    LeftT    :: Angle -> Program                 -- | Left turn, 0 - 360 degree
    ColorT   :: Color -> Program                 -- | Turtle trace color (from HGL)
    PenUp    :: Program                          -- | No trace, stop drawing
    PenDown  :: Program                          -- | Start drawing
    Times    :: Int -> Program -> Program        -- | Repeats program
    Lifespan :: Ttl -> Program                   -- | Kills Turtle after time t
    Limited  :: Lim -> Program -> Program
    Die      :: Program                          -- | Kill turtle, no drawing possible
    Idle     :: Program                          -- | Do nothing during one time unit
    Forever  :: Program -> Program               -- | Repeats the program forever

    -- Operations:
    Bind    :: Program -> Program -> Program     -- | Sequencing operator (>*>)
    Split   :: Program -> Program -> Program     -- | Parallel composition, performs
                                                 -- | two programs during one time unit
-- | Abstract data type Turtle
data Turtle where
    -- Constructors:
    Dead :: Turtle
    -- | Constructor for the Turtle State, keeps the current state of a Turtle
    T :: TurtleState -> Turtle
    -- | Constructor for additional Turtles, being arisen when applying Split
    -- | operator (<|>)
    Ts :: [Turtle] -> Turtle
-- | Turtle State: Coordinates (X, Y); Orientation (Deg); Color (HGL); Pen State (Up/Down)
type TurtleState = (Coords, Angle, Color, Bool, Ttl, Lim)
type Coords = (Double, Double)
type Ttl = Int
type Lim = Int
--type Angle = Int
--data Color = Red | Green | Blue | Yellow deriving Show

-- | Combinator functions
forward :: Double -> Program
forward = Forward

backward :: Double -> Program
backward = Backward

rightT :: Angle -> Program
rightT = RightT

leftT :: Angle -> Program
leftT = LeftT

penup :: Program
penup = PenUp

pendown :: Program
pendown = PenDown

times :: Int -> Program -> Program
times = Times

lifespan :: Int -> Program
lifespan = Lifespan

limited :: Int -> Program -> Program
limited = Limited

forever :: Program -> Program
forever = Forever

color :: Color -> Program
color = ColorT

die :: Program
die = Die

idle :: Program
idle = Idle

-- Higher-order commands:
circle :: Double -> Program
circle radius = times arcNum (forward st >*> rightT arcAngleStepDeg)
    where 
        arcNum :: Int
        arcNum = 72
        arcAngleStepDeg = 5
        st = (2 * pi * radius) / 72

square :: Double -> Program
square size = times squareSidesNum (forward size >*> rightT squareAngleDeg)
    where
        squareSidesNum = 4
        squareAngleDeg = 90

(>*>) :: Program -> Program -> Program
(>*>) = Bind

(<|>) :: Program -> Program -> Program
(<|>) = Split

-- | Initial Turtle, located on the screen center, oriented to the North, Red Pen, Pen is Down
initialTurtle = T ((turtleStartX, turtleStartY), 0, Red, True, -1, -1)
    where
        turtleStartX = 600
        turtleStartY = 600

-- | Two initial turtles
initialTurtleDbl = Ts [initialTurtle,(T ((turtleStartX, turtleStartY), 45, Green, True, -1, -1))]
    where
        turtleStartX = 600
        turtleStartY = 620


-- Main Program
-- main = runGraph initialTurtle ex_tree_forever
--main = runGraph initialTurtle ex_limited
--main = runGraph initialTurtle ex_forever
--main = runGraph initialTurtleDbl ex_finiteSpiral
------main = runGraph initialTurtle (ex_infSpiralTwistCol 5)      {- nice -}
-- main = runGraph initialTurtle (ex_fracTree'' 150)        {- this tree -}
-- main = runGraph initialTurtleDbl ex_finiteSpiral
--main = runGraph initialTurtleDbl (ex_infSpiral 5)
--main = runGraph initialTurtle (ex_fracTree' 75)
--main = runGraph initialTurtle ex_finSpiral1
main = runGraph initialTurtle ex_circle



-- | Run Functions:

-- | Main drawing function, takes an Initial Turtle, draws turtle trace, calls
-- | function which describes Turtle commands being performed at the Time Unit
-- | Creates graphical window and passes it to the runFunc
runGraph :: Turtle -> Program -> IO ()
runGraph tur prg = runGraphics $ do
    w <- openWindowEx "Turtle!" Nothing (windowSizeX, windowSizeY) DoubleBuffered (Just 10 {- delay, ms -})
    drawInWindow w (polygon [(minX, minY), (minX, maxY), (maxX, maxY), (maxX, minY)])
    runFunc w tur prg
    return ()
        where
            windowSizeX = 1200
            windowSizeY = 1200
            minX = 0
            minY = 0
            maxX = windowSizeX
            maxY = windowSizeY

-- | Function, which fullfills the program execution, one Turtle Command per Time Unit
-- | After each execution step returns a Turtle in the new State
runFunc :: Window -> Turtle -> Program -> IO (Turtle)
runFunc w t p = do
    case t of
        Dead    ->
            return t
        T ((x,y),a,c,penSt, oldTtl, oldLim) ->
            let ttl = oldTtl - 1 in
            --let tWithTtl = T ((x,y),a,c,penSt, ttl, lim) in
            if ttl == 0 
                then do putStrLn ("Turtle has been destroyed.") >> return Dead
                else
                    if oldLim == 0
                        then return t
                        else
                            let lim = oldLim - 1 in
                            let tWithTtl = T ((x,y),a,c,penSt, ttl, lim) in
                            case p of
                                Die -> do
                                    putStrLn $ "Killing the turtle"
                                    getWindowTick w
                                    return Dead
                                Idle -> do
                                    putStrLn $ "Idle cycle"
                                    getWindowTick w
                                    return $ tWithTtl
                                Forward n -> do
                                    putStrLn $ "Forward movement by " ++ show n ++ " steps, Turtle now is here: (" ++ show newX ++ ", " ++ show newY ++ ", angle " ++ show a ++ ")"
                                    getWindowTick w
                                    drawInWindow w $ withColor penColor $ line (round x, round y) (round newX, round newY)
                                    return (T ((newX, newY),a,c, penSt, ttl, lim))
                                        where
                                            newX = x + n * sin(a*(pi/180))
                                            newY = {-600 - -} (y - n * (cos(a*(pi/180))) )
                                            penColor = if penSt then c else White
                                Backward n -> do
                                    putStrLn $ "Backward move by " ++ show n ++ " steps, Turtle now is here: (" ++ show newX ++ ", " ++ show newY ++ ", angle " ++ show a ++ ")"
                                    getWindowTick w
                                    drawInWindow w $ withColor penColor $ line (round x, round y) (round newX, round newY)
                                    return (T ((newX, newY),a,c, penSt, ttl, lim))
                                        where
                                            newX = x - n * sin(a*(pi/180))
                                            newY = {-600 - -} (y + n * (cos(a*(pi/180))) )
                                            penColor = if penSt then c else White
                                RightT n -> do
                                    putStrLn $ "Right turn: " ++ show n
                                    getWindowTick w
                                    return (T ((x,y), a + n, c, penSt, ttl, lim))
                                LeftT n -> do
                                    putStrLn $ "Left " ++ show n
                                    getWindowTick w
                                    return (T ((x,y), a + (360 - n), c, penSt, ttl, lim))
                                ColorT newColor -> do
                                    putStrLn $ "Changing color for turtle to: " ++ show newColor
                                    getWindowTick w
                                    return $ T ((x,y),a,newColor, penSt, ttl, lim)
                                Times n pr -> do
                                    putStrLn $ "Repeating program for times: " ++ show n
                                    getWindowTick w
                                    foldM (\trtl prgrm -> runFunc w trtl prgrm) t (replicate n pr)
                                Lifespan ttl -> do
                                    putStrLn $ "Lifespan set to: " ++ show ttl
                                    getWindowTick w
                                    return $ T ((x, y), a, c, penSt, ttl, lim)
                                Limited limNew pr -> do                                    -- pr = forward 10 >*> f 20 >*> f 30 >*> p2
                                    putStrLn $ "Time limit set to: " ++ show limNew
                                    getWindowTick w
                                    (T ((x, y), a, c, p, t, l)) <- runFunc w (T ((x, y), a, c, penSt, ttl, limNew)) pr
                                    putStrLn "Limited END"
                                    return $ T ((x, y), a, c, p, t, -1)       -- reset lim counter

                                    --return $ T ((x, y), a, c, penSt, ttl)
                                PenDown -> do
                                    putStrLn $ "Start drawing with color: " ++ show c
                                    getWindowTick w
                                    return $ T ((x, y), a, c, True, ttl, lim)
                                PenUp -> do
                                    putStrLn $ "Stop drawing"
                                    getWindowTick w
                                    return $ T ((x, y), a, c, False, ttl, lim)
                                Forever pr ->
                                    --pr `Bind` pr
                                    foldM (\trtl prgrm -> runFunc w trtl prgrm) t (if lim >= 0 then take lim (repeat pr) else (repeat pr))

                                p1 `Bind` p2 -> do
                                    t2 <- runFunc w t p1
                                    runFunc w t2 p2
                                p1 `Split` p2 -> do
                                    putStrLn $ "Split turtles"
                                    t1 <- runFunc w t p1
                                    t2 <- runFunc w t p2
                                    return $ Ts [t1,t2]
          --      _ -> return t
        -- If there are more than one Turtle, execute current command for all of them
        -- sequencing the list of runFunc applied to turtles
        Ts tts ->
            fmap (\z -> Ts z) $ sequence [runFunc w t p | t <- tts]


ex1 = forward 10
ex1' = backward 50 >*> rightT 45 >*> forward 100
ex2 = forward 75 >*> (forward 50) >*> (rightT 60) >*> forward 100
ex22 = forward 100 >*> forward 100 -- >*> color Blue >*> forward 100 >*> color Green >*> forward 150
-- square
ex23 = forward 100 >*> rightT 90 >*> forward 100
                        >*> rightT 90 >*> forward 100
                             >*> rightT 90 >*> forward 100
-- home
ex24 = forward 100 >*> rightT 45 >*> forward 100 >*> rightT 90
                    >*> times 1000 (idle) >*> forward 100
                    >*> rightT 45 >*> forward 100

ex3 = (forward 100) >*> color Green >*>
      ((rightT 45) <|> (leftT 45)) >*>
      (forward 150)

ex31 = (forward 100) >*> color Green >*>
       ((rightT 45) <|> (leftT 45 >*> times 200 (idle) >*> forward 123)) >*>
       (forward 150) >*> (rightT 30 <|> leftT 30) >*> forward 150 >*> lifespan 55 >*> times 50 (forward 30 >*> leftT 70 >*> forward 33 >*> leftT 80 >*> forward 31 >*> leftT 88)


ex4 = (forward 11) >*>
      ((rightT 22) <|> (rightT 23)) >*>
      (forward 30) >*>
      ((rightT 41) <|> (forward 42)) >*>
      (forward 55)

ex5 = color Red >*> (times 3 (forward 10 >*> (rightT 30 <|> leftT 30)) ) >*> color Green >*> forward 50
ex6 = ex5 >*> color Green >*> ex5

ex11 = color Red -- >*> (leftT 22) >*> (times 2 (forward 11))
ex12 = color Yellow >*> forward 11 >*> (leftT 22 <|> rightT 23) >*> color Green
ex13 = times 2 (forward 5)

ex666 = forward 100 >*> die

ex100 = forward (step) >*> backward step >*> ((rightT 30 <|> leftT 30) <|> (rightT 45 <|> leftT 45))
            >*> backward (step / 2) >*> (rightT 30 <|> leftT 30)
            >*> backward (step / 3) >*> (rightT 30 <|> leftT 30) >*> color Blue >*> penup
            >*> backward (step / 4) >*> (rightT 30 <|> leftT 30) >*> color Green
            >*> backward (step / 5) >*> (rightT 30 <|> leftT 30) >*> pendown
            >*> backward (step / 6) >*> (rightT 30 <|> leftT 30) >*> color Yellow
            >*> backward (step / 7) >*> (rightT 30 <|> leftT 30)
            where step = 150

ex101 = forward 100 >*> penup >*> forward 50 >*> pendown >*> forward 50
--exSpiral1 :: Int -> Program
--exSpiral1 x = forward _ >*> rightT 90

ex_forever =  backward 50 >*> forever (forward 100 >*> color Green >*> rightT 90
                                      >*> forward 100 >*> color Cyan >*> rightT 90
                                        >*> forward 100 >*> color Yellow >*> rightT 90
                                          >*> forward 120 >*> color Black >*> rightT 90 )

ex_limited = forward 99 >*> limited 50 (
                                    forever (forward 100 >*> color Green >*> rightT 90
                                      >*> forward 100 >*> color Cyan >*> rightT 90
                                        >*> forward 100 >*> color Yellow >*> rightT 90
                                          >*> forward 120 >*> color Black >*> rightT 90 )
    ) >*> color Red >*> forward 100

ex_limited_simple = forward 100 >*> rightT 90 >*> forward 10 >*> rightT 90 >*> color Green
                        >*> limited 5 (times 10 (forward 10)) >*> rightT 90 >*> color Yellow >*> forward 50

-- Infinite Spiral, normal square form
ex_infSpiral :: Double -> Program
ex_infSpiral iStep = spiralStep >*> ex_infSpiral (iStep + 12)
  where
    spiralStep = forward iStep
                    >*> rightT 90 >*> color Cyan
                      >*> forward (iStep + 3)
                        >*> rightT 90 >*> color Red
                          >*> forward (iStep + 6)
                            >*> rightT 90 >*> color Green
                              >*> forward (iStep + 9)
                                >*> rightT 90 >*> color Blue


-- Infinite Spiral, twisted
ex_infSpiralTwist :: Double -> Program
ex_infSpiralTwist iStep = spiralStep >*> ex_infSpiralTwist (iStep + 3)
  where
    spiralStep = forward iStep >*> rightT 89

-- Infinite Spiral, square form
ex_infSpiral0 :: Double -> Program
ex_infSpiral0 iStep = spiralStep >*> ex_infSpiral0 (iStep + 3)
  where
    spiralStep = forward iStep >*> rightT 89
-- Finite spiral from the infinite one
ex_finiteSpiral = lifespan 200 >*> ex_infSpiral 2


-- Infinite Spiral, normal square form
ex_infSpiralTwistCol :: Double -> Program
ex_infSpiralTwistCol iStep = spiralStep >*> ex_infSpiralTwistCol (iStep + 12)
  where
    spiralStep = forward iStep
                    >*> rightT 89 >*> color Cyan
                      >*> forward (iStep + 3)
                        >*> rightT 89 >*> color Red
                          >*> forward (iStep + 6)
                            >*> rightT 89 >*> color Green
                              >*> forward (iStep + 9)
                                >*> rightT 89 >*> color Blue

ex_finiteSpiral0 = color Cyan >*> lifespan 200 >*> (ex_infSpiralTwist 2)

ex_finSpiral1 = times 50 (forward 4 >*> rightT 27 >*> forward 8 >*> leftT 13 >*> backward 22) >*> ex_infSpiral 3
spStep x = forward x >*> rightT 90 >*> spStep (x + 5)



ex_finSpiral :: Double -> Program
--ex_finSpiral = lifespan 8 >*>
ex_finSpiral iStep = forward (10 + iStep) >*> rightT 95 >*>
    forward (10 + iStep) >*> rightT 95 >*>forward (10 + iStep) >*> rightT 88 >*>forward (10 + iStep) >*> rightT 88 >*>forward (10 + iStep) >*> rightT 88 >*>forward (10 + iStep) >*> rightT 88 >*>forward (10 + iStep) >*> rightT 88 >*>forward (10 + iStep) >*> rightT 88 >*>forward (10 + iStep) >*> rightT 88 >*>forward (10 + iStep) >*> rightT 88 >*>forward (10 + iStep) >*> rightT 88 >*>forward (10 + iStep) >*> rightT 88 >*>forward (10 + iStep) >*> rightT 88 >*>forward (10 + iStep) >*> rightT 88 >*>forward (10 + iStep) >*> rightT 88 >*>forward (10 + iStep) >*> rightT 88 >*>forward (10 + iStep) >*> rightT 88 >*>forward (10 + iStep) >*> rightT 88 >*>forward (10 + iStep) >*> rightT 88 >*>forward (10 + iStep) >*> rightT 88 >*>forward (10 + iStep) >*> rightT 88 >*>forward (10 + iStep) >*> rightT 88 >*>forward (10 + iStep) >*> rightT 88
-- Fractal tree 1
ex_fracTree :: Double -> Program
ex_fracTree step = color Green >*> forward (step) >*> (rightT 30 <|> leftT 30)
            >*> color Blue >*> forward (step) >*> ex_fracTree (step / 2)
-- Fractal tree
ex_fracTree' :: Double -> Program
ex_fracTree' step = forward (step) >*> forward step >*> ((rightT 30 <|> leftT 30) <|> (rightT 45 <|> leftT 45))
            >*> forward (step / 2) >*> (rightT 30 <|> leftT 30)
            >*> forward (step / 3) >*> (rightT 30 <|> leftT 30) >*> color Blue
            >*> forward (step / 4) >*> (rightT 30 <|> leftT 30) >*> color Green
            >*> forward (step / 5) >*> (rightT 30 <|> leftT 30) -- >*> die
            >*> forward (step / 6) >*> (rightT 30 <|> leftT 30) >*> color Yellow
            >*> forward (step / 7) >*> (rightT 30 <|> leftT 30)
-- Fractal Tree
ex_fracTree'' :: Double -> Program
ex_fracTree'' step = forward (step) >*> forward step >*> ((rightT 30 <|> leftT 30) <|> (rightT 45 <|> leftT 45))
            >*> forward (step / 2) >*> ((rightT 30 <|> leftT 30) <|> (rightT 45 <|> leftT 45))
            >*> forward (step / 3) >*> ((rightT 30 <|> leftT 30) <|> (rightT 45 <|> leftT 45))  >*> color Blue
            >*> forward (step / 4) >*> (rightT 30 <|> leftT 30) >*> color Green
            >*> forward (step / 5) >*> ((rightT 30 <|> leftT 30) <|> (rightT 45 <|> leftT 45))
            >*> forward (step / 6) >*> ((rightT 30 <|> leftT 30) <|> (rightT 45 <|> leftT 45))  >*> color Yellow
            >*> forward (step / 7) >*> (rightT 30 <|> leftT 30) >*> forward (step / 8)


ex_tree_forever :: Program
ex_tree_forever = forward 75 >*> lifespan 20 >*> forever (forward 10 >*> ((rightT 30 >*> color Green) <|> (leftT 30 >*> color Blue >*> forward 5)) )


ex_circleR radius = times 72  (forward radius >*> rightT 5)
-- ex_circle = ex_circleR 10 >*> color Cyan >*> ex_circleR 15
ex_circle = circle 100 >*> 
            color Cyan >*> circle 150 >*> 
            color Black >*> square 75

{-

Black
Blue
Green
Cyan
Red
Magenta
Yellow
White

-}
