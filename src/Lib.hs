
{-# LANGUAGE FlexibleContexts #-}

module Lib
    ( someFunc
    ) where


import System.IO
import System.Console.ANSI
import Control.Monad
import Data.List     (elemIndices, transpose)
import System.Random
import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent
import System.Exit (die)

-- util
setNoBuffering :: IO ()
setNoBuffering = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
-- util

type Board      = [[Int]]
type Score      = Int
data Game       = Game
  {
    board     :: Board,
    score     :: Score
  }
  deriving Show

initGame :: IO Game
initGame = return $ Game
  {
    board     = replicate 4 (replicate 4 0),
    score     = 0
  }

-- Returns tuples of the indices of all of the empty tiles
emptyTiles :: Board -> [(Int, Int)]
emptyTiles = concatMap (uncurry search) . zip [0..3]
  where search b = zip (replicate 4 b) . elemIndices 0

-- Given a point, update replaces the value at the point on the board with the given value
updateTile :: (Int, Int) -> Int -> Board -> Board
updateTile (rowI, columnI) value = updateIndex (updateIndex (const value) columnI) rowI
  where updateIndex fn i list = take i list ++ fn (head $ drop i list) : tail (drop i list)

-- Adds a tile to a random empty spot.
-- 90% of the time the tile is a 2, 10% of the time it is a 4
addTile :: Board -> IO Board
addTile b = do
  let tiles = emptyTiles b
  newPoint <- randomRIO (0, length tiles - 1) >>= return . (tiles !!)
  newValue <- randomRIO (1, 10 :: Int) >>= return . \x -> if x == 1 then 4 else 2
  return $ updateTile newPoint newValue b

-- Board pretty printing
printBoard :: Board -> IO ()
printBoard p = do
--  clearScreen
  forM_ p (putStrLn . show)
  
renderGame :: (MonadState Game m, MonadIO m) => m ()
renderGame = do
  game <- get
  liftIO $ forM_ (board game) (putStrLn . show)
  liftIO $ putStrLn " "
  liftIO $ putStrLn $ "Score: " ++ show (score game)
  return ()

-- broadcast
data Event
  = TickEvent
  | KeyEvent Char deriving Show

castKey :: Chan Event -> IO ()
castKey chan = forever $ do
  hSetEcho stdin False
  c <- getChar
  writeChan chan (KeyEvent c)

castTick :: Chan Event -> IO ()
castTick chan = forever $ do
  threadDelay (2 * (10 ^ 5))
  writeChan chan TickEvent

someFunc = do
  game <- initGame
  boardWithTiles <- liftIO $ addTile (board game) >>= addTile
  let readyGame = game {board = boardWithTiles}
  setNoBuffering
  chan <- newChan
  forkIO $ castTick chan
  forkIO $ castKey chan
  runStateT (play chan) readyGame


play :: Chan Event -> StateT Game IO ()
play chan = forever $ do
  game <- get
  if stalled (board game) then do
    liftIO $ die "Game Over. You Lose!"
    else if completed (board game) then
    liftIO $ die "You Won"
      else do
        event <- liftIO $ readChan chan
        case event of
            TickEvent   -> do liftIO clearScreen; renderGame; return ()
            KeyEvent k  -> do
              case k of
                'w' -> updateBoard North
                's' -> updateBoard South
                'd' -> updateBoard East
                'a' -> updateBoard West
                _   -> return ()


updateBoard :: Direction -> StateT Game IO ()
updateBoard direction = do
    game <- get
    let b0 = slide direction (board game)
    b1 <- liftIO $ addTile b0
    if b0 == board game then
      return ()
      else do
    let s1 = calculateScore b1
    put $ game {board = b1, score = s1}

calculateScore :: Board -> Int
calculateScore b = sum $ map (\x -> sum x) b


data Direction = North | East | South | West

-- Tells us if the game is over because there are no valid moves left
stalled :: Board -> Bool
stalled b = all stalled' b && all stalled' (transpose b)
  where stalled' row = notElem 0 row && noNeighbors row
        noNeighbors [ ] = True
        noNeighbors [_] = True
        noNeighbors (x:y:zs)
          | x == y    = False
          | otherwise = noNeighbors (y:zs)

-- Tells us if the player won the game by getting a 2048 tile
completed :: Board -> Bool
completed b = any (elem 2048) b

slideLeft :: Board -> Board
slideLeft = map slideRow
  where slideRow [ ] = [ ]
        slideRow [x] = [x]
        slideRow (x:y:zs)
          | x == 0 = slideRow (y : zs) ++ [0]
          | y == 0 = slideRow (x : zs) ++ [0] -- So that things will combine when 0's are between them
          | x == y = (x + y) : slideRow zs ++ [0]
          | otherwise = x : slideRow (y : zs)

slide :: Direction -> Board -> Board
slide North = transpose . slideLeft . transpose
slide East  = map reverse . slideLeft . map reverse
slide South = transpose . map reverse . slideLeft . map reverse . transpose
slide West  = slideLeft