
{-# LANGUAGE FlexibleContexts #-}

module Lib
    ( someFunc
    ) where


import System.IO
import System.Console.ANSI
import Control.Monad
import Data.List     (elemIndices, intercalate, transpose)
import System.Random
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import qualified Text.Read as R

-- util
setNoBuffering :: IO ()
setNoBuffering = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
-- util

type Size       = (Int, Int)
newtype Config  = Config {maxGrid :: Size} deriving Show

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

-- init
initConfig :: IO Config
initConfig = do
  Just size <- getTerminalSize
  return $ Config
    {
      maxGrid   = size
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
  
renderGame :: (MonadReader Config m, MonadState Game m, MonadIO m) => m ()
renderGame = do
  config <- ask
  game <- get
  liftIO clearScreen
  liftIO $ forM_ (board game) (putStrLn . show)
  return ()


someFunc = do
  game <- initGame
  config <- initConfig
  setNoBuffering
  runStateT (runReaderT play config) game

play :: ReaderT Config (StateT Game IO) ()
play = do
  game <- get
  b1 <- liftIO $ addTile (board game) >>= addTile
  put $ game {board = b1}
  renderGame
--  liftIO $ printBoard (board game)
--  liftIO $ printBoard (board game)
--  printBoard b1


--someFunc = runStateT play

--deductChances :: ReaderT Secret (StateT Chance IO) ()
--deductChances = do
--  modify (subtract 1)
--  chances <- get
--  lift . lift . putStrLn $ "You have " ++ show chances ++ " chances."
--  if chances > 0
--  then process
--  else liftIO $ putStrLn "Sorry! You lose."