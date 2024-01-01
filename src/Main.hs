{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (forever, void)
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Functor ((<&>))
import Data.IORef

class (Monad m) => MonadGame m where
    getIn :: m String
    update :: Int -> m ()
    logLN :: String -> m ()
    logScore :: m ()

newtype GameEnv = GameEnv {score :: Int}

newtype Game a = Game {runGameM :: StateT GameEnv IO a}
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadGame Game where
    logLN = liftIO . putStrLn
    update n = Game $ do
        GameEnv{..} <- get
        let g = GameEnv{score = score + n}
        put g
    getIn = liftIO getLine
    logScore = Game $ do
        GameEnv{..} <- get
        liftIO (putStrLn $ "Your score is: " ++ show score)

loop :: Game ()
loop = do
    logLN "Welcome to guessing game!"
    forever $ do
        logLN "Please enter a number:"
        i <- getIn
        if i == "5"
            then update 1 >> logScore
            else logLN "You Incorrect :)"

newGame :: GameEnv
newGame = GameEnv{score = 0}

runGame :: Game a -> IO ()
runGame = void . flip runStateT newGame . runGameM

main :: IO ()
main = runGame loop
