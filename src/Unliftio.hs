{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Unliftio where

import Control.Monad.Reader

newtype UnliftIO m = UnliftIO { unliftIO :: forall a. m a -> IO a }

class (Monad m, MonadIO m) => MonadUnliftIO m where
  askUnliftIO :: m (UnliftIO m)
  withRunInIO :: ((forall a. m a -> IO a) -> IO b) -> m b

instance MonadUnliftIO IO where
  askUnliftIO :: IO (UnliftIO IO)
  askUnliftIO = pure (UnliftIO id)

  withRunInIO :: ((forall a. IO a -> IO a) -> IO b) -> IO b
  withRunInIO inner = inner id

instance forall m r. MonadUnliftIO m => MonadUnliftIO (ReaderT r m) where
  askUnliftIO :: ReaderT r m (UnliftIO (ReaderT r m))
  askUnliftIO = ReaderT $ \(env :: r) -> do
    UnliftIO (unlift    :: forall a. m a           -> IO a)  <- askUnliftIO
    let ret = (\rdr -> do
                  let readRet = runReaderT rdr env :: m a
                  unlift readRet
              ) :: forall a. ReaderT r m a -> IO a
    pure $ UnliftIO ret

  withRunInIO :: ((forall a1. ReaderT r m a1 -> IO a1) -> IO b) -> ReaderT r m b
  withRunInIO (inner :: ((forall a2. ReaderT r m a2 -> IO a2) -> IO b)) = ReaderT $ \env -> do
    withRunInIO $ \(runInIO :: (forall a3. m a3 -> IO a3)) -> do
      let whatToRun' = runInIO . flip runReaderT env :: ReaderT r m a4 -> IO a4
      liftIO $ (inner whatToRun' :: IO b)


jonk :: forall a b. (a -> b) -> ((a -> Int) -> Int) -> ((b -> Int) -> Int)
jonk fab faii = \fbi -> faii (fbi . fab)

zoop :: forall a b. (a -> b -> b) -> b -> [a] -> b
zoop _ b [] = b
zoop f b (a:as) = f a (zoop f b as)
