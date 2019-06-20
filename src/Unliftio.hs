{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unliftion where

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

instance forall m r a. MonadUnliftIO m => MonadUnliftIO (ReaderT r m) where
  askUnliftIO :: ReaderT r m (UnliftIO (ReaderT r m))
  askUnliftIO = ReaderT $ \(env :: r) -> do
    UnliftIO (unlift    :: forall a. m a           -> IO a)  <- askUnliftIO
    let ret = (\rdr -> do
                  let readRet = runReaderT rdr env :: m a
                  unlift readRet
              ) :: forall a. ReaderT r m a -> IO a
    pure $ UnliftIO ret

  withRunInIO :: ((forall a b. ReaderT r m a -> IO a) -> IO b) -> ReaderT r m b
  withRunInIO (inner :: ((forall a b. ReaderT r m a -> IO a) -> IO b)) = ReaderT $ \env -> do
    withRunInIO $ \(runInIO :: (forall a. m a -> IO a)) -> do
      let whatToRun' = (\rdr -> do
                          let readerRet = runReaderT rdr env
                          withRunInIO $ \unlift -> unlift readerRet
                      ) :: forall a b. ReaderT r m a -> IO a
      let whatToRun = runInIO . whatToRun'
      liftIO $ inner whatToRun
