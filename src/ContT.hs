{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ContT where

import System.IO
import Control.Monad.IO.Class

newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }


instance Functor (ContT r m) where
  fmap f m = ContT $ \_return ->
    runContT m $ \a -> _return (f a)

instance Applicative (ContT r m) where
  pure a = ContT $ \_return -> _return a

  ff <*> fa = ContT $ \_return ->
    runContT ff $ \f ->
      runContT fa $ \a ->
        _return (f a)

instance Monad (ContT r m) where
  ma >>= f = ContT $ \_return ->
    runContT ma $ \a ->
      runContT (f a) _return

instance MonadIO m => MonadIO (ContT r m) where
  liftIO ioa = ContT $ \_return ->
    liftIO ioa >>= _return


test :: IO ()
test = flip runContT pure $ do
  f1 <- ContT $ withFile "/tmp/t1" WriteMode
  f2 <- ContT $ withFile "/tmp/t2" WriteMode
  liftIO $ hPutStrLn f1 "test"
  liftIO $ hPutStrLn f2 "test"
  pure ()
