{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Wrapper around 'interactIO', making it easier to use a state+error-like monad.
module Util.Gloss (interactM) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Bifunctor
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

class MonadIO m => MonadGloss m world err | m -> err, m -> world where
    runUpdate :: (err -> m a) -> m a -> world -> IO (world, a)
    initWorld :: m world
instance MonadGloss IO () () where
    runUpdate _h x () = ((),) <$> x
    initWorld = pure ()
instance (MonadGloss m world err0) => MonadGloss (MaybeT m) world (Maybe err0) where
    runUpdate :: (Maybe err0 -> MaybeT m a) -> MaybeT m a -> (world -> IO (world, a))
    runUpdate h x s = runUpdate (h' . Just) (runMaybeT x >>= h'') s
      where
        h' = h'' <=< runMaybeT . h
        h'' = maybe (h' Nothing) pure
    initWorld = lift initWorld
instance (MonadGloss m world err0) => MonadGloss (ExceptT err m) world (Either err err0) where
    runUpdate :: (Either err err0 -> ExceptT err m a) -> ExceptT err m a -> (world -> IO (world, a))
    runUpdate h x s = runUpdate (h' . Right) (runExceptT x >>= h'') s
      where
        h' = h'' <=< runExceptT . h
        h'' = either (h' . Left) pure
    initWorld = lift initWorld
instance (MonadGloss m world err) => MonadGloss (ReaderT r m) (world, r) err where
    runUpdate :: (err -> ReaderT r m a) -> ReaderT r m a -> (world, r) -> IO ((world, r), a)
    runUpdate h x (s, r) = first (,r) <$> runUpdate (run . h) (run x) s
      where
        run = flip runReaderT r
    initWorld = (,) <$> lift initWorld <*> ask
instance (MonadGloss m world err) => MonadGloss (StateT s m) (world, s) err where
    runUpdate :: (err -> StateT s m a) -> StateT s m a -> (world, s) -> IO ((world, s), a)
    runUpdate h x (world0, s) = reTuple <$> runUpdate (run . h) (run x) world0
      where
        run = flip runStateT s
        reTuple (a, (b, c)) = ((a, c), b)
    initWorld = (,) <$> lift initWorld <*> get

interactM ::
    MonadGloss m world e =>
    Display ->
    Color ->
    (world -> IO (Picture, String)) ->
    (Event -> m ()) ->
    -- | Handle errors.
    (e -> m ()) ->
    (Controller -> IO ()) ->
    m ()
interactM dis col draw upd he eat = do
    s0 <- initWorld
    liftIO $
        interactIO
            dis
            col
            s0
            draw
            (\e s -> fst <$> runUpdate he (upd e) s)
            eat
