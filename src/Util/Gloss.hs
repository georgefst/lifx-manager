{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Wrapper around 'interactIO', making it easier to use a state+error-like monad.
module Util.Gloss (interactM) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

class MonadIO m => MonadGloss m world err | m -> err, m -> world where
    runUpdate :: (err -> IO a) -> m a -> world -> IO (world, a)
    initWorld :: m world
instance MonadGloss IO () () where
    runUpdate _h x () = ((),) <$> x
    initWorld = pure ()
instance (MonadGloss m world err0) => MonadGloss (ExceptT err m) world (Either err err0) where
    runUpdate :: (Either err err0 -> IO a) -> ExceptT err m a -> (world -> IO (world, a))
    runUpdate h x s = runUpdate (h . Right) (runExceptT x >>= either (liftIO . h . Left) pure) s
    initWorld = lift initWorld
instance (MonadGloss m world err) => MonadGloss (ReaderT r m) (world, r) err where
    runUpdate :: (err -> IO a) -> ReaderT r m a -> (world, r) -> IO ((world, r), a)
    runUpdate h x (s, r) = first (,r) <$> runUpdate h (runReaderT x r) s
    initWorld = (,) <$> lift initWorld <*> ask
instance (MonadGloss m world err) => MonadGloss (StateT s m) (world, s) err where
    runUpdate :: (err -> IO a) -> StateT s m a -> (world, s) -> IO ((world, s), a)
    runUpdate h x (world0, s) =
        (\(x', (y, z)) -> ((x', z), y))
            <$> runUpdate (fmap (,s) . h) (runStateT x s) world0
    initWorld = (,) <$> lift initWorld <*> get

interactM ::
    MonadGloss m world e =>
    Display ->
    Color ->
    (world -> IO (Picture, String)) ->
    (Event -> m ()) ->
    -- | Handle errors.
    (e -> IO ()) ->
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
