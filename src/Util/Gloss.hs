{-# LANGUAGE TypeFamilies #-}

{-TODO
upstream, or release separate library
clean up variable names etc.
somehow avoid awkward `Either () err` (~ `err`) error type for `EitherT err IO`?
    similar for Reader etc.
    probably difficult/impossible due to need for overlapping type family instances
-}

-- | Wrapper around 'interactIO', making it easier to use a state+error-like monad.
module Util.Gloss (interactM) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Bifunctor
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

class MonadIO m => MonadGloss m where
    type World m
    type Error m
    runUpdate :: (Error m -> m a) -> m a -> World m -> IO (World m, a)
    initWorld :: m (World m)
instance MonadGloss IO where
    type World IO = ()
    type Error IO = ()
    runUpdate _h x () = ((),) <$> x
    initWorld = pure ()
instance (MonadGloss m) => MonadGloss (MaybeT m) where
    type World (MaybeT m) = World m
    type Error (MaybeT m) = (Maybe (Error m))
    runUpdate h x s = runUpdate (h' . Just) (runMaybeT x >>= h'') s
      where
        h' = h'' <=< runMaybeT . h
        h'' = maybe (h' Nothing) pure
    initWorld = lift initWorld
instance (MonadGloss m) => MonadGloss (ExceptT err m) where
    type World (ExceptT err m) = World m
    type Error (ExceptT err m) = (Either err (Error m))
    runUpdate h x s = runUpdate (h' . Right) (runExceptT x >>= h'') s
      where
        h' = h'' <=< runExceptT . h
        h'' = either (h' . Left) pure
    initWorld = lift initWorld
instance (MonadGloss m) => MonadGloss (ReaderT r m) where
    type World (ReaderT r m) = (World m, r)
    type Error (ReaderT r m) = Error m
    runUpdate h x (s, r) = first (,r) <$> runUpdate (run . h) (run x) s
      where
        run = flip runReaderT r
    initWorld = (,) <$> lift initWorld <*> ask
instance (MonadGloss m) => MonadGloss (StateT s m) where
    type World (StateT s m) = (World m, s)
    type Error (StateT s m) = Error m
    runUpdate h x (s, s') = reTuple <$> runUpdate (run . h) (run x) s
      where
        run = flip runStateT s'
        reTuple (a, (b, c)) = ((a, c), b)
    initWorld = (,) <$> lift initWorld <*> get

interactM ::
    MonadGloss m =>
    Display ->
    Color ->
    (World m -> IO Picture) ->
    (Event -> m ()) ->
    -- | Handle errors.
    (Error m -> m ()) ->
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
