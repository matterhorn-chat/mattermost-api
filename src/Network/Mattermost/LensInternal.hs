{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}

module Network.Mattermost.LensInternal (Lens', makeLens) where

-- This is the same type alias as in @Control.Lens@, and so can be used
-- anywhere lenses are needed.
type Lens' a b = forall f. Functor f => (b -> f b) -> (a -> f a)

-- Create a lens by combining a getter and a setter.
makeLens :: (a -> b) -> (b -> a -> a) -> Lens' a b
makeLens get set f a = (`set` a) `fmap` f (get a)
