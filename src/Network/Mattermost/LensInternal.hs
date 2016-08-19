{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}

module Network.Mattermost.LensInternal (Lens', makeLens) where


-- We need these to write get and set
newtype C a b = C { fromC :: a } deriving (Functor)
newtype I a = I { fromI :: a } deriving (Functor)

-- This is the same type alias as in @Control.Lens@, and so can be used
-- anywhere lenses are needed.
type Lens' a b = forall f. Functor f => (b -> f b) -> (a -> f a)

get :: Lens' a b -> a -> b
get lens a = fromC (lens C a)

set :: Lens' a b -> b -> a -> a
set lens x a = fromI (lens (const I x) a)

makeLens :: (a -> b) -> (b -> a -> a) -> Lens' a b
makeLens get set f a = (`set` a) `fmap` f (get a)
