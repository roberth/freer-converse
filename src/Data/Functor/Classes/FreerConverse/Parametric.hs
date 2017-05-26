-- | When you have a value of @f a@, but you can not possibly have
-- some constraint on @a@, you often can not use type class instances
-- of @f a@, because they require instances for @a@. This module
-- provides type classes that mirror existing type classes but do not
-- have the constraint on @a@, usually at the cost of functionality.

module Data.Functor.Classes.FreerConverse.Parametric
  ( ShowP(..)
  , defaultShowsPrecP
  ) where
import           Control.Applicative (Const)
import           Data.Proxy          (Proxy)

-- | Without a @Show a@ constraint it is impossible to show a value of type
-- @Maybe a@. It is, however, possible to distinguish between the @Just _@ and @Nothing cases.
--
-- This type class provides this functionality. It is similar to @Show1@ in @transformers@' @Data.Functor.Classes@, but without the @Show a@ constraint. Therefore, it can be used on quantified, unconstrained types like @forall a. Maybe a@.
class ShowP f where
  -- | Like 'showsPrec', but without using values of type @a@
  showsPrecP :: Int -> f a -> ShowS
  showsPrecP _ x s = showP x ++ s
  -- | Like 'show', but without using values of type @a@
  showP :: f a -> String
  showP x = showsPrecP 0 x ""

-- | Implements 'showsPrecP' using an instance 'Functor'@ f@ and an instance 'Show'@ a => @'Show'@ (f a)@
--
-- The values of type @a@ will be shown as @_@
defaultShowsPrecP :: (Functor f, Show (f Placeholder)) => Int -> f a -> ShowS
defaultShowsPrecP n x = showsPrec n (fmap (const Placeholder) x)

-- Not exported. For use by defaultShowsPrecP to get the 'right' Show instance to feed into the Show a => Show (f a) instance.
data Placeholder = Placeholder
instance Show Placeholder where
  show _ = "_"

-- | Equal to the Show instance
instance ShowP Proxy where
  showsPrecP = showsPrec
  showP = show

-- | Equal to the Show instance
instance Show a => ShowP (Const a) where
  showsPrecP = showsPrec
  showP = show

instance ShowP Maybe where
  showsPrecP = defaultShowsPrecP

instance ShowP [] where
  showsPrecP = defaultShowsPrecP

instance ShowP IO where
  showP _ = "<IO>"

instance Show a => ShowP (Either a) where
  showsPrecP = defaultShowsPrecP

instance Show a => ShowP ((,) a) where
  showsPrecP = defaultShowsPrecP
