-- | This module lets you handle effects using an effectful program using a
-- model of two communicating processes.
--
--  * The 'normal' computation: a computation that uses some effect @f@ and possibly others effects, @r@
--  * The handling computation: a computation that responds to the
--    effects @f@ of the normal computation by means of @Converse f@
--    and is also allowed to use the effects @r@.
--
-- This effect is called @Converse@, because it is meaningful both as a verb and as an adjective.
--
--  * The computations converse — having a one-to-one conversation where @f@ is the language
--  * The handling computation has perform the opposite\/converse of
--    the normal computation: when the normal computation /requests/ a
--    value by means of @f@ and the handler emits a @Converse f@
--    effect, the handler has to /provide/ that value.
--
module Control.Monad.Freer.Converse
  ( -- * Running tests
    runConverse
  , Converse
  , converse
  , peekEvent
  , showNext
  , module Control.Monad.Freer
  ) where
import           Control.Monad.Freer
import           Control.Monad.Freer.Internal
import           Data.Functor.Classes.FreerConverse.Parametric

-- | Handle the effects of another computation, as an effect.
--
-- For example, a handling computation may have type @Eff '[Converse f m v] a@
-- and handles effects for the 'normal' computation @Eff '[f] v@.
--
--
-- /Type parameters:/
--
-- [@f@] The effect that is communicated between the normal computation and the handling computation
-- [@r@] The remaining effects that both computations may use
-- [@v@] The result of the normal computation
-- [@a@] The result of the handling computation
data Converse f r v a where
  Converse
    :: (forall x. f x -> Eff r (Maybe x, a)) -- Maybe handle an effect. When Nothing is returned, the same effect can be handled next time, so this provides a 1-item lookahead. (FIXME: turn into haddocks when supported)
    -> (v -> Eff r a)                        -- Handle the result
    -> Converse f r v a

-- | Look at the next event without handling it
peekEvent :: (forall x. f x -> Eff r a) -> Eff (Converse f r v ': r) (Either v a)
peekEvent peeker = converse (fmap (\a -> (Nothing, Right a)) . peeker)
                            (return . Left)

-- | Show what happens next, examples:
--
--     "Next event: ReadLine"
--
--     "Done with result: 42"
showNext :: (Show v, ShowP f) => Eff (Converse f r v ': r) String
showNext = do
  p <- peekEvent (return . showP)
  return $ case p of
    Right x -> "Next event: " ++ x
    Left v  -> "Done with result: " ++ show v

inj1 :: t v -> Union (t ': r) v
inj1 = inj

-- | Called by the handling computation, to interact with the 'normal' computation. (See module description for definitions)
--
-- This is the most general way of interacting with the normal computation, reflecting the constructor of the 'Converse' type.
--
converse
  :: (forall x. f x -> Eff r (Maybe x, b)) -- ^ Handle an effect emitted by the normal computation. This may produce other effects in @r@. In order to handle the effect, return a @(Just x, <...>)@. The right hand side of the tuple may be used to return a value to be used later on by the handling computation (@b@ also occurs in the return value)
  -> (v -> Eff r b) -- ^ Handle the case where the normal computation has completed and returned a value of type @v@.
  -> Eff (Converse f r v ': r) b -- ^ A computation that should run in the handling computation.
converse f f' = E (inj1 (Converse f f')) (tsingleton return)

-- | Zips together the two communicating computations, the normal computation
-- that uses effect @f@ and the handling computation that uses effect @Converse f@
--
-- The handling computation gets to run effects (the @r@ parameter) first
-- whenever a 'scheduling' choice presents itself.
--
runConverse
  :: Eff (f ': r) v               -- ^ The normal computation
  -> Eff (Converse f r v ': r) b  -- ^ The handling computation
  -> Eff r b                      -- ^ A runnable combined computation
runConverse _subject _script@(Val b) = return b
runConverse subject script@(E x qScript) = case decomp x of
    Left scriptSideEffect -> E  scriptSideEffect
                                (tsingleton (qComp qScript (runConverse subject)))
    Right (Converse onEvent onResult) -> case subject of
      Val subjectResult -> do
        c <- onResult subjectResult
        runConverse subject (qScript `qApp` c)
      E subjectEffect qSubject -> case decomp subjectEffect of
        Left sideEffect -> E sideEffect (tsingleton (qComp qSubject c))
           where c s = runConverse s script
        Right event -> do
          (maybeReply, spy) <- onEvent event
          case maybeReply of
            Just reply -> runConverse (qSubject `qApp` reply) (qScript `qApp` spy)
            Nothing -> runConverse subject (qScript `qApp` spy)
