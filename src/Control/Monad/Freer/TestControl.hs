-- | This module provides functions and a TestControl effect for implmenting
-- unit tests with 'Control.Monad.Freer.Converse'.

module Control.Monad.Freer.TestControl
  ( TestControl
  , runTestControl
  , runTestControlData
  , runTestControlData_
  , runTestControlError
  , TestExitStatus(..)

      -- * Controlling the test
  , fulfilled
  , throwUnexpected
  , throwExpecting
  , failure

      -- * Interacting with the test subject
  , expect
  , collect
  , stub
  , stubs
  , result
  , result_
  , converse

      -- * Arguments to 'expect', 'stubs', etc
  , spy

  ) where

import           Control.Arrow                                 (first)
import           Control.Monad                                 (join)
import           Control.Monad.Freer.Converse
import           Control.Monad.Freer.Exception
import           Data.Functor.Classes.FreerConverse.Parametric

-- | An effect for terminating the test when either the test has failed, or the
-- goal of the test has been fulfilled without problems but need not continue
-- the normal flow of execution.
type TestControl = Exc TestExitStatus
-- | Interruption of a test run.
data TestExitStatus = TestFulfilled -- ^ The goal of the test was accomplished and the test need not continue.
                    | TestFailed String -- ^ A problem was detected

-- | The goal of the test has been accomplished. Stops further execution of the
-- test. Results in a successful test result.
fulfilled :: Member TestControl r => Eff r a
fulfilled = throwError TestFulfilled

-- | Handler for 'TestControl' effects. Runs the computation (a test) and
--
--   * calls into the first argument on failure,
--   * calls into the second argument on 'fulfilled' or
--   * returns the value produced by the test (often just '()').
--
-- Note that the @r@ parameter determines what (computational or I/O)
-- effects are required/allowed for running the test. This makes it
-- possible to write pure tests, tests that explore all branches of
-- nondeterministic choices, tests that read from files dynamically,
-- etc.
runTestControl :: (String -> Eff r a) -- ^ On failure
               -> Eff r a             -- ^ On fulfill
               -> Eff (TestControl ': r) a -- ^ The test, with @TestControl@
               -> Eff r a             -- ^ The test, without @TestControl@
runTestControl onFail onFulfill t = runError t >>= \testResult -> case testResult of
  Left (TestFailed s) -> onFail s
  Left TestFulfilled  -> onFulfill
  Right x             -> return x

-- | Runs a test, letting it terminate early, as appropriate.
--
-- Like 'runTestControl' but for those who like to pattern match instead.
runTestControlData :: Eff (TestControl : r) a -> Eff r (Either String (Maybe a))
runTestControlData a = runTestControl (return . Left) (return (Right Nothing)) (fmap (Right . Just) a)

-- | Runs a test, letting it terminate early, as appropriate.
--
-- Like 'runTestControlData' but will not return a value from the test.
runTestControlData_ :: Eff (TestControl : r) a -> Eff r (Either String ())
runTestControlData_ a = runTestControl (return . Left) (return (Right ())) (Right () <$ a)

-- | Runs a test, letting it terminate early, as appropriate.
--
-- Throws an error with 'error' on failure.
runTestControlError :: Eff (TestControl ': r) () -> Eff r ()
runTestControlError = runTestControl (error . showString "Test failed: ") (return ())

-- | Terminates the test with error, showing provided reason and next event.
failure :: (Member TestControl r, Show v, ShowP f) => String -- ^ Reason for test failure
  -> Eff (Converse f r v ': r) a
failure reason = do
  nextEvent <- showNext
  throwError $ TestFailed $ reason ++ "\nNext event: " ++ nextEvent

-- | Terminates test as a failure by showing the expectation and the event.
throwExpecting
  :: ( ShowP f
     , Member TestControl r
     )
  => String -- ^ Noun phrase describing expectation
  -> f a -- ^ Unexpected event
  -> Eff r b
throwExpecting expectation v = throwError $ TestFailed $ "Expecting " ++ expectation ++ ", but got " ++ showP v

-- | Throw an unexpected event error
throwUnexpected :: (ShowP f, Member TestControl r) => f a -> Eff r b
throwUnexpected v = throwError $ TestFailed $ "Unexpected effect: " ++ showP v

-- | When an event occurs, provide a value @a@ for the test subject and a value @b@
-- for the test script.
expect
  :: Member TestControl r
  => (forall a. f a -> Eff r (a, b))
  -> Eff (Converse f r v ': r) b
expect f = converse (\x -> first Just <$> f x) (const $ throwError $ TestFailed "Unexpected program termination: effect expected.")

-- | When an event occurs, provide a value to the test subject.
--
-- Like 'expect', but does not return a value to the test script.
stub :: Member TestControl r
  => (forall b. f b -> Eff r b)
  -> Eff (Converse f r v ': r) ()
stub f = expect (fmap (\x -> (x,())) <$> f)

-- | Provide a value to the test subject, if and as long as matching
-- events occur. Matching stops when Nothing is returned from the passed function.
--
-- Returns the number of events that have been matched.
collect :: (forall a. f a -> Eff r (Maybe (a, b))) -> Eff (Converse f r v ': r) [b]
collect f = do
  join $ converse (
            \x -> do
              replyMaybe <- f x
              case replyMaybe of
                Just (reply, spied) -> return (Just reply, (spied :) <$> collect f)
                Nothing -> return (Nothing, (return []))
            ) (const $ return (return []))

-- | Like 'collect', but simpler because it does not return a value to
-- the test script.
stubs
  :: (forall b. f b -> Eff r (Maybe b))
  -> Eff (Converse f r v ': r) ()
stubs f = do
  join $ converse (
            \x -> do
              replyMaybe <- f x
              case replyMaybe of
                Just reply -> return (Just reply, stubs f)
                Nothing    -> return (Nothing, (return ()))
            ) (const $ return (return ()))

-- | Retrieve the result of the program. Fails if an effect of type
-- @f@ is still pending.
result :: (Member TestControl r, ShowP f) => Eff (Converse f r v ': r) v
result = converse throwUnexpected return

-- | Like 'result' but more generic because it does not attempt to
-- show the unexpected effect in the error message.
result_ :: (Member TestControl r) => Eff (Converse f r v ': r) v
result_ = converse (const $ throwError $ TestFailed $
                      "Expected program termination with result, but got an effect instead."
                   )
                   return



-- | Provide empty response to test subject, pass argument to test script
spy :: (Monad m, Monoid mm) => a -> m (mm, a)
spy a = return (mempty, a)
