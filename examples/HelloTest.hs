-- | This is a basic example use of 'Control.Monad.Freer.Converse' and 'Control.Monad.Freer.Test' to perform ad-hoc unit tests.
--
-- This example is intentionally a bit repetitive, in an attempt to
-- lower the mental overhead. In practice you should probably refactor
-- some low hanging fruit.

{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Freer
import           Control.Monad.Freer.Converse
import           Control.Monad.Freer.TestControl
import           Data.Functor.Classes.FreerConverse.Parametric
import           Data.Monoid
import           Data.Text                                     (Text, pack)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Hello

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, properties]

properties :: TestTree
properties = testGroup "Properties"
  [ testProperty "hello responds with your name" $
    forAll genText $ \name -> Right () == (
      pureTest hello $ do
        greeting <- expect $ \case
          PutLine v -> spy v
          other -> throwUnexpected other

        when (greeting /= ("Who are you?" :: Text)) $
          failure "Wrong greeter"

        stub $ \case
          GetLine -> return (name :: Text)
          other -> throwUnexpected other

        v <- expect $ \case
          PutLine v -> spy v
          other -> throwUnexpected other

        when (v /= ("Hello, " <> name :: Text)) $
          failure "Wrong greeter"

        return $ Right ()
      )
  ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "hello asks who you are and responds" $
    hUnitTest hello $ do

        v <- expect $ \case
          PutLine v -> spy v
          other -> throwUnexpected other

        send $ v @?= ("Who are you?" :: Text)

        stub $ \case
          GetLine -> return ("Haskell Curry" :: Text)
          other -> throwUnexpected other

        v <- expect $ \case
          PutLine v -> spy v
          other -> throwUnexpected other

        send $ v @?= ("Hello, Haskell Curry" :: Text)

  , testGroup "Verbose step by step" $
    [ testCase "hello asks who you are" $
      hUnitTest hello $ do

        v <- expect $ \case
          PutLine v -> spy v
          other -> throwUnexpected other

        send $ v @?= ("Who are you?" :: Text)

    , testCase "hello responds with your name" $
      hUnitTest hello $ do

        stub $ \case PutLine _ -> return ()

        stub $ \case
          GetLine -> return ("Haskell Curry" :: Text)
          other -> throwUnexpected other

        v <- expect $ \case
          PutLine v -> spy v
          other -> throwUnexpected other

        send $ v @?= ("Hello, Haskell Curry" :: Text)
    ]
  ]

instance ShowP Console where
  showP GetLine     = "ReadLine"
  showP (PutLine v) = ("PutLine " ++ show v)

genText :: Gen Text
genText = pack <$> arbitrary

hUnitTest :: Eff '[f, TestControl, IO] v
          -> Eff '[Converse f '[TestControl, IO] v, TestControl, IO] ()
          -> IO ()
hUnitTest program script = runM $ runTestControlError $ runConverse program script

pureTest :: Eff '[f, TestControl] v
         -> Eff '[Converse f '[TestControl] v, TestControl] (Either String ())
         -> Either String ()
pureTest program script = run $ runTestControl (return . Left) (return $ Right ()) $ runConverse program script
