module Test.Initialization where

import Heterogeneous.Initialization (class Initialization, class InitializationWithIndex, hinitialize, hinitializeWithIndex)
import Type.Proxy (Proxy)

-- Test 1
data FillRec = FillRec

instance Initialization FillRec Int where
  initialization _ = 1

instance Initialization FillRec Char where
  initialization _ = 'a'

myRec :: { foo :: Int, bar :: Int, baz :: Char }
myRec = hinitialize FillRec

-- Test 2

data FillRec2 = FillRec2

instance InitializationWithIndex FillRec2 (Proxy "foo") Int where
  initializationWithIndex _ _ = 1

else instance InitializationWithIndex FillRec2 ix Int where
  initializationWithIndex _ _ = 2

instance InitializationWithIndex FillRec2 ix Char where
  initializationWithIndex _ _ = 'a'

myRec2 :: { foo :: Int, bar :: Int, baz :: Char }
myRec2 = hinitializeWithIndex FillRec2
