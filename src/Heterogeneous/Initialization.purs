module Heterogeneous.Initialization where

import Prelude

import Data.Symbol (class IsSymbol)
import Prim.Row as Row
import Prim.RowList (RowList)
import Prim.RowList as RL
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Proxy (Proxy(..))

class Initialization f a where
  initialization :: f -> a

class InitializationWithIndex f i a where
  initializationWithIndex :: f -> i -> a

newtype ConstInitialization f = ConstInitialization f

instance
  Initialization f a =>
  InitializationWithIndex (ConstInitialization f) ix a
  where
  initializationWithIndex (ConstInitialization f) _ = initialization f

class HInitialize f a where
  hinitialize :: f -> a

class HInitializeWithIndex f a where
  hinitializeWithIndex :: f -> a

instance
  ( RL.RowToList rout rl
  , InitializeWithIndexRL rl (ConstInitialization fn) () rout
  ) =>
  HInitialize fn { | rout }
  where
  hinitialize = hinitializeWithIndex <<< ConstInitialization

instance
  ( RL.RowToList rout rl
  , InitializeWithIndexRL rl fn () rout
  ) =>
  HInitializeWithIndex fn { | rout }
  where
  hinitializeWithIndex fn = Builder.build (initializeWithIndexRL (Proxy :: _ rl) fn) {}

class InitializeWithIndexRL (xs :: RowList Type) f (as :: Row Type) (bs :: Row Type) | xs -> as where
  initializeWithIndexRL :: forall proxy. proxy xs -> f -> Builder { | as } { | bs }

instance
  ( IsSymbol sym
  , InitializationWithIndex f (Proxy sym) a
  , InitializeWithIndexRL rest f as bs'
  , Row.Cons sym a bs' bs
  , Row.Lacks sym bs'
  ) =>
  InitializeWithIndexRL (RL.Cons sym a rest) f as bs
  where
  initializeWithIndexRL _ f = Builder.insert prop (initializationWithIndex f prop)
    <<< initializeWithIndexRL (Proxy :: _ rest) f
    where
    prop = Proxy :: Proxy sym

instance InitializeWithIndexRL RL.Nil fn as as where
  initializeWithIndexRL _ _ = identity