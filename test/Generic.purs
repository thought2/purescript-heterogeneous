module Test.Generic where

import Prelude

import Data.Generic.Rep (class Generic, Argument, Constructor, Sum, from)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)

data Baz = Foo Int | Bar Boolean

derive instance Generic Baz _

type RepBaz' = Sum
  (Constructor "Foo" (Argument String))
  (Constructor "Bar" (Argument String))

data Show = Show

instance Show a => Mapping Show a String where
  mapping _ = show

go :: forall a rep rep'. Generic a rep => HMap Show rep rep' => a -> rep'
go x = hmap Show $ from x

test1 :: RepBaz'
test1 = go $ Foo 3

test2 :: RepBaz'
test2 = go $ Bar true
