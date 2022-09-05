module Heterogeneous.Mapping where

import Prelude

import Data.Either (Either(..))
import Data.Functor.App (App(..))
import Data.Functor.Variant (VariantF)
import Data.Functor.Variant as VariantF
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Generic.Rep as R
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Prim.Row as Row
import Prim.RowList (RowList)
import Prim.RowList as RL
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Proxy (Proxy(..))

class Mapping f a b | f a -> b where
  mapping :: f -> a -> b

class MappingWithIndex f i a b | f i a -> b where
  mappingWithIndex :: f -> i -> a -> b

instance mappingFunction :: Mapping (a -> b) a b where
  mapping k = k

newtype ConstMapping f = ConstMapping f

instance constMapping ::
  Mapping f a b =>
  MappingWithIndex (ConstMapping f) ix a b
  where
  mappingWithIndex (ConstMapping f) _ = mapping f

class HMap f a b | f a -> b where
  hmap :: f -> a -> b

class HMapWithIndex f a b | f a -> b where
  hmapWithIndex :: f -> a -> b

instance hmapApp ::
  ( Functor f
  , Mapping fn a b
  ) =>
  HMap fn (App f a) (App f b)
  where
  hmap f (App a) =
    App (mapping f <$> a)

instance hmapWithIndexApp ::
  ( FunctorWithIndex i f
  , MappingWithIndex fn i a b
  ) =>
  HMapWithIndex fn (App f a) (App f b)
  where
  hmapWithIndex f (App a) =
    App (mapWithIndex (mappingWithIndex f) a)

instance hmapRecord ::
  ( RL.RowToList rin rl
  , MapRecordWithIndex rl (ConstMapping fn) rin rout
  ) =>
  HMap fn { | rin } { | rout }
  where
  hmap =
    Builder.build
      <<< mapRecordWithIndexBuilder (Proxy :: Proxy rl)
      <<< ConstMapping

instance hmapWithIndexRecord ::
  ( RL.RowToList rin rl
  , MapRecordWithIndex rl fn rin rout
  ) =>
  HMapWithIndex fn { | rin } { | rout }
  where
  hmapWithIndex =
    Builder.build
      <<< mapRecordWithIndexBuilder (Proxy :: Proxy rl)

class MapRecordWithIndex (xs :: RowList Type) f (as :: Row Type) (bs :: Row Type) | xs f -> bs, xs -> as where
  mapRecordWithIndexBuilder :: forall proxy. proxy xs -> f -> Builder { | as } { | bs }

instance mapRecordWithIndexCons ::
  ( IsSymbol sym
  , MappingWithIndex f (Proxy sym) a b
  , MapRecordWithIndex rest f as bs'
  , Row.Cons sym a bx bs'
  , Row.Cons sym b bx bs
  ) =>
  MapRecordWithIndex (RL.Cons sym a rest) f as bs
  where
  mapRecordWithIndexBuilder _ f =
    Builder.modify prop (mappingWithIndex f prop)
      <<< mapRecordWithIndexBuilder (Proxy :: Proxy rest) f
    where
    prop = Proxy :: Proxy sym

instance mapRecordWithIndexNil :: MapRecordWithIndex RL.Nil fn as as where
  mapRecordWithIndexBuilder _ _ = identity

instance hmapTuple ::
  ( Mapping fn a a'
  , Mapping fn b b'
  ) =>
  HMap fn (Tuple a b) (Tuple a' b')
  where
  hmap fn (Tuple a b) =
    Tuple (mapping fn a) (mapping fn b)

instance hmapEither ::
  ( Mapping fn a a'
  , Mapping fn b b'
  ) =>
  HMap fn (Either a b) (Either a' b')
  where
  hmap fn = case _ of
    Left a -> Left (mapping fn a)
    Right b -> Right (mapping fn b)

instance hmapVariant ::
  ( RL.RowToList rin rl
  , MapVariantWithIndex rl (ConstMapping fn) rin rout
  ) =>
  HMap fn (Variant rin) (Variant rout)
  where
  hmap =
    mapVariantWithIndex (Proxy :: Proxy rl) <<< ConstMapping

instance hmapWithIndexVariant ::
  ( RL.RowToList rin rl
  , MapVariantWithIndex rl fn rin rout
  ) =>
  HMapWithIndex fn (Variant rin) (Variant rout)
  where
  hmapWithIndex =
    mapVariantWithIndex (Proxy :: Proxy rl)

class MapVariantWithIndex (xs :: RowList Type) f (as :: Row Type) (bs :: Row Type) | xs f -> bs, xs -> as where
  mapVariantWithIndex :: forall proxy. proxy xs -> f -> Variant as -> Variant bs

instance mapVariantWithIndexCons ::
  ( IsSymbol sym
  , Row.Cons sym a r1 r2
  , Row.Cons sym b r3 r4
  , MappingWithIndex fn (Proxy sym) a b
  , MapVariantWithIndex rest fn r1 r4
  ) =>
  MapVariantWithIndex (RL.Cons sym a rest) fn r2 r4
  where
  mapVariantWithIndex _ fn =
    mapVariantWithIndex (Proxy :: Proxy rest) fn
      # Variant.on label (Variant.inj label <<< mappingWithIndex fn label)
    where
    label = Proxy :: Proxy sym

instance mapVariantWithIndexNil :: MapVariantWithIndex RL.Nil fn () r where
  mapVariantWithIndex _ _ = Variant.case_

instance hmapVariantF ::
  ( RL.RowToList rin rl
  , MapVariantFWithIndex rl (ConstMapping fn) rin rout x y
  ) =>
  HMap fn (VariantF rin x) (VariantF rout y)
  where
  hmap =
    mapVariantFWithIndex (Proxy :: Proxy rl) <<< ConstMapping

instance hmapWithIndexVariantF ::
  ( RL.RowToList rin rl
  , MapVariantFWithIndex rl fn rin rout x y
  ) =>
  HMapWithIndex fn (VariantF rin x) (VariantF rout y)
  where
  hmapWithIndex =
    mapVariantFWithIndex (Proxy :: Proxy rl)

class MapVariantFWithIndex (xs :: RowList (Type -> Type)) f (as :: Row (Type -> Type)) (bs :: Row (Type -> Type)) x y | xs f x -> as bs y where
  mapVariantFWithIndex :: forall proxy. proxy xs -> f -> VariantF as x -> VariantF bs y

instance mapVariantFWithIndexCons ::
  ( IsSymbol sym
  , Row.Cons sym a r1 r2
  , Row.Cons sym b r3 r4
  , MappingWithIndex fn (Proxy sym) (a x) (b y)
  , MapVariantFWithIndex rest fn r1 r4 x y
  , Functor b
  ) =>
  MapVariantFWithIndex (RL.Cons sym a rest) fn r2 r4 x y
  where
  mapVariantFWithIndex _ fn =
    mapVariantFWithIndex (Proxy :: Proxy rest) fn
      # VariantF.on label (VariantF.inj label <<< mappingWithIndex fn label)
    where
    label = Proxy :: Proxy sym

instance mapVariantFWithIndexNil :: MapVariantFWithIndex RL.Nil fn () r x y where
  mapVariantFWithIndex _ _ = VariantF.case_


instance hmapNoConstructors :: HMap c R.NoConstructors R.NoConstructors where
  hmap _ = identity

instance HMap c R.NoArguments R.NoArguments
  where
  hmap _ = identity

instance
  ( HMap c a a'
  , HMap c b b'
  ) =>
  HMap c (R.Product a b) (R.Product a' b')
  where
  hmap c (R.Product x y) = R.Product (hmap c x) (hmap c y)

instance
  ( HMap c a a'
  , HMap c b b'
  ) =>
  HMap c (R.Sum a b) (R.Sum a' b')
  where
  hmap opts  (R.Inl x) = R.Inl (hmap opts x)
  hmap opts (R.Inr x) = R.Inr (hmap opts x)

instance Mapping c a b => HMap c  (R.Argument a) (R.Argument b)
  where
  hmap c (R.Argument x) = R.Argument (mapping c x)

instance HMap c a a' => HMap c (R.Constructor s a) (R.Constructor s a')
  where
  hmap opts (R.Constructor x) = R.Constructor (hmap opts x)
