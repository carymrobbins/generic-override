-- | This is the internal generic-override API and should be considered
-- unstable and subject to change. This module is exposed for library integrators
-- (e.g. generic-override-aeson). In general, unless you are integrating
-- some type class with generic-override, you should prefer to use the
-- public, stable API provided by 'Data.Override'.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Override.Internal where

import Data.Type.Bool (If)
import Data.Coerce
import Data.Type.Equality (type (==))
import GHC.Generics
import GHC.TypeLits (Symbol)

-- | The feature of this library. For use with DerivingVia.
-- Apply it to a type 'a' and supply a type-level list of instance
-- overrides 'xs'.
newtype Override a (xs :: [*]) = Override a

-- | Unwrap an 'Override' value.
unOverride :: Override a xs -> a
unOverride (Override a) = a

-- | Construct an 'Override' using a proxy of overrides.
override :: a -> proxy xs -> Override a xs
override a _ = Override a

-- | Used to construct a type-level override. Usually used infix.
-- The 'o' should be either a type (kind '*') or a type-level string
-- (kind 'Symbol').
data As (o :: k) n

-- | Used to wrap a field into a something of kind @* -> *@, for example another newtype.
data With (o :: k) (w :: * -> *)

-- | Used at the leaf nodes of a generic 'Rep'
newtype Overridden (ms :: Maybe Symbol) a (xs :: [*]) = Overridden a

-- | Unwrap an 'Overridden' value.
unOverridden :: Overridden ms a xs -> a
unOverridden (Overridden a) = a

-- | Same as 'override' but for 'Overridden' types.
overridden
  :: forall a (ms :: Maybe Symbol) (xs :: [*]) proxy0 proxy1.
     a -> proxy0 ms -> proxy1 xs -> Overridden ms a xs
overridden a _ _ = Overridden a

instance (Generic a, GOverride xs (Rep a)) => Generic (Override a xs) where
  type Rep (Override a xs) = OverrideRep xs (Rep a)
  from = overrideFrom @xs . from . unOverride
  to = Override . to . overrideTo @xs

-- | Type class used to build the 'Generic' instance for 'Override'.
class GOverride (xs :: [*]) (f :: * -> *) where
  -- | Analogous to 'Rep'; rewrites the type for a given 'Rep' and injects
  -- 'Overridden' at the leaves.
  type OverrideRep xs f :: * -> *
  overrideFrom :: f x -> OverrideRep xs f x
  overrideTo :: OverrideRep xs f x -> f x

instance GOverride xs U1 where
  type OverrideRep xs U1 = U1
  overrideFrom U1 = U1
  overrideTo U1 = U1

instance (GOverride xs f) => GOverride xs (M1 D c f) where
  type OverrideRep xs (M1 D c f) = M1 D c (OverrideRep xs f)
  overrideFrom (M1 x) = M1 (overrideFrom @xs x)
  overrideTo (M1 x) = M1 (overrideTo @xs x)

instance (GOverride xs f) => GOverride xs (M1 C c f) where
  type OverrideRep xs (M1 C c f) = M1 C c (OverrideRep xs f)
  overrideFrom (M1 x) = M1 (overrideFrom @xs x)
  overrideTo (M1 x) = M1 (overrideTo @xs x)

instance (GOverride xs f, GOverride xs g) => GOverride xs (f :*: g) where
  type OverrideRep xs (f :*: g) = OverrideRep xs f :*: OverrideRep xs g
  overrideFrom (f :*: g) = overrideFrom @xs f :*: overrideFrom @xs g
  overrideTo (f :*: g) = overrideTo @xs f :*: overrideTo @xs g

instance (GOverride xs f, GOverride xs g) => GOverride xs (f :+: g) where
  type OverrideRep xs (f :+: g) = OverrideRep xs f :+: OverrideRep xs g
  overrideFrom (L1 l) = L1 (overrideFrom @xs l)
  overrideFrom (R1 r) = R1 (overrideFrom @xs r)
  overrideTo (L1 l) = L1 (overrideTo @xs l)
  overrideTo (R1 r) = R1 (overrideTo @xs r)

-- | Auxiliary family to put Overridden under Maybe.
--
-- This is required to preserve aeson's omitNothingFields behavior
-- if the user has enabled it.
type family OverrideField ms c xs where
  OverrideField ms (Maybe c) xs = Maybe (Overridden ms c xs)
  OverrideField ms c xs = Overridden ms c xs

-- | Auxiliary type class to actually put Overridden in the correct place.
--
-- It has to be a MultiParamTypeClass instead of using the above type family, because
-- otherwise GHC can't solve the bare @c@ case.
--
-- Ideally we would like to use @Coercible c (OverrideField ms c xs)@ instead of
-- this class, but then users would have to import Overridden constructor, in order for GHC
-- to solve Coercible constraint.
class DoOverrideField c a where
  overrideFieldTo :: c -> a
  overrideFieldFrom :: a -> c

instance {-# OVERLAPPING #-} DoOverrideField (Maybe c) (Maybe (Overridden ms c xs)) where
  overrideFieldTo = coerce
  overrideFieldFrom = coerce

instance {-# OVERLAPPING #-} DoOverrideField c (Overridden ms c xs) where
  overrideFieldTo = coerce
  overrideFieldFrom = coerce

instance DoOverrideField c (OverrideField ms c xs) => GOverride xs (M1 S ('MetaSel ms su ss ds) (K1 R c)) where
  type OverrideRep xs (M1 S ('MetaSel ms su ss ds) (K1 R c)) =
    M1 S ('MetaSel ms su ss ds) (K1 R (OverrideField ms c xs))
  overrideFrom (M1 (K1 x)) = M1 (K1 (overrideFieldTo x))
  overrideTo (M1 (K1 x)) = M1 (K1 (overrideFieldFrom x))

-- | Type family used to determine which override from @xs@
-- to replace @x@ with, if any. The @ms@ holds the field name
-- for @x@, if applicable.
type family Using (ms :: Maybe Symbol) (x :: *) (xs :: [*]) where
  -- No matching override found.
  Using ms x '[] = x

  -- Override the matching field.
  Using ms x (As (o :: Symbol) n ': xs) =
    If (ms == 'Just o) n (Using ms x xs)

  Using ms x (With (o :: Symbol) w ': xs) =
    If (ms == 'Just o) (w x) (Using ms x xs)

  -- Override the matching type.
  Using ms x (As (o :: *) n ': xs) =
    If (x == o) n (Using ms x xs)

  Using ms x (With (o :: *) w ': xs) =
    If (x == o) (w x) (Using ms x xs)
