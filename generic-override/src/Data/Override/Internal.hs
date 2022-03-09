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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Override.Internal where

import GHC.Generics
import GHC.TypeLits (Symbol)

-- | The feature of this library. For use with DerivingVia.
-- Apply it to a type @a@ and supply a type-level list of instance
-- overrides @xs@.
newtype Override a (xs :: [*]) = Override a

-- | Unwrap an 'Override' value.
unOverride :: Override a xs -> a
unOverride (Override a) = a

-- | Construct an 'Override' using a proxy of overrides.
override :: a -> proxy xs -> Override a xs
override a _ = Override a

-- | Used to construct a type-level override. Usually used infix.
-- The @o@ should be either a type (kind '*') or a type-level string
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

  overrideFrom = \case
    L1 f -> L1 $ overrideFrom @xs f
    R1 g -> R1 $ overrideFrom @xs g

  overrideTo = \case
    L1 f -> L1 $ overrideTo @xs f
    R1 g -> R1 $ overrideTo @xs g

instance GOverride xs (M1 S ('MetaSel ms su ss ds) (K1 R c)) where
  type OverrideRep xs (M1 S ('MetaSel ms su ss ds) (K1 R c)) =
    M1 S ('MetaSel ms su ss ds) (K1 R (Overridden ms c xs))
  overrideFrom (M1 (K1 x)) = M1 (K1 (Overridden @ms x))
  overrideTo (M1 (K1 (Overridden x))) = M1 (K1 x)

instance GOverride xs U1 where
  type OverrideRep xs U1 = U1
  overrideFrom U1 = U1
  overrideTo U1 = U1

-- | Type family used to determine which override from @xs@
-- to replace @a@ with, if any. The @ms@ holds the field name
-- for @a@, if applicable.
type family Using (ms :: Maybe Symbol) (a :: *) (xs :: [*]) where
  -- No matching override found.
  Using ms a '[] = a

  -- Override the matching field.
  Using ('Just o) a (As o n ': xs) = n
  Using ('Just o) a (With o w ': xs) = w a

  -- Override the matching type.
  Using ms a (As a n ': xs) = n
  Using ms a (With a w ': xs) = w a

  -- Override the matching kind (up to 10).
  Using ms (f a0) (As f g ': xs) = g a0
  Using ms (f a0 a1) (As f g ': xs) = g a0 a1
  Using ms (f a0 a1 a2) (As f g ': xs) = g a0 a1 a2
  Using ms (f a0 a1 a2 a3) (As f g ': xs) = g a0 a1 a2 a3
  Using ms (f a0 a1 a2 a3 a4) (As f g ': xs) = g a0 a1 a2 a3 a4
  Using ms (f a0 a1 a2 a3 a4 a5) (As f g ': xs) = g a0 a1 a2 a3 a4 a5
  Using ms (f a0 a1 a2 a3 a4 a5 a6) (As f g ': xs) = g a0 a1 a2 a3 a4 a5 a6
  Using ms (f a0 a1 a2 a3 a4 a5 a6 a7) (As f g ': xs) = g a0 a1 a2 a3 a4 a5 a6 a7
  Using ms (f a0 a1 a2 a3 a4 a5 a6 a7 a8) (As f g ': xs) = g a0 a1 a2 a3 a4 a5 a6 a7 a8
  Using ms (f a0 a1 a2 a3 a4 a5 a6 a7 a8 a9) (As f g ': xs) = g a0 a1 a2 a3 a4 a5 a6 a7 a8 a9

  -- No match on this override, recurse.
  Using ms a (x ': xs) = Using ms a xs
