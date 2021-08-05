-- | This module contains only orphan instances. It is only needed to
-- be imported where you are overriding instances for aeson generic derivation.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Override.Aeson where

import Data.Coerce (Coercible, coerce)
import Data.Override.Internal (Override, Overridden(Overridden), Using)
import GHC.Generics (Generic, Rep)
import qualified Data.Aeson as Aeson

newtype OverrideAeson a (xs :: [*]) (options :: [*]) = OverrideAeson  { unOverrideAeson :: Override a xs }

data OmitNothingFields (x :: Bool)

-- TODO: Make this stuff @Internal@. {{{

class KnownBool (a :: Bool) where
  boolVal :: Bool

instance KnownBool 'True where
  boolVal = True

instance KnownBool 'False where
  boolVal = False

class ApplyAesonOption x where
  applyAesonOption :: Aeson.Options -> Aeson.Options

instance (KnownBool x) => ApplyAesonOption (OmitNothingFields x) where
  applyAesonOption o = o { Aeson.omitNothingFields = boolVal @x }

class ApplyAesonOptions (xs :: [*]) where
  applyAesonOptions :: Aeson.Options -> Aeson.Options

instance (ApplyAesonOption x, ApplyAesonOptions xs) => ApplyAesonOptions (x ': xs) where
  applyAesonOptions = applyAesonOptions @xs . applyAesonOption @x

instance ApplyAesonOptions '[] where
  applyAesonOptions = id

aesonOptions :: forall xs. (ApplyAesonOptions xs) => Aeson.Options
aesonOptions = applyAesonOptions @xs Aeson.defaultOptions

-- TODO: Make this stuff @Internal@. }}}

instance
  ( Generic (Override a xs)
  , Aeson.GToJSON Aeson.Zero (Rep (Override a xs))
  , Aeson.GToEncoding Aeson.Zero (Rep (Override a xs))
  , ApplyAesonOptions options
  ) => Aeson.ToJSON (OverrideAeson a xs options)
  where
  toJSON = Aeson.genericToJSON (aesonOptions @options) . unOverrideAeson
  toEncoding = Aeson.genericToEncoding (aesonOptions @options) . unOverrideAeson

instance
  ( Generic (Override a xs)
  , Aeson.GToJSON Aeson.Zero (Rep (Override a xs))
  , Aeson.GToEncoding Aeson.Zero (Rep (Override a xs))
  ) => Aeson.ToJSON (Override a xs)

instance
  ( Coercible a (Using ms a xs)
  , Aeson.ToJSON (Using ms a xs)
  ) => Aeson.ToJSON (Overridden ms a xs)
  where
  toJSON = Aeson.toJSON @(Using ms a xs) . coerce
  toEncoding = Aeson.toEncoding @(Using ms a xs) . coerce

instance
  ( Generic (Override a xs)
  , Aeson.GFromJSON Aeson.Zero (Rep (Override a xs))
  , ApplyAesonOptions options
  ) => Aeson.FromJSON (OverrideAeson a xs options)
  where
  parseJSON = fmap OverrideAeson . Aeson.genericParseJSON (aesonOptions @options)

instance
  ( Generic (Override a xs)
  , Aeson.GFromJSON Aeson.Zero (Rep (Override a xs))
  ) => Aeson.FromJSON (Override a xs)

instance
  ( Coercible a (Using ms a xs)
  , Aeson.FromJSON (Using ms a xs)
  ) => Aeson.FromJSON (Overridden ms a xs)
  where
  parseJSON = coerce . Aeson.parseJSON @(Using ms a xs)
