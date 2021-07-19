{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module OmitNothingFieldsAeson where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic, Rep)
import qualified Data.Aeson as Aeson
import qualified Data.Char as Char

newtype OmitNothingFields a = OmitNothingFields a

omitNothingFieldsAesonOptions :: Aeson.Options
omitNothingFieldsAesonOptions =
  Aeson.defaultOptions
    { Aeson.omitNothingFields = True
    }

instance
  ( Generic a
  , Aeson.GToJSON Aeson.Zero (Rep a)
  , Aeson.GToEncoding Aeson.Zero (Rep a)
  ) => ToJSON (OmitNothingFields a)
  where
  toJSON (OmitNothingFields a) = Aeson.genericToJSON omitNothingFieldsAesonOptions a
  toEncoding (OmitNothingFields a) = Aeson.genericToEncoding omitNothingFieldsAesonOptions a

instance
  ( Generic a
  , Aeson.GFromJSON Aeson.Zero (Rep a)
  ) => FromJSON (OmitNothingFields a)
  where
  parseJSON v = OmitNothingFields <$> Aeson.genericParseJSON omitNothingFieldsAesonOptions v
