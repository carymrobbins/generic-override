{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON), Result(Success), fromJSON)
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Override (Override(Override), As, With)
import Data.Override.Aeson ()
import Data.Text (Text)
import GHC.Generics (Generic)
import LispCaseAeson (LispCase(LispCase))
import OmitNothingFieldsAeson (OmitNothingFields(OmitNothingFields))
import qualified Data.Text as Text
import Test.Hspec
import Text.Read (readMaybe)

main :: IO ()
main = hspec do
  describe "Override ToJSON machinery" do
    it "Rec1" testRec1
    it "Rec2" testRec2
    it "Rec3" testRec3
    it "Rec4" testRec4
    it "Rec5" testRec5
    it "Rec6" testRec6
    it "Rec7" testRec6
    it "Rec8" testRec8
    it "Rec8 with Nothing field" testRec8_Nothing
    it "Rec9 with L constructor" testRec9_L
    it "Rec9 with R constructor" testRec9_R

newtype Uptext = Uptext { unUptext :: Text }

instance ToJSON Uptext where
  toJSON = toJSON . Text.toUpper . unUptext

newtype Shown a = Shown { unShown :: a }

instance (Show a) => ToJSON (Shown a) where
  toJSON = toJSON . show . unShown

instance (Read a) => FromJSON (Shown a) where
  parseJSON v = do
    s <- parseJSON v
    case readMaybe s of
      Nothing -> fail "read: no parse"
      Just a -> pure $ Shown a

newtype CharArray s = CharArray { unCharArray :: s }
  deriving stock (Functor)

instance ToJSON (CharArray String) where
  toJSON = toJSON . map (:[]) . unCharArray

instance FromJSON (CharArray String) where
  parseJSON v = do
    cs :: [String] <- parseJSON v
    pure $ CharArray $ concat cs

instance ToJSON (CharArray Text) where
  toJSON = toJSON . fmap Text.unpack

-- | Overriding instances by type.
data Rec1 = Rec1
  { foo :: Int
  , bar :: String
  , baz :: Text
  } deriving stock (Show, Eq, Generic)
    deriving (ToJSON)
      via Override Rec1
            '[ Text `As` Uptext
             , Int `As` Shown Int
             ]

testRec1 :: IO ()
testRec1 = do
  let r = Rec1 { foo = 12, bar = "hi", baz = "bye" }
  toJSON r `shouldBe` [aesonQQ|
    {
      "foo": "12",
      "bar": "hi",
      "baz": "BYE"
    }
  |]

-- | Overriding instances by field name.
data Rec2 = Rec2
  { foo :: Int
  , bar :: Text
  , baz :: Text
  } deriving stock (Show, Eq, Generic)
    deriving (ToJSON)
      via Override Rec2
            '[ "baz" `As` CharArray Text
             , "bar" `As` Uptext
             ]

testRec2 :: IO ()
testRec2 = do
  let r = Rec2 { foo = 12, bar = "hi", baz = "bye" }
  toJSON r `shouldBe` [aesonQQ|
    {
      "foo": 12,
      "bar": "HI",
      "baz": ["b", "y", "e"]
    }
  |]

-- | Overriding instances by type and field name.
data Rec3 = Rec3
  { foo :: Int
  , bar :: String
  , baz :: Text
  } deriving stock (Show, Eq, Generic)
    deriving (ToJSON)
      via Override Rec3
            '[ String `As` CharArray String
             , "foo" `As` Shown Int
             , Text `As` Uptext
             ]

testRec3 :: IO ()
testRec3 = do
  let r = Rec3 { foo = 12, bar = "hi", baz = "bye" }
  toJSON r `shouldBe` [aesonQQ|
    {
      "foo": "12",
      "bar": ["h", "i"],
      "baz": "BYE"
    }
  |]

-- | Overriding instance by type and field name; first match wins.
-- In this case, 'foo' and 'bar' use 'CharArray Text' and 'baz' uses 'Uptext'.
data Rec4 = Rec4
  { foo :: Text
  , bar :: Text
  , baz :: Text
  } deriving stock (Show, Eq, Generic)
    deriving (ToJSON)
      via Override Rec4
            '[ "baz" `As` Uptext
             , Text `As` CharArray Text
             ]

testRec4 :: IO ()
testRec4 = do
  let r = Rec4 { foo = "go", bar = "hi", baz = "bye" }
  toJSON r `shouldBe` [aesonQQ|
    {
      "foo": ["g", "o"],
      "bar": ["h", "i"],
      "baz": "BYE"
    }
  |]

-- We can compose an 'Override' with another deriving-via newtype, 'LispCase'.
data Rec5 = Rec5
  { fooBar :: Int
  , baz :: Text
  , quuxSpamEggs :: String
  } deriving stock (Show, Eq, Generic)
    deriving (ToJSON)
      via LispCase (Override Rec5
            '[ "fooBar" `As` Shown Int
             , Text `As` Uptext
             , "quuxSpamEggs" `As` CharArray String
             ])

testRec5 :: IO ()
testRec5 = do
  let r = Rec5 { fooBar = 1, baz = "hi", quuxSpamEggs = "bye" }
  toJSON r `shouldBe` [aesonQQ|
    {
      "foo-bar": "1",
      "baz": "HI",
      "quux-spam-eggs": ["b", "y", "e"]
    }
  |]

-- Test 'Override' for both 'ToJSON' and 'FromJSON'.
data Rec6 = Rec6
  { foo :: Int
  , bar :: String
  , baz :: Text
  } deriving stock (Show, Eq, Generic)
    deriving (ToJSON, FromJSON)
      via Override Rec6
            '[ "foo" `As` Shown Int
             , String `As` CharArray String
             ]

testRec6 :: IO ()
testRec6 = do
  let r = Rec6 { foo = 1, bar = "hi", baz = "bye" }
  let j = [aesonQQ|
    {
      "foo": "1",
      "bar": ["h", "i"],
      "baz": "bye"
    }
  |]
  toJSON r `shouldBe` j
  fromJSON j `shouldBe` Success r

-- Test 'Override' for both 'ToJSON' and 'FromJSON'.
data Rec7 = Rec7
  { foo :: Int
  , bar :: String
  , baz :: Text
  } deriving stock (Show, Eq, Generic)
    deriving (ToJSON, FromJSON)
      via Override Rec7
            '[ "foo" `With` Shown
             , String `With` CharArray
             ]

testRec7 :: IO ()
testRec7 = do
  let r = Rec7 { foo = 1, bar = "hi", baz = "bye" }
  let j = [aesonQQ|
    {
      "foo": "1",
      "bar": ["h", "i"],
      "baz": "bye"
    }
  |]
  toJSON r `shouldBe` j
  fromJSON j `shouldBe` Success r

newtype Dummy a = Dummy a
  deriving stock (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

data Rec8 = Rec8
  { foo :: Int
  , bar :: Maybe Text
  } deriving stock (Show, Eq, Generic)
    deriving (ToJSON, FromJSON)
      via OmitNothingFields (Override Rec8
            '[ "foo" `With` Dummy
             ])

testRec8 :: IO ()
testRec8 = do
  let r = Rec8 { foo = 1, bar = Just "hi" }
  let j = [aesonQQ|
    {
      "foo": 1,
      "bar": "hi"
    }
  |]
  toJSON r `shouldBe` j
  fromJSON j `shouldBe` Success r

testRec8_Nothing :: IO ()
testRec8_Nothing = do
  let r = Rec8 { foo = 1, bar = Nothing }
  let j = [aesonQQ|
    {
      "foo": 1
    }
  |]
  toJSON r `shouldBe` j
  fromJSON j `shouldBe` Success r

data Rec9
  = Rec9_L { foo :: Int }
  | Rec9_R { bar :: Text }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON)
    via Override Rec9
          '[ "bar" `As` Uptext
           ]

testRec9_L :: IO ()
testRec9_L = do
  let r = Rec9_L { foo = 1 }
  let j = [aesonQQ|
    {
      "tag": "Rec9_L",
      "foo": 1
    }
  |]
  toJSON r `shouldBe` j

testRec9_R :: IO ()
testRec9_R = do
  let r = Rec9_R { bar = "hi" }
  let j = [aesonQQ|
    {
      "tag": "Rec9_R",
      "bar": "HI"
    }
  |]
  toJSON r `shouldBe` j
