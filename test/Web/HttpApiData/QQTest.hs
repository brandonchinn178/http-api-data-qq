{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Web.HttpApiData.QQTest (tests) where

import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import qualified Data.Map as Map
import Data.String (IsString, fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy (Text)
import Network.HTTP.Client (
  Response (..),
  defaultManagerSettings,
  httpLbs,
  newManager,
  parseRequest,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Web.HttpApiData (ToHttpApiData (..))

import Web.HttpApiData.QQ

tests :: TestTree
tests =
  testGroup
    "Web.HttpApiData.QQ"
    [ testCase "Quasiquoter works" $ do
        [url|/foo|] @?= "/foo"
        let asdf = "bar"
        [url|/foo/#{asdf}|] @?= "/foo/bar"
        [url|/foo/#{asdf}/baz|] @?= "/foo/bar/baz"
    , testCase "Quasiquoter generates any IsString" $ do
        let check :: forall a. (Eq a, IsString a, Show a) => Assertion
            check = do
              let asdf = "test"
              [url|/foo/#{asdf}|] @?= (fromString "/foo/test" :: a)
        check @String
        check @Text
        check @Lazy.Text
        check @ByteString
        check @Lazy.ByteString
    , testCase "Uses ToHttpApiData instances" $ do
        let path = PathHello
        [url|#{path}/1|] @?= "/hello/1"
    , testCase "Can be used with http-client" $ do
        manager <- newManager defaultManagerSettings
        let userId = 100 :: Int
        request <- parseRequest [url|http://httpbin.org/anything/user/#{userId}|]
        response <- responseBody <$> httpLbs request manager
        body <-
          maybe (assertFailure $ "Response could not be decoded: " ++ show response) return $
            Aeson.decode response
        "url" `Map.lookup` body @?= Just (Aeson.toJSON "http://httpbin.org/anything/user/100")
    ]

data MyPath = PathHello | PathWorld

instance ToHttpApiData MyPath where
  toUrlPiece =
    Text.pack . \case
      PathHello -> "/hello"
      PathWorld -> "/world"
