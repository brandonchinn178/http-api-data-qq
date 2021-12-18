# `http-api-data-qq`

[![CircleCI](https://img.shields.io/circleci/build/github/brandonchinn178/http-api-data-qq)](https://app.circleci.com/pipelines/github/brandonchinn178/http-api-data-qq)
[![Hackage](https://img.shields.io/hackage/v/http-api-data-qq)](https://hackage.haskell.org/package/http-api-data-qq)
[![Codecov](https://img.shields.io/codecov/c/gh/brandonchinn178/http-api-data-qq)](https://codecov.io/gh/brandonchinn178/http-api-data-qq)

Quasiquoter for building URLs with strings interpolated using `ToHttpApiData` instances.

```hs
{-# LANGUAGE QuasiQuotes #-}

import Network.HTTP.Client
import Web.HttpApiData.QQ (url)

userId :: Int
userId = 100

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings
  request <- parseRequest [url|http://httpbin.org/anything/user/#{userId}|]
  response <- httpLbs request manager
  print response
```
