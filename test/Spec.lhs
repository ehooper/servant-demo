Testing the API
===============

\begin{code}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Lib (serverApp, ServerConf(..))
import Data.ByteString (append)
import qualified Data.ByteString.Lazy as BSL
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.Matcher
import Test.Hspec.Wai.JSON
import Network.HTTP.Types.Header (Header)
import Network.Wai.Test (SResponse(..))

import DalTest

main :: IO ()
main = hspec $ do
  userDalSpec
  apiSpec

login :: WaiSession SResponse
login = request "POST" "/login" [("Content-Type", "application/json")] [json|{username:"user",password:"password"}|]

getHeaders :: WaiSession [Header]
getHeaders = do
  (SResponse _ _ token) <- login
  return [("Content-Type", "application/json"), ("Authorization", "Bearer " `append` BSL.toStrict (stripQuotes token))]
  where stripQuotes = BSL.init . BSL.tail

apiSpec :: Spec
apiSpec = beforeAll (serverApp (ServerConf "default")) $ do
  describe "POST /login" $
    it "gets a JWT token" $
      login `shouldRespondWith` 200

  describe "GET /users [0]" $
    it "gets an empty list of users" $ do
      headers <- getHeaders
      request "GET" "/users" headers "" `shouldRespondWith` "{\"status\":\"success\",\"data\":[]}"

  describe "POST /user" $
    it "adds a new user" $ do
      headers <- getHeaders
      let user = [json|{firstName:"Test",lastName:"Test"}|]
      request "POST" "/user" headers user `shouldRespondWith` "{\"status\":\"success\",\"data\":1}"

  describe "GET /users [1]" $
    it "gets list of one user" $ do
      headers <- getHeaders
      let users = [json|{status:"success",data:[{userId:1,firstName:"Test",lastName:"Test"}]}|]
      request "GET" "/users" headers "" `shouldRespondWith` ResponseMatcher 200 [] (bodyEquals users)
\end{code}
