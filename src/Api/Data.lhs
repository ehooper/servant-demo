Data Definitions
================

\begin{code}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE StrictData   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Api.Data
  ( User(..)
  , NewUser(..)
  , UserId(..)
  , ApiResponse(..)
  ) where

import GHC.Generics (Generic)
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.TH
import Data.Proxy
import Data.Text (Text, append)
import Data.Swagger hiding (Header)
import Servant.Auth.Server (Default(..))
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField

newtype UserId = UserId Int deriving (Eq, Show, FromField, ToField, Generic)
instance ToSchema UserId
instance FromJSON UserId
instance ToJSON UserId

data User = User
  { userId    :: UserId
  , firstName :: Text
  , lastName  :: Text
  } deriving (Eq, Show, Generic)

instance FromRow User where
  fromRow = User <$> field <*> field <*> field

instance ToSchema User where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "User schema"

instance Default User where
  def = User (UserId 0) "Test" "User"

$(deriveJSON defaultOptions ''User)

data NewUser = NewUser
  { firstName :: Text
  , lastName  :: Text
  } deriving (Eq, Show, Generic)

instance ToSchema NewUser where
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions

$(deriveJSON defaultOptions ''NewUser)

data ApiResponse a = SuccessResponse a | ErrorResponse Text deriving (Eq, Show, Generic)

instance (ToJSON a) => ToJSON (ApiResponse a) where
  toJSON (SuccessResponse x) = object ["status" .= String "success", "data" .= toJSON x]
  toJSON (ErrorResponse msg) = object ["status" .= String "error", "message" .= msg]
\end{code}

The Swagger models are automatically generated for the main data types, but since the API responds with `ApiResponse` data, we must define rules for documenting an `ApiResponse t` where `t` has its own Swagger model. I tried to define a ToSchema instance over the generic type `ApiResponse t`, but that proved to be far more complicated than I was willing to deal with. Instead, we can make implementations for each instance of `ApiResponse t` that we actually use with minor boilerplate.

\begin{code}
instance ToSchema (ApiResponse [User]) where
  declareNamedSchema _ = declareSuccessResponse "Users" (Proxy :: Proxy [User])

instance ToSchema (ApiResponse UserId) where
  declareNamedSchema _ = declareSuccessResponse "UserId" (Proxy :: Proxy UserId)
\end{code}

Swagger 2.0 cannot represent sum types, so for documentation purposes we will simply display the successful response defined in the `ToJSON` instance of `ApiResponse`.

\begin{code}
declareSuccessResponse dataName proxy = do
  dataRef <- declareSchemaRef proxy
  let statusParam = sketchStrictSchema ("success" :: Text)
  return $ NamedSchema (Just $ append dataName "Response")
    $ mempty -- "m(onoid) empty", a simple construct despite a possibly intimidating name: it's just a blank starting point for the definition.
    & type_ .~ SwaggerObject
    & required .~ ["status", "data"]
    & properties .~ [ ("status", Inline statusParam) , ("data", dataRef) ]
\end{code}
