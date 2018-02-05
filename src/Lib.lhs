Introduction
============

This is a small example API server application written in Haskell. I will demonstrate a type-level API specification using [servant](http://haskell-servant.readthedocs.io/en/stable/index.html) with JWT authentication and [Swagger](http://swagger.io/) documentation. An independent HTTPS server can be built in the [docker](docker/) directory of this project.

All of the Haskell code in this project is written in literate Hakskell with markdown syntax and compiled into an HTML page with [Pandoc](http://pandoc.org/).

This project was expanded from [this example project](https://github.com/plow-technologies/servant-auth).

This first section is the main codebase for the API server application. I will go over every line of code in this module.

Preliminaries
-------------

First we need to enable some language extensions. I have rarely needed to think about them ahead of time or in detail; GHC (the compiler) will usually suggest whatever extensions you need to enable when you attempt to compile your code. In further examples these language pragmas will be hidden.

\begin{code}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
\end{code}

Next is the module definition. This module only exports three things: the entry point for starting the server independently (`startApp`), the code the run just the API (`serverApp`), and the configuration data the server needs (`ServerConf`). The latter two exports are only used for testing.

\begin{code}
module Lib
    ( startApp
    , serverApp
    , ServerConf(..)
    ) where
\end{code}

We start the module with a list of imports. I won't go into detail about how Haskell's module system works, but as you can see, import lists can get quite large. Tools like flycheck are very helpful for managing imports.

\begin{code}
import GHC.Generics (Generic)
import Control.Lens hiding ((.=))
import Control.Monad.Trans (liftIO)
import Data.Aeson
import Data.Text.Internal (Text)
import Data.Swagger hiding (Header)
import qualified Data.Yaml as Yaml
import Network.Wai.Handler.Warp
import Servant
import Servant.Swagger
import Servant.Auth.Server
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import System.Log.Logger
import System.Log.Handler.Simple
import System.IO (stdout)
import Options.Applicative hiding (info)
import qualified Options.Applicative as Options
import Data.Semigroup ((<>))
\end{code}

We have a couple of local modules to import too. These will be expanded on in [Data Definitions][] and [Data Access Layer][].

\begin{code}
import Api.Data
import Api.Dal
\end{code}

Definitions
-----------

In this section we will describe the objects in our application as types and defer implementations to the next section.

This application will be using [JSON web tokens](https://jwt.io/) for authentication, so we will define a data type for the contents of each token. The token will only contain the user name.

\begin{code}
type UserName = Text
type Password = Text
data Login = Login { username :: UserName, password :: Password }
   deriving (Eq, Show, Read, Generic)
instance ToJSON Login
instance FromJSON Login
instance ToSchema Login
\end{code}

\begin{code}

data AuthUser = AuthUser { user :: UserName }
   deriving (Eq, Show, Read, Generic)

instance ToJSON AuthUser
instance FromJSON AuthUser
instance ToJWT AuthUser
instance FromJWT AuthUser
\end{code}

A constant for our logs.

\begin{code}
appLog :: String
appLog = "ServantTest"
\end{code}

And a simple configuration for this server:

\begin{code}
data ServerConf = ServerConf { env :: String } deriving (Show, Generic)
instance FromJSON ServerConf
\end{code}

Our API begins with a type-level Servant specification. These type definitions are schemas for our API and its subcomponents.

We will create a new type for JwtToken, which is just a type-level "wrapper" around a string. This is a good and cheap way to document the meaning of a piece of data that is implemented with a simple type.
\begin{code}
newtype JwtToken = JwtToken Text deriving (Eq, Show, Generic)
instance ToJSON JwtToken
instance ToSchema JwtToken

type LoginAPI =
 "login"
     :> ReqBody '[JSON] Login
     :> Post '[JSON] JwtToken

type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

type UserAPI =
  "users" :> Get '[JSON] (ApiResponse [User]) :<|>
  "user" :> ReqBody '[JSON] NewUser :> Post '[JSON] (ApiResponse UserId)

type ProtectedAPI = SwaggerAPI :<|> UserAPI
\end{code}

The main API combines all of the API sections and has an argument for the authentication system.

\begin{code}
type API auths = (Auth auths AuthUser :> ProtectedAPI) :<|> LoginAPI
\end{code}

While we're at it, we'll define the Swagger specification for the user API.

\begin{code}
authDef :: SecurityScheme
authDef = SecurityScheme (SecuritySchemeApiKey (ApiKeyParams "Authorization" ApiKeyHeader)) Nothing

swaggerSpec :: Swagger
swaggerSpec = toSwagger apiDef
  & info.title .~ "Servant example"
  & info.version .~ "0.1"
  & info.description ?~ "This is an API that tests swagger integration"
  & info.license ?~ ("BSD3" & url ?~ URL "http://opensource.org/licenses/BSD-3-Clause")
  & host ?~ "localhost:8080"
  & schemes ?~ [Https, Http]
  & securityDefinitions .~ [("Bearer", authDef)]
  & (userOps . security .~ [SecurityRequirement [("Bearer", [])]])
  where apiDef = Proxy :: Proxy (LoginAPI :<|> UserAPI)
        userOps = subOperations (Proxy :: Proxy UserAPI) apiDef
\end{code}

Implementation
--------------

Now we create type-checked implementations of the Servant definitions from above.
Notice that these objects have requirements of their own.

\begin{code}
login :: JWTSettings -> Server LoginAPI
login jwtSettings (Login usr@"user" "password") = do
  token <- liftIO $ makeJWT (AuthUser usr) jwtSettings Nothing
  liftIO $ debugM appLog "Created token"
  case token of
    (Left e) -> do
      liftIO $ criticalM appLog $ "Error creating token: " ++ show e
      throwError $ err500 { errBody = "error creating authentication token" }
    (Right tok) -> return $ JwtToken (decodeUtf8 . BSL.toStrict $ tok)
login _ _ = throwError err401

userApi :: (UserDal dal) => (DalProvider dal) -> Server UserAPI
userApi dal =
  liftIO (SuccessResponse <$> (withProvider dal getAllUsers)) :<|>
  (\user -> liftIO $ SuccessResponse <$> withProvider dal (addUser user))

protected :: (UserDal dal) => (DalProvider dal) -> AuthResult AuthUser -> Server ProtectedAPI
protected dal (Authenticated _) = return swaggerSpec :<|> userApi dal
protected _ _ = throwAll err401
\end{code}

Finally, the implementation of the API server:

\begin{code}
server :: (UserDal dal) => JWTSettings -> (DalProvider dal) -> Server (API auths)
server jwts dal = (protected dal) :<|> login jwts
\end{code}

With the excellent optparse-applicative library, we can specify command-line arguments for our application in a concise and type-safe way.

We will have two execution "modes": one to start the server with optional arguments for a port and configuration file, and another to dump the Swagger specificationof the API to standard output.

\begin{code}
data Mode = Serve Port String | DumpSwagger
\end{code}

We can define a parser for the server mode and its arguments by itself, and then combine it with parsers for other modes.

\begin{code}
serveOpts :: Parser Mode
serveOpts = Serve
            <$> option auto
            ( long "port"
              <> short 'p'
              <> help "Port to run the server on"
              <> showDefault
              <> value 8080
              <> metavar "PORT" )
            <*> strOption
            ( long "config"
              <> short 'c'
              <> help "Server configuration file"
              <> showDefault
              <> value "config.yml"
              <> metavar "FILE" )
\end{code}

The swagger dump mode is set by a flag - note that alternative operator (`<|>`) will actually enforce the mutual-exclusion constraint on these modes, so server-mode arguments cannot be mixed with the dump-swagger flag.

\begin{code}
progOpts :: Parser Mode
progOpts = serveOpts
           <|> flag' DumpSwagger (long "dump-swagger" <> help "Dump swagger.json")
\end{code}

The code to start the server is a bit complicated, so we will abstract it into its own function which takes the port number and configuration file as arguments.

\begin{code}
serverApp :: ServerConf -> IO Application
serverApp config = do
\end{code}

We configure JWT at runtime.

\begin{code}
  key <- generateKey
  let jwtCfg = defaultJWTSettings key
      cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
      api = Proxy :: Proxy (API '[JWT])
\end{code}

Load the config.

\begin{code}
  let withDal provider = server jwtCfg provider
  app <- case config of
    -- In "default" mode, use an empty dummy DAL.
    (ServerConf "default") -> withDal <$> initMockUserDal 1 []
    -- In "test" mode, use the dummy DAL with some example data.
    (ServerConf "test") -> withDal <$> initMockUserDal 3 [User (UserId 1) "Isaac" "Newton" , User (UserId 2) "Albert" "Einstein"]
    -- Otherwise use the "real" DAL.
    _ -> withDal <$> initSqlUserDal
\end{code}

Start the server.

\begin{code}
  return $ serveWithContext api cfg app
\end{code}

In the application entry point, we parse the command-line arguments and run the appropriate execution mode.

\begin{code}
startApp :: IO ()
startApp = do
\end{code}

Set up the logger.

\begin{code}
  stdoutHandler <- verboseStreamHandler stdout DEBUG
  updateGlobalLogger rootLoggerName removeHandler -- disable the default log handler since we have our own
  updateGlobalLogger appLog (setLevel DEBUG . setHandlers [stdoutHandler])
\end{code}

Parse the config file.

\begin{code}
  mode <- execParser (Options.info (progOpts <**> helper) ( fullDesc <> progDesc "Servant demo server" ))
  case mode of
    Serve port configFile -> do
      parsedFile <- Yaml.decodeFileEither configFile
      config <-
        case parsedFile of
          Left e -> do criticalM appLog $ "Failed to load config: " ++ show e
                       return $ ServerConf "default"
          Right c -> return c
\end{code}

\begin{code}
      debugM appLog ("Starting server on port " ++ show port)
      app <- serverApp config
      run port app
    DumpSwagger -> BSL8.putStrLn $ encode swaggerSpec
\end{code}
