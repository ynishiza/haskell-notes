{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module ServantAPI
  ( User,
    UserId,
    BaseAPI,
    UserAPI,
    API,
    TargetUserAPI,
    api,
    mainHandler,
  )
where

import Control.Monad.IO.Class
import Data.Aeson
import GHC.Generics
import Servant
import Control.Monad.Catch (throwM)

-- Data
data User where
  User :: {userId :: UserId, name :: String, email :: String} -> User
  deriving (Generic, FromJSON, ToJSON, Show, Eq)

type UserId = String

api :: Proxy API
api = Proxy

-- Interfaces
type API =
  "api"
    :> Summary "Main API"
    :> Description
         "This is a sample API\
         \5 endpoints \
         \ GET      /api/user \
         \ POST     /api/user \
         \ GET      /api/user/:uid \
         \ POST     /api/user/:uid \
         \ DELETE   /api/user/:uid "
    :> ( BaseAPI
           :<|> UserAPI
           :<|> TargetUserAPI
       )

type BaseAPI =
  Summary "Base API"
    :> Get '[JSON] String

type UserAPI =
  "user"
    :> Summary "User API"
    :> ( Get '[JSON] [User]
           :<|> ReqBody '[JSON] User :> Post '[JSON] ()
       )

type CaptureUID = Capture "uid" UserId

type TargetUserAPI =
  ( CaptureUID :> Get '[JSON] User
      :<|> CaptureUID :> ReqBody '[JSON] User :> Post '[JSON] ()
      :<|> CaptureUID :> Delete '[JSON] ()
  )

-- Handlers
mainHandler :: Server API
mainHandler =
  baseAPIHandler
    :<|> userAPIHandler
    :<|> targetUserAPIHandler

baseAPIHandler :: Server BaseAPI
baseAPIHandler = return "Hello"

userAPIHandler :: Server UserAPI
userAPIHandler =
  ( do
      liftIO $ putStrLn "GET user"
      return []
  )
    :<|> createUser

createUser :: User -> Handler ()
createUser user = do
  liftIO $ putStrLn $ "POST user:" <> show user
  return ()

targetUserAPIHandler :: Server TargetUserAPI
targetUserAPIHandler =
  ( \uid -> do
      liftIO $ putStrLn $ "GET uid:" <> uid
      return $ User "123" "Yui" "test@test.com"
  )
    :<|> updateTargetUser
    :<|> deleteTargetUser

updateTargetUser :: String -> User -> Handler ()
updateTargetUser uid user = do
  liftIO $ putStrLn $ "POST uid:" <> uid <> " user:" <> show user
  return ()

deleteTargetUser :: String -> Handler ()
deleteTargetUser uid = do
  liftIO $ putStrLn $ "DELETE uid:" <> uid
  throwError err403
