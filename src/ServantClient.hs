{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ServantClient
  ( runClient,
    getTargetUser,
    updateTargetUser,
    deleteTartgetUser,
    getUsers,
    createUser,
    getBase,
  )
where

import Network.HTTP.Client hiding (Proxy)
import Servant
import Servant.Client
import ServantAPI

runClient :: BaseUrl -> ClientM a -> IO (Either ClientError a)
runClient url cl = do
  manager <- newManager defaultManagerSettings
  runClientM cl (mkClientEnv manager url)

getTargetUser :: UserId -> ClientM User
updateTargetUser :: UserId -> User -> ClientM ()
deleteTartgetUser :: UserId -> ClientM ()
getTargetUser :<|> updateTargetUser :<|> deleteTartgetUser = client (Proxy @TargetUserAPI)

getUsers :: ClientM [User]
createUser :: User -> ClientM ()
getUsers :<|> createUser = client (Proxy @UserAPI)

getBase :: ClientM String
getBase = client (Proxy @BaseAPI)

