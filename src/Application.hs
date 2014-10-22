{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Control.Lens
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Session

import Snap (get)
import Snap.Snaplet.PostgresqlSimple
import Snap.Snaplet.Router
import Snap.Snaplet.Router.Types
import Text.Blaze
import Data.Text (Text)
import Web.Routes

import Util
------------------------------------------------------------------------------
data AppUrl
    = Bakes
    | Echo Text
    deriving (Eq, Show, Read, Generic)

data App = App
    { _heist :: Snaplet (Heist App)
    , _sess :: Snaplet SessionManager
    , _auth :: Snaplet (AuthManager App)
    , _pg :: Snaplet Postgres
    , _router :: Snaplet RouterState
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasPostgres (Handler b App) where
    getPostgresState = with pg get

instance PathInfo AppUrl
instance ToValue AppUrl where
  toValue = toValue . toPathInfo

instance HasRouter (Handler App App) where
    type URL (Handler App App) = AppUrl
    getRouterState = with router get

instance HasRouter (Handler b RouterState) where
    type URL (Handler b RouterState) = AppUrl
    getRouterState = get

------------------------------------------------------------------------------
type AppHandler = Handler App App


