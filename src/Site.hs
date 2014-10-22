{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import           Snap.Core hiding (method)
import qualified Snap.Core as S
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Heist
import qualified Heist.Interpreted as I

import           Control.Monad
import           Database.PostgreSQL.Simple.ToField
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Blaze
import           Text.Blaze.Html5 hiding (form,label)
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes hiding (form,label)
import qualified Text.Blaze.Html5.Attributes as A
import           Snap.Snaplet.Router
import           Text.Digestive
import           Text.Digestive.Snap
import           Text.Digestive.Blaze.Html5

import qualified Heist.Compiled as C
import           Data.Monoid
import           Control.Monad.IO.Class
import           Web.Routes
------------------------------------------------------------------------------
import           Application

------------------------------------------------------------------------------
-- | Render home page
homeHandler :: Handler App App ()
homeHandler = ifTop $ heistLocal (I.bindSplices linksSplices) $ render "home"
  where
    linksSplices = "bakesUrl" ## urlSplice Bakes


------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = maybe noSplices splice authError
    splice err = "loginError" ## I.textSplice err


------------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"


------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = S.method GET handleForm <|> S.method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = registerUser "login" "password" >> redirect "/"


------------------------------------------------------------------------------
-- | Bake data
data Bake = Bake {bakeName :: T.Text}
          deriving (Show)

instance FromRow Bake where
  fromRow = Bake <$> field

instance ToRow Bake where
  toRow (Bake n) = [toField n]

------------------------------------------------------------------------------
-- | Bake forms
bakeForm :: Monad m => Form T.Text m Bake
bakeForm = Bake <$> "bakeName" .: nonEmptyText
  where
    nonEmptyText = check "Cannot be empty" (not . T.null) $ text Nothing

bakeView :: View H.Html -> H.Html
bakeView view = do
  errorList "bakeName" view
  label     "bakeName" view "Name: "
  inputText "bakeName" view

------------------------------------------------------------------------------
-- | Handle new bake form submit
handleNewBake :: Handler App App ()
handleNewBake = do
  (view, result) <- runForm "bakeForm" bakeForm
  case result of
    Nothing -> do
      let view' = fmap toHtml view
      blaze $ docTypeHtml $ do
        form view' "/new_bake" $ do
          bakeView view'
          br
          inputSubmit "Add Bake"
    Just bake -> do
      execute "insert into bakes values (?)" bake
      redirectURL Bakes

------------------------------------------------------------------------------
bakesHandler = do
  results <- query_ "select * from bakes"
  blaze $ docTypeHtml $ do
    body $ do
      ul $ do
        forM_ results $ \b -> do
          li $
            a ! href (toValue $ Echo $ bakeName b) $ toHtml $ bakeName b
      p $ a ! href "/new_bake" $ "Add Bake"

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/",         homeHandler)
         , ("/login",    with auth handleLoginSubmit)
         , ("/logout",   with auth handleLogout)
         , ("/new_user", with auth handleNewUser)
         , ("/new_bake", handleNewBake)
         , ("",          routeWith routeAppUrl)
         , ("",          serveDirectory "static")
         ]

routeAppUrl :: AppUrl -> Handler App App ()
routeAppUrl appUrl =
    case appUrl of
      (Bakes)     -> bakesHandler

cURLSplice :: Monad n => AppUrl -> C.Splice n
cURLSplice r = return $ C.yieldPureText $ toPathInfo r

runtime :: MonadIO n => RuntimeSplice n T.Text
runtime = liftIO $ do
    putStrLn "Write something:"
    T.pack <$> getLine

splice :: MonadIO n => C.Splice n
splice = return $ C.yieldRuntimeText $ runtime

allURLSplices :: MonadIO n => Splices (C.Splice n)
allURLSplices = do
  "foo" ## cURLSplice Bakes

stringSplice :: Monad n => C.Splice n
stringSplice = C.manyWithSplices C.runChildren splicefuncs (return ["aa","bb","cc"])
  where
    splicefuncs = "string" ## (C.pureSplice . C.textSplice $ Prelude.id)

stringSplices :: Monad n => Splices (C.Splice n)
stringSplices = do
  "strings" ## stringSplice

allCompiledSplices :: MonadIO n => Splices (C.Splice n)
allCompiledSplices = mconcat [ allURLSplices
                             , stringSplices
                             ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    addConfig h $ mempty { hcCompiledSplices = allCompiledSplices }
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    -- NOTE: We're using initJsonFileAuthManager here because it's easy and
    -- doesn't require any kind of database server to run.  In practice,
    -- you'll probably want to change this to a more robust auth backend.
    a <- nestSnaplet "auth" auth $
           initJsonFileAuthManager defAuthSettings sess "users.json"
    p <- nestSnaplet "pg" pg pgsInit
    r <- nestSnaplet "router" router $ initRouter ""
    addRoutes routes
    addAuthSplices h auth
    return $ App h s a p r

