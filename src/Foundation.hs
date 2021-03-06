{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Foundation where

import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Import.NoFoundation
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)

-- Used only when in "auth-dummy-login" setting is enabled.
import Yesod.Auth.Dummy

import qualified Data.CaseInsensitive as CI
import qualified Data.Guest as Guest
import qualified Data.Text.Encoding as TE
import Text.Read (readMaybe)
import Yesod.Auth.GuestList
import Yesod.Auth.Message
import Yesod.Auth.OpenId (IdentifierType(Claimed), authOpenId)
import Yesod.Core.Types (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import Yesod.Default.Util (addStaticContentExternal)

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
  { appSettings :: AppSettings
  , appStatic :: Static -- ^ Settings for static file serving.
  , appConnPool :: ConnectionPool -- ^ Database connection pool.
  , appHttpManager :: Manager
  , appLogger :: Logger
  }

data MenuItem = MenuItem
  { menuItemLabel :: Text
  , menuItemRoute :: Route App
  , menuItemAccessCallback :: Bool
  }

data MenuTypes
  = NavbarLeft MenuItem
  | NavbarRight MenuItem

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a
   = forall (m :: * -> *). (MonadIO m, Functor m) =>
                             ReaderT SqlBackend m a

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
                                                                      where
  approot =
    ApprootRequest $ \app req ->
      case appRoot $ appSettings app of
        Nothing -> getApprootText guessApproot app req
        Just root -> root
    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
  makeSessionBackend _ =
    Just <$>
    defaultClientSessionBackend
      120 -- timeout in minutes
      "config/client_session_key.aes"
    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
  yesodMiddleware = defaultYesodMiddleware
  defaultLayout widget = do
    master <- getYesod
    mmsg <- getMessage
    muser <- maybeAuthPair
    let mguestname = Guest.prettyName . snd <$> muser
    pc <- widgetToPageContent $ do 
      setTitle "Toby loves Jenny"
      $(widgetFile "default-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")
    -- The page to be redirected to when authentication is required.
  authRoute _ = Just $ AuthR LoginR
    -- Routes not requiring authentication.
  isAuthorized (AuthR _) _ = return Authorized
  isAuthorized FaviconR _ = return Authorized
  isAuthorized RobotsR _ = return Authorized
  isAuthorized (StaticR _) _ = return Authorized
  isAuthorized TravelR _ = isAuthenticatedWithRedirect
  isAuthorized AccommodationR _ = isAuthenticatedWithRedirect
  isAuthorized InfoR _ = isAuthenticatedWithRedirect
  isAuthorized RsvpR _ = isAuthenticatedWithRedirect
    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
  addStaticContent ext mime content = do
    master <- getYesod
    let staticDir = appStaticDir $ appSettings master
    addStaticContentExternal
      minifym
      genFileName
      staticDir
      (StaticR . flip StaticRoute [])
      ext
      mime
      content
        -- Generate a unique filename based on the content itself
    where
      genFileName lbs = "autogen-" ++ base64md5 lbs
    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
  shouldLog app _source level =
    appShouldLogAll (appSettings app) ||
    level == LevelWarn || level == LevelError
  makeLogger = return . appLogger

-- Define breadcrumbs.
instance YesodBreadcrumbs App where
  breadcrumb _ = return ("home", Nothing)

-- How to run database actions.
instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = do
    master <- getYesod
    runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
  getDBRunner = defaultGetDBRunner appConnPool

instance YesodGuestList App where
  isGuestOnList firstname lastname = do
    let f = Guest.makeConsistent firstname
    let l = Guest.makeConsistent lastname
    x <- lift $ runDB $ getBy $ UniqueGuestName f l
    case x of
      Just (Entity _ g) -> pure $ Right (guestIdent g)
      _ -> lift $ Left <$> (runDB $ closeMatch f l)

closeMatch :: Text -> Text -> DB [(Text, Text)]
closeMatch firstname lastname = do
  gs <-
    selectList
      ([GuestFirstName ==. firstname] ||. [GuestLastName ==. lastname])
      [LimitTo 20]
  pure $ (\(Entity _ (Guest _ f l _)) -> (f, l)) <$> gs

instance YesodAuth App where
  type AuthId App = GuestId
    -- Where to send a user after successful login
  loginDest _ = InfoR
    -- Where to send a user after logout
  logoutDest _ = InfoR
    -- Override the above two destinations when a Referer: header is present
  redirectToReferer _ = True
  authenticate Creds {..} =
    (case credsPlugin of
       "guestlist" ->
         runDB $ do
           x <- getBy $ UniqueGuest $ credsIdent
           case x of
             Just (Entity uid _) -> return $ (Authenticated) uid
             Nothing -> return $ UserError InvalidLogin)
  -- You can add other plugins like Google Email, email or OAuth here
  authPlugins app = extraAuthPlugins
        -- Enable authDummy login if enabled.
    where
      extraAuthPlugins = [authGuestList]
  authHttpManager = getHttpManager

-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
  muid <- maybeAuthId
  return $
    case muid of
      Nothing -> Unauthorized "You must login to access this page"
      Just _ -> Authorized

isAuthenticatedWithRedirect :: Handler AuthResult
isAuthenticatedWithRedirect = do
  muid <- requireAuthId
  return Authorized

instance YesodAuthPersist App where
  type AuthEntity App = Guest
  getAuthEntity uid = do
    x <- runDB (get uid)
    return x

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
  getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger
-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
