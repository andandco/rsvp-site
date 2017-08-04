{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE RankNTypes #-}

module Foundation where

import           Database.Persist.Sql (runSqlPool)
import           Import.NoFoundation
import           Text.Hamlet (hamletFile)
import           Text.Jasmine (minifym)

-- Used only when in "auth-dummy-login" setting is enabled.
import           Yesod.Auth.Dummy

import           Yesod.Auth.GoogleEmail2 (forwardUrl, authGoogleEmail)
import           Yesod.Default.Util (addStaticContentExternal)
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE

import           AppType
import           Model.User
import           Role
import           Routes

data NavItem =  SingleNav (Route App) | Dropdown [Route App]

data MenuItem = MenuItem
    { menuItemLabel :: Text
    , menuItemRoute :: Route App
    , menuItemAccessCallback :: Bool
    }

data MenuTypes
    = NavbarLeft MenuItem
    | NavbarRight MenuItem

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)


-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        muser <- maybeAuthPair
        mcurrentRoute <- getCurrentRoute

        -- Define the menu items of the header.
        let menuItems =
                [ NavbarLeft $ MenuItem
                    { menuItemLabel = "Events"
                    , menuItemRoute = HomeR
                    , menuItemAccessCallback = True
                    }
                , NavbarLeft $ MenuItem
                    { menuItemLabel = "Curators"
                    , menuItemRoute = CuratorsR
                    , menuItemAccessCallback = True
                    }
                , NavbarRight $ MenuItem
                    { menuItemLabel = "➕ Announce ➕"
                    , menuItemRoute = AdminEventR
                    , menuItemAccessCallback = isJust muser
                    }
                , NavbarRight $ MenuItem
                    { menuItemLabel = userIdent $ snd $ fromMaybe (error "no user") muser
                    , menuItemRoute = ProfileR
                    , menuItemAccessCallback =
                      isJust muser && not (mcurrentRoute == Just (ProfileR))
                    }
                , NavbarRight $ MenuItem
                    { menuItemLabel = "Login"
                    , menuItemRoute = AuthR LoginR
                    , menuItemAccessCallback = isNothing muser
                    }
                , NavbarRight $ MenuItem
                    { menuItemLabel = "Logout"
                    , menuItemRoute = AuthR LogoutR
                    , menuItemAccessCallback = isJust muser && mcurrentRoute == Just (ProfileR)
                    }
                ]

        let navbarLeftMenuItems = [x | NavbarLeft x <- menuItems]
        let navbarRightMenuItems = [x | NavbarRight x <- menuItems]

        let navbarLeftFilteredMenuItems = [x | x <- navbarLeftMenuItems, menuItemAccessCallback x]
        let navbarRightFilteredMenuItems = [x | x <- navbarRightMenuItems, menuItemAccessCallback x]

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_css
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- isAuthorized route isWrite = do
    --   mauth <- maybeAuthPair
    --   mauth `isAuthorizedTo` permissionsRequiredFor route isWrite
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized (StaticR _) _ = return Authorized
    isAuthorized (CuratorR _) _ = return Authorized
    isAuthorized CuratorsR _ = return Authorized

    isAuthorized ProfileR _ = isLoggedIn
    isAuthorized EventsR _ = return Authorized
    isAuthorized (EventR _) _ = return Authorized
    isAuthorized AdminEventR _ = checkAuth canCreateEvent
    isAuthorized AdminCuratorR _ = checkAuth canCreateCurator

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
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

    -- Provide proper Bootstrap styling for default displays, like
    -- error pages
    defaultMessageWidget title body = $(widgetFile "default-message-widget")


checkAuth f = do
  muid <- maybeAuthId
  r <- runDB $ f muid
  return $ if r then Authorized else Unauthorized "must be at least an Admin"

isLoggedIn :: Handler AuthResult
isLoggedIn = do
  muid <- maybeAuthId
  return $ case muid of
      Nothing -> Unauthorized "You must log in to access this page"
      Just _  -> Authorized

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    loginDest _ = HomeR
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = False

    authenticate = runDB . authenticateUser

    authPlugins app = [authGoogleEmail authKey authSecret] ++ extraAuthPlugins
      where
        settings = appSettings app
        authKey = appGoogleAuthKey settings
        authSecret = appGoogleAuthSecret settings

        extraAuthPlugins = [authDummy | appAuthDummyLogin $ appSettings app]

    authHttpManager = getHttpManager

    loginHandler = lift $ do
      app <- getYesod
      murl <- runInputGet $ iopt textField "dest"
      mapM_ setUltDest murl

      defaultLayout $ do
        setTitle' "Login"
        $(widgetFile "login")


instance YesodAuthPersist App

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


runDBor404 :: DB (Maybe a) -> Handler a
runDBor404 dba = do
  ma <- runDB dba
  case ma of
    Nothing -> notFound
    Just a -> return a

setTitle' :: MonadWidget m => Text -> m ()
setTitle' t = setTitle $ toHtml $ t <> " - Dusk"

-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
 
