{-# OPTIONS_GHC -fno-warn-orphans #-}

module Routes where

import Import.NoFoundation
import AppType

mkYesodData "App" [parseRoutes|
/static StaticR Static appStatic
/auth   AuthR   Auth   getAuth

/favicon.ico FaviconR GET
/robots.txt RobotsR GET

/ HomeR GET

/events EventR GET POST

/profile ProfileR GET
|]

-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
