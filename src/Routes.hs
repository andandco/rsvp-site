{-# OPTIONS_GHC -fno-warn-orphans #-}

module Routes where

import Import.NoFoundation
import AppType


uploadDirectory :: FilePath
uploadDirectory = "static/upload"

imageFilePath :: String -> FilePath
imageFilePath f = uploadDirectory </> f


-- http://www.yesodweb.com/book/routing-and-handlers
mkYesodData "App" [parseRoutes|
/static       StaticR Static appStatic
/favicon.ico  FaviconR GET
/robots.txt   RobotsR  GET

/auth         AuthR Auth   getAuth

/                  HomeR GET
/events            EventsR GET
/events/#EventId   EventR GET
/admin/events      AdminEventR GET POST
/admin/curators    AdminCuratorR POST
/profile           ProfileR GET
/curators          CuratorsR GET
/curators/#UserId  CuratorR GET
|]

-- /signup      SignupR  GET POST
-- /signout     SignoutR GET
