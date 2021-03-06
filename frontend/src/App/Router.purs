module App.Router where

import App.Data.Profile (Profile(..), decodeProfile)
import Component.Admin.Main as Admin
import Component.Auth as Auth
import Component.Curator as Curator
import Component.Event as Event
import Component.NotFound as NotFound
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import Control.Monad.State.Class (modify)
import Data.Either.Nested (Either4)
import Data.Functor.Coproduct.Nested (Coproduct4)
import Halogen as H
import Halogen.Component.ChildPath (ChildPath, cp1, cp2, cp3, cp4)
import Halogen.HTML as HH
import Halogen.HTML hiding (map)
import Halogen.HTML.Events (input_, onClick)
import Halogen.HTML.Properties (class_, href, id_) as HP
import Halogen.HTML.Properties.ARIA (hidden, role) as HP
import Helper (Message(..), apiUrl, flashMessage, styleClass, styleClassIf)
import Import hiding (div)
import Message as Msg
import Network.HTTP.Affjax as AX
import Routes (Input(..), Location(..), ChildAction(..))
import Routing (matchesAff)
import Routing.Match (Match)
import Routing.Match.Class (int, lit, fail)


oneSlash :: Match Unit
oneSlash = lit "/"

homeSlash :: Match Unit
homeSlash = lit ""

routing :: Match Location
routing =
  events      <|>
  admins      <|>
  login       <|>
  curators    <|>
  event       <|>
  home        <|>
  notFound
  where
    home = HomeR <$ lit ""
    login = LoginR <$ lit "login"
    events = EventsR <$ lit "events"
    admins = AdminR <$ lit "admin"
    adminPath = lit "admin" *> homeSlash
    event = EventR <$> (homeSlash *> lit "events" *> int)
    curators = CuratorsR <$ lit "curators"
    notFound = NotFoundR <$> fail "Not Found"


type State =
  { currentPage :: Location
  , currentUser :: Maybe Profile
  , checkingUser :: Boolean
  }

type ChildQuery = Coproduct4 Curator.Input Event.Input Auth.Input Admin.Input
type ChildSlot = Either4 Curator.Slot Event.Slot Auth.Slot Admin.Slot

pathToCurators :: ChildPath Curator.Input ChildQuery Curator.Slot ChildSlot
pathToCurators = cp1

pathToEvents :: ChildPath Event.Input ChildQuery Event.Slot ChildSlot
pathToEvents = cp2

pathToAuth :: ChildPath Auth.Input ChildQuery Auth.Slot ChildSlot
pathToAuth = cp3

pathToAdmin :: ChildPath Admin.Input ChildQuery Admin.Slot ChildSlot
pathToAdmin = cp4

ui :: H.Component HH.HTML Input Unit ChildAction Top
ui = H.lifecycleParentComponent
  { initialState: const init
  , initializer: Just (H.action CheckProfile)
  , finalizer: Nothing
  , render
  , eval
  , receiver: const Nothing
  }
  where
    render :: State -> H.ParentHTML Input ChildQuery ChildSlot Top
    render st =
      mainBody st (viewPage st.currentPage)

    init :: State
    init = { currentUser: Nothing, currentPage: HomeR, checkingUser: false }

    viewPage :: Location -> H.ParentHTML Input ChildQuery ChildSlot Top
    viewPage AdminR = do
      HH.slot' pathToAdmin Admin.Slot Admin.ui unit listen
    viewPage LoginR = do
      HH.slot' pathToAuth Auth.Slot Auth.ui unit listen
    viewPage EventsR = do
      HH.slot' pathToEvents Event.Slot Event.ui unit listen
    viewPage CuratorsR = do
      HH.slot' pathToCurators Curator.Slot Curator.ui unit listen
    viewPage HomeR =
      HH.slot' pathToEvents Event.Slot Event.ui unit listen
    viewPage s = NotFound.view (show s)

    listen :: ChildAction -> Maybe (Input Unit)
    listen = Just <<< case _ of
      Redirect loc -> H.action $ Goto loc

    eval :: Input ~> H.ParentDSL State Input ChildQuery ChildSlot ChildAction Top
    eval (Noop next) = pure next
    eval (CheckProfile next) = do
      modify (_ { checkingUser = true })
      response <- H.liftAff $ AX.get (apiUrl <> "/profile")
      user <- pure $ decodeProfile response.response
      modify (_ { currentUser = hush user, checkingUser = false })
      pure next
    eval (Goto ProfileR next) = do
      modify (_ { currentPage = ProfileR })
      pure next
    eval (Goto CuratorsR next) = do
      modify (_ { currentPage = CuratorsR })
      pure next
    eval (Goto EventsR next) = do
      modify (_ { currentPage = EventsR })
      pure next
    eval (Goto AdminR next) = do
      s <- H.get
      if isJust s.currentUser
        then do
          modify (_ { currentPage = AdminR })
          pure next
        else do
          _ <- H.liftEff $ flashMessage Info Msg.loginRequired
          modify (_ { currentPage = LoginR })
          pure next
    eval (Goto LoginR next) = do
      modify (_ { currentPage = LoginR })
      pure next
    eval (Logout next) = do
      response <- H.liftAff $ AX.post (apiUrl <> "/auth/logout") unit
      H.liftAff $ log $ "logged out successfully: " <> response.response
      H.liftEff $ flashMessage Success Msg.loggedOut
      modify (_ { currentUser = Nothing
                , currentPage = HomeR })
      pure next
    eval (Goto (EventR i) next) = do
      modify (_ { currentPage = EventR i })
      pure next
    eval (Goto HomeR next) = do
      modify (_ { currentPage = HomeR })
      pure next
    eval (Goto (NotFoundR s) next) = do
      modify (_ { currentPage = (NotFoundR s) })
      pure next


routeSignal :: H.HalogenIO Input ChildAction (Aff TopEffects)
            -> Aff TopEffects Unit
routeSignal driver = do
  Tuple old new <- matchesAff routing
  redirects driver old new
  where
  redirects d _old =
    d.query <<< H.action <<< Goto


viewBanner =
    div [ styleClass "banner masthead" ]
        [ div [ styleClass "container" ]
          [ div [ styleClass "row" ]
            [ h1 [ styleClass "header logo" ] [ text "DUSK" ]
            , h2_ [ text "Curated Nightlife" ]
            ]
          ]
        ]

mainBody st sub =
  div [ HP.class_ $ ClassName "home-page" ]
    [ navbar st
    , if st.currentPage == HomeR then viewBanner else p_ [text ""]
    , div [styleClass "container" ]
      [ div [ styleClass "row", HP.id_ "app-messages" ]
        [ div [styleClass "text-center alert", HP.hidden "true", HP.role "alert"]
          [ p [ HP.id_ "app-message" ] [] ]
        ]
      ]
    , sub ]

navbar st =
  nav [ styleClass "navbar navbar-default navbar-static-top" ]
    [ div [ styleClass "container" ]
      [ div [ styleClass "navbar-header" ]
        [ button [ styleClass "navbar-toggle collapsed"]
          [ span [ styleClass "icon-bar"] []
          , span [ styleClass "icon-bar"] []
          , span [ styleClass "icon-bar"] [] ]
        ]
      , div [ HP.id_ "navbar", styleClass "collapse navbar-collapse" ]
        [ ul [ styleClass "nav navbar-nav"]
          [ li [ checkActiveLogo st HomeR ] [ a [ HP.href "/" ] [text "DUSK"] ]
          , li [ checkActive st EventsR ] [ a [ HP.href "#events"] [text "Events"] ]
          , li [ checkActive st CuratorsR ][ a [ HP.href "#curators"] [text "Curators"] ]
          ]
        , ul [ styleClass "nav navbar-nav navbar-right"]
          (maybeAdminNavs st)
        ]
      ]
    ]
  where
    checkActiveLogo st r = styleClassIf (isActive st r) "active-logo logo"
    checkActive st r = if isActive st r then styleClass "active" else styleClass ""
    isActive st r = st.currentPage == r

    maybeAdminNavs st
      | st.checkingUser = [ li [ styleClass "spinner"] [ text "loading"] ]
      | isJust st.currentUser =
          [ li [ checkActive st AdminR ]
            [ a [ HP.href "#admin"]
              [ text (fromMaybe "Dashboard" $ getPreferredName <$> st.currentUser ) ] ]
          , li_
            [ a [ HP.href "#", onClick (input_ Logout) ]
              [ text "Logout" ]
            ]
          ]
      | otherwise =
          [ li [ checkActive st LoginR ]
            [ a [ HP.href "#login"]
              [ text "Login" ]
            ]
          ]

getPreferredName :: Profile -> String
getPreferredName (Profile { user_id, user_ident, user_name }) =
  fromMaybe user_ident user_name
