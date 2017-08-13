module Page.Home exposing (Model, Msg, init, update, view)

{-| The homepage. You can get here via either the / or /#/ routes. -}

import Data.Session as Session exposing (Session)
import Html exposing (..)
import Request.Event
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder)
import Html.Events exposing (onClick)
import Http
import Page.Errored as Errored exposing (PageLoadError, pageLoadError)
import Task exposing (Task)
import Views.Page as Page
import Util exposing ((=>), formatDate)
import Data.Gen
import Date exposing (..)


-- MODEL --
type alias Model =
    { events : List Data.Gen.Event
    }


init : Session -> Task PageLoadError Model
init session =
    let
        maybeAuthToken =
            session.user
                |> Maybe.map .token

        loadEvents =
            Request.Event.events |> Http.toTask

        handleLoadError _ =
            "Events failed to load"
                |> pageLoadError Page.Home
    in
    Task.map Model loadEvents
        |> Task.mapError handleLoadError

-- VIEW --


view : Session -> Model -> Html Msg
view session model =
    div [ class "home-page" ]
        [ viewBanner
        , div [ class "container page" ]
            [ div [ class "row" ]
                [ div [ class "col-md-12" ]
                    [ div [ class "sidebar" ]
                        [ p [] [ text "Events" ]
                        , viewEvents model.events
                        ]
                    ]
                ]
            ]
        ]


viewBanner : Html Msg
viewBanner =
    div [ class "banner" ]
        [ div [ class "container" ]
            [ h1 [ class "logo-font" ] [ text "dusk" ]
            , p [] [ text "Curated events" ]
            ]
        ]


viewEvents : List Data.Gen.Event -> Html Msg
viewEvents events =
    div [ class "event-list" ] (List.map viewEvent events)


viewEvent : Data.Gen.Event -> Html Msg
viewEvent event =
    div [ class "row" ]
        [ div [ class "col-sm-3" ]
              [ h3 [class "event-time" ]
                   [ p [] [ text (formatDate event.eventStart_datetime) ] ]
              ]
        , div [ class "col-sm-3" ]
              [ a
                  [ class "event-image event-default"
                  , Html.Attributes.height 250
                  , Html.Attributes.width 250
                  , href "javascript:void(0)"
                  , onClick (SelectEvent event.eventId)
                  ]
                  [ text event.eventName ]
              ]
         ]




-- UPDATE --


type Msg
    = SelectEvent Int


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        SelectEvent eventId ->
            model => Cmd.none
                    -- (Maybe.map .token session.user) -- eventId