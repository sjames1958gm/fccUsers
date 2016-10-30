module Main exposing (..)

-- This is where our Elm logic lives.`module SignupForm` declares that this is
-- the SignupForm module, which is how other modules will reference this one
-- if they want to import it and reuse its code.

import Html.App

-- Elms "import" keyword works similarly to "require" in node.js.

import Html exposing (..)


-- The "exposing (..)" option says that we want to bring the Html modules contents
-- into this files current namespace, so that instead of writing out
-- Html.form and Html.label we can use "form" and "label" without the "Html."

import Html.Events exposing (..)


-- This works the same way; we also want to import the entire
-- Html.Events module into the current namespace.

import Html.Attributes exposing (id, type', for, value, class, src, href, target)


-- With this import we are only bringing a few specific functions into our
-- namespace, specifically "id", "type'", "for", "value", and "class".

import Http
import Task exposing (Task)

import FccUsersApi

recentUrl : String
recentUrl = "https://fcctop100.herokuapp.com/api/fccusers/top/recent"
alltimeUrl : String
alltimeUrl = "https://fcctop100.herokuapp.com/api/fccusers/top/alltime"

type ListType
    = ALL_TIME
    | RECENT
    
type alias Model = {
    loading : Bool
    , listType : ListType
    , campers : List FccUsersApi.FccUser
    }

type Msg
    = ALLTIME_AVAILABLE (List FccUsersApi.FccUser)
    | ALLTIME_UNAVAILABLE String
    | RECENT_AVAILABLE (List FccUsersApi.FccUser)
    | RECENT_UNAVAILABLE String
    | SWAP_LISTS
    
swapButton : ListType -> Msg -> Html Msg
swapButton listType msg = 
    button [ onClick msg, class "button"] [ text (if listType == RECENT then "Show All Time" else "Show Recent") ]

viewCampers : Model -> List (Html Msg)
viewCampers model =
    List.indexedMap (\i c -> 
        div [class "row camper"] [
            div [class "columns small-3"] [
                h3 [] [text (toString (i + 1)) ]
            ]
            , div [class "columns small-3"] [
                h3 [] [
                    a [href ("http://www.freecodecamp.com/" ++ c.username), target "_blank"] [ text c.username ]
                ]
            ]
            , div [class "columns small-2"] [
                h3 [] [text (toString (if model.listType == ALL_TIME then c.alltime else c.recent)) ]
            ]
            , div [class "columns small-2"] [
                h3 [] [text (toString (if model.listType == ALL_TIME then c.recent else c.alltime)) ]
            ]
            , div [class "columns small-2"] [
                img [class "icon", src c.img] []
            ]
        ]
    ) model.campers 
    
view : Model -> Html Msg
view model =
    div [ class "row"] [
        div [class "header columns medium-11 medium-offset-1"] [
            if (model.loading) then
                h1 [] [ text "Loading . . ." ]
            else
                h1 [] [ text ((if model.listType == RECENT then "Recent" else "All-time") ++ " FCC Top Campers in ELM") ]
            , swapButton model.listType SWAP_LISTS
            
            , div [class "row headings"] [
                div [class "columns small-3"] [
                    h4 [] [text "Rank"]
                ]
                , div [class "columns small-3"] [
                    h4 [] [text "Name"]
                ]
                , div [class "columns small-2"] [
                    h4 [] [text (if model.listType == ALL_TIME then "All-time Score" else "Recent Score")]
                ]
                , div [class "columns small-2"] [
                    h4 [] [text (if model.listType == ALL_TIME then "Recent Score" else "All-time Score")]
                ]
                , div [class "columns small-2"] [
                    h4 [] [text "Image"]
                ]
            ]
        ]
        , div [class "columns medium-11 medium-offset-1"] (viewCampers model)
    ]

initialModel : Model
initialModel = {
    loading = True
    , listType = ALL_TIME
    , campers = []
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ALLTIME_AVAILABLE fccUsers ->
            ( { model | campers = fccUsers, listType = ALL_TIME, loading = False}, Cmd.none)
        ALLTIME_UNAVAILABLE errMsg ->
            (model, Cmd.none)
        RECENT_AVAILABLE fccUsers ->
            ( { model | campers = fccUsers, listType = RECENT, loading = False}, Cmd.none)
        RECENT_UNAVAILABLE errMsg ->
            (model, Cmd.none)
        SWAP_LISTS ->
            if (model.listType == ALL_TIME) then getRecent model else getAllTime model

getRecent : Model -> ( Model, Cmd Msg )
getRecent model =
    let
        failureToMsg err =
            RECENT_UNAVAILABLE "Failed to get Recent campers"
    
        successToMsg result =
            RECENT_AVAILABLE result
    
        request =
            Http.get FccUsersApi.decodeFccUsers recentUrl
    
        cmd =
            Task.perform failureToMsg successToMsg request
    in
        ({model | loading = True, listType = RECENT}, cmd)

getAllTime : Model -> ( Model, Cmd Msg )
getAllTime model =
    let
        failureToMsg err =
            ALLTIME_UNAVAILABLE "Failed to get All Time campers"
    
        successToMsg result =
            ALLTIME_AVAILABLE result
    
        request =
            Http.get FccUsersApi.decodeFccUsers alltimeUrl
    
        cmd =
            Task.perform failureToMsg successToMsg request
    in
        ({model | loading = True, listType = ALL_TIME}, cmd)


init : ( Model, Cmd Msg )
init = getRecent initialModel

main : Program Never
main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

