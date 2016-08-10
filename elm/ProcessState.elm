module ProcessState exposing (..)
import Http
import Html exposing (..)
import Html.Shorthand exposing (..)
import Bootstrap.Html exposing (..)
import Html.Attributes exposing (style, class, type', value, id)
import Html.Events exposing (onInput)
import Task exposing (Task)
import Json.Decode as Json exposing(Value)
import Http exposing (get, getString)
import Json.Encode
import ErlangProcess as EP
import ErlangTerm as ET exposing (..)
import Html.App exposing (map)

-- MODEL

type alias Model = { pid : EP.Pid
                   , state : Maybe State
                   , filter : String
                   , error : String
                   }
type alias State = ETerm

init pid =
  ( { pid = pid
    , state = Nothing
    , filter = ""
    , error = ""}
    , getProcessState pid
  )

-- UPDATE
type alias ID = Int
type Msg = ReceiveProcessState State
         | Filter String
         | ETermMsg ET.Msg
         | Error String

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    ReceiveProcessState state ->
      ( {model | state = Just (ET.setup state)}
      , Cmd.none
      )

    Filter f ->
      let t' = Maybe.map (ET.update <| ET.Filter f) model.state
      in
        ( {model | state = t', filter = f}
      , Cmd.none
      )

    ETermMsg a ->
      let t' = Maybe.map (ET.update a) model.state
      in
        ( {model | state = t'}
        , Cmd.none
        )

    Error s ->
      ( {model | error = s}
      , Cmd.none
      )

-- VIEW

view : Model -> Html Msg
view model =
  div_
  [
   div_ [input [type' "text", value model.filter
                , onInput (\str -> Filter str) ] []]
  , Maybe.withDefault (text model.error)
         (Maybe.map (\s-> Html.App.map ETermMsg (ET.view s)) model.state)
  ]


-- AJAX

getProcessState pid =
  let
    fetchTask = get ET.decode <| "/json/obs.json?pid=" ++ pid
    fetchError = (\err -> (Error (toString err)))
    fetchSuccess = ReceiveProcessState
  in
    Task.perform fetchError fetchSuccess fetchTask
