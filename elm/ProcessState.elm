module ProcessState where
import Http
import StartApp
import Html exposing (..)
import Html.Shorthand exposing (..)
import Bootstrap.Html exposing (..)
import Html.Attributes exposing (style, class, type', value, id)
import Html.Events exposing (onClick, onKeyPress, on, targetValue)
import Task exposing (Task)
import Effects exposing (Never)
import Json.Decode as Json exposing(Value)
import Http exposing (get, getString)
import Effects exposing (Effects)
import Json.Encode
import Process
import ErlangTerm as ET exposing (..)

-- MODEL

type alias Model = { pid : Process.Pid
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
type Action = ReceiveProcessState State
            | Filter String
            | ETermAction ET.Action
            | Error String

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    ReceiveProcessState state ->
      ( {model | state = Just (ET.setup state)}
      , Effects.none
      )

    Filter f ->
      let t' = Maybe.map (ET.update <| ET.Filter f) model.state
      in
        ( {model | state = t', filter = f}
      , Effects.none
      )

    ETermAction a ->
      let t' = Maybe.map (ET.update a) model.state
      in
        ( {model | state = t'}
        , Effects.none
        )

    Error s ->
      ( {model | error = s}
      , Effects.none
      )

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div_
  [
   div_ [input [type' "text", value model.filter
                , on "input" targetValue (\str -> Signal.message address (Filter str)) ] []]
  , Maybe.withDefault (text model.error)
         (Maybe.map (ET.view (Signal.forwardTo address ETermAction)) model.state)
  ]

-- AJAX

getProcessState pid =
  let
    request = get ET.decode <| "/json/obs.json?pid=" ++ pid
    result = (Task.map ReceiveProcessState request)
  in
    Effects.task (Task.onError result (\err -> Task.succeed (Error (toString err))))

