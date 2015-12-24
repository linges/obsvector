module ProcessList(init, update, view, Model) where

import Html exposing (..)
import Html.Shorthand exposing (..)
import Html.Attributes exposing (style, class, type', value, id)
import Html.Events exposing (onClick, onKeyPress, on, targetValue)
import Process
import List exposing (map, filter)
import Json.Decode exposing (list, string, succeed)
import Task exposing (Task)
import Http exposing (get)
import Effects exposing (Effects)
import ProcessState as State
import String

-- MODEL

type alias Model = { processList : ProcessList
                   , filter : String
                   , error : String
                   , selected : Maybe Process.Pid
                   , state : Maybe State.Model
                   }


init current =
  ( { processList = current
    , filter = ""
    , error = ""
    , selected = Nothing
    , state = Nothing
    }
  , getProcessList
  )

-- UPDATE
type alias ProcessList = List Process.Model

type Action = Refetch
            | ReceiveProcessList ProcessList
            | Filter String
            | Error String
            | Selected Process.Pid
            | StateAction Process.Pid State.Action

update : Action -> Model -> (Model, Effects Action)
update action model =

  case action of
    Refetch ->
      (model, getProcessList)

    ReceiveProcessList processList ->
      ( {model | processList = processList}
      , Effects.none
      )

    Filter f ->
      ( {model | filter = f}
      , Effects.none
      )

    Error s ->
      ( {model | error = s}
      , Effects.none
      )

    Selected p ->
      let (state, eff) = State.init p
      in
      ( {model | selected = Just p
               , state = Just state}
      , Effects.map (StateAction p) eff
      )

    StateAction pid action ->
      case model.state of
        Nothing ->
          ( model
          , Effects.none)
        Just state ->
          if state.pid == pid then
            let (state, eff) = (State.update action) state
            in
              ( {model | state = Just state}
              , Effects.map (StateAction state.pid) eff
              )
          else
            ( model
            , Effects.none)

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div [class "wrapper", style [("display", "flex")]]
        [ left address model
        , right address model
        ]


right address model =
  div [style [("order", "2"), ("max-width", "50%")]]
        ( [ h1 [] [text "State"]
         , div [id "state"] []
        ]
         ++ (Maybe.withDefault []
                    <| Maybe.map
                         (\s -> [State.view (Signal.forwardTo address (StateAction s.pid)) s])
                         model.state)
        )

left address model =
  div [class "wrapper", style [("order", "1")]]
        [ h1 [] [text "Process List"]
        , span [] [text model.error, text <| Maybe.withDefault "" <| Maybe.map (.error) model.state]
        , input [ type' "text", value model.filter
                , on "input" targetValue (\str -> Signal.message address (Filter str)) ] []
        , viewProcessList address model
        ]

viewProcessList address model =
  let
      filterArgs = String.split " " model.filter
      filtered = filter (Process.filter filterArgs) model.processList
      passon = Signal.forwardTo address Selected
      isSelected = \p -> Just p.pid == model.selected
  in
  table_ ([Process.thead]
           ++ (map (\p -> Process.viewTr passon p <| isSelected p ) filtered))

-- AJAX

getProcessList =
  let
    request = get (list Process.decode) "/json/obs.json"
    result = (Task.map ReceiveProcessList request)
  in
    Effects.task (Task.onError result (\err -> Task.succeed (Error (toString err))))
