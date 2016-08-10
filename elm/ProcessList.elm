module ProcessList exposing (init, update, view, Model, getProcessList)

import Html exposing (..)
import Html.Shorthand exposing (..)
import Html.Attributes exposing (style, class, type', value, id)
import Html.Events exposing (onClick, onInput)
import ErlangProcess as EP
import List exposing (map, filter)
import Json.Decode exposing (list, string, succeed)
import Task exposing (Task)
import Http exposing (get)
import ProcessState as State
import String
import Html.App
import Debug

-- MODEL

type alias Model = { processList : ProcessList
                   , filter : String
                   , error : String
                   , selected : Maybe EP.Pid
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
type alias ProcessList = List EP.Model

type Msg = Refetch
         | ReceiveProcessList ProcessList
         | Filter String
         | Error String
         | Selected EP.Pid
         | StateMsg EP.Pid State.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update action model =

  case (Debug.log "ac" action) of
    Refetch ->
      (model, getProcessList)

    ReceiveProcessList processList ->
      ( {model | processList = processList}
      , Cmd.none
      )

    Filter f ->
      ( {model | filter = f}
      , Cmd.none
      )

    Error s ->
      ( {model | error = s}
      , Cmd.none
      )

    Selected p ->
      let (state, eff) = State.init p
      in
      ( {model | selected = Just p
               , state = Just state}
      , Cmd.map (StateMsg p) eff
      )

    StateMsg pid action ->
      case model.state of
        Nothing ->
          ( model
          , Cmd.none)
        Just state ->
          if state.pid == pid then
            let (state, eff) = (State.update action) state
            in
              ( {model | state = Just state}
              , Cmd.map (StateMsg state.pid) eff
              )
          else
            ( model
            , Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  div [class "wrapper", style [("display", "flex")]]
        [ left model
        , right model
        ]


right model =
  div [style [("order", "2"), ("max-width", "50%")]]
        ( [ h1 [] [text "State"]
         , div [id "state"] []
        ]
         ++ (Maybe.withDefault []
                    <| Maybe.map
                         (\s -> [Html.App.map (StateMsg s.pid) (State.view s)])
                         model.state)
        )

left model =
  div [class "wrapper", style [("order", "1")]]
        [ h1 [] [text "Process List"]
        , span [] [text model.error, text <| Maybe.withDefault "" <| Maybe.map (.error) model.state]
        , input [ type' "text", value model.filter
                , onInput Filter ] []
        , viewProcessList model
        ]
viewProcessList : Model -> Html Msg
viewProcessList model =
  let
      filterArgs = String.split " " model.filter
      filtered = filter (EP.filter filterArgs) model.processList
      isSelected = \p -> Just p.pid == model.selected
      viewPL = table_ ([EP.thead]
                           ++ (map (\p -> EP.viewTr p <| isSelected p ) filtered))
  in
      Html.App.map Selected viewPL

-- AJAX

getProcessList =
  let
    fetchTask = get (list EP.decode) "/json/obs.json"
    fetchError = (\err -> (Error (toString err)))
    fetchSuccess = ReceiveProcessList
  in
    Task.perform fetchError fetchSuccess fetchTask
