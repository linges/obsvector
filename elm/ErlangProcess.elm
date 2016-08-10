module ErlangProcess exposing (update, viewTr, Model, Pid, decode, filter, thead, Msg)

import Html exposing (..)
import Html.Shorthand exposing (..)
import Html.Attributes exposing (style, align)
import Html.Events exposing (onClick)
import Task exposing (Task)
import Json.Decode as Json exposing(..)
import String exposing (contains)

-- MODEL

type alias Model = { pid : String
                   , current_function : Code
                   , initial_call : Code
                   , registered_name : Maybe String
                   , status : String
                   }
type alias Code = { mod : String
                  , fun : String
                  , args : Int
                  }
type alias Pid = String

-- UPDATE

type Msg = Click

update : Msg -> Model -> Model
update action model =
  model


-- VIEW

viewTr : Model -> Bool -> Html Pid
viewTr model selected =
   tbody_
      [ tr ((if selected then [highlight] else []) ++ [onClick model.pid])
        [ td_ [text model.pid]
        , td_ [(Maybe.withDefault (viewCode model.initial_call) <| Maybe.map text model.registered_name)]
        , td_ [viewCode model.current_function]
        , td_ [text model.status]
        ]
      ]
viewCode =
  text << showCode

showCode v =
  String.join "" ["fun " ++ v.mod, ":", v.fun, "/", toString v.args]

thead =
  Html.thead [align "left"]
          [ th [style [("width", "80px")]] [text "Pid"]
          , th [style [("width", "300px")]] [text "Registered name or initial call"]
          , th [style [("width", "250px")]] [text "Current function"]
          , th [style [("width", "80px")]] [text "Status"]
          ]

highlight =
  style [("backgroundColor", "grey")]

filter: List String -> Model -> Bool
filter fs p =
  let oneword =
        \f -> List.any (contains f) candidats
      candidats =
        [ showCode p.current_function
        ,  Maybe.withDefault ""  p.registered_name
        , showCode p.initial_call
        , p.pid ]
  in
    List.all oneword fs

-- JSON

apply : Json.Decoder (a -> b) -> Json.Decoder a -> Json.Decoder b
apply func value =
    Json.object2 (<|) func value

decode = object5 Model
          ("pid" := string)
          ("current_function" := decodeCode)
          ("initial_call" := decodeCode)
          (maybe ("registered_name" := string))
          ("status" := string)

decodeCode = tuple3 Code string string int
