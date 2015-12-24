import ProcessList exposing (init, update, view)
import StartApp
import Task exposing (Task)
import Effects exposing (Never)
import ErlangTerm as ET exposing (ETerm, encodeWithoutTags)
import Json.Encode

app =
  StartApp.start
    { init = init []
    , update = update
    , view = view
    , inputs = []
    }

main =
  app.html

port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks

port state_json : Signal Json.Encode.Value
port state_json =
  Signal.filterMap (\x-> Maybe.map ET.encodeWithoutTags <| getState x) Json.Encode.null  app.model
getState : ProcessList.Model -> Maybe ET.ETerm
getState s =
   s.state `Maybe.andThen` .state
