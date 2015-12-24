import Http
import StartApp
import Task exposing (Task)
import Effects exposing (Never)
import ErlangTerm  exposing (..)

app =
  StartApp.start
    { init = (test, Effects.none)
    , update = \x -> \y ->  (y, Effects.none)
    , view = \a -> view
    , inputs = []
    }

main =
  app.html

port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks

p = EBasic {} <| EString "test"

test = nested 2 2
list r e = EList {} <| List.repeat r e

nested d b =
  if d > 0 then
    EList {}  <| [p] ++ List.repeat b (nested (d-1) b)
  else
    list b p
    
