import Http
import Html.App as Html
import ErlangTerm exposing (..)

main =
    Html.program
    { init = (test, Cmd.none)
    , update = \msg-> \model-> (update msg model, Cmd.none)
    , view = view
    , subscriptions = \_ -> Sub.none
    }

p = EBasic ErlangTerm.dm <| EString "test"

test = nested 2 2
list r e = EList ErlangTerm.dm <| List.repeat r e

nested d b =
  if d > 0 then
    EList ErlangTerm.dm <| [p] ++ List.repeat b (nested (d-1) b)
  else
    list b p
