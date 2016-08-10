import ProcessList exposing (init, update, view, getProcessList)
import Html.App as Html
import ErlangTerm as ET exposing (ETerm, encodeWithoutTags)
import Json.Encode

main =
    Html.program
    { init = init []
    , update = update
    , view = view
    , subscriptions = \_ -> Sub.none
    }
