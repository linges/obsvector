import ProcessList exposing (init, update, view, getProcessList, Msg)
import Html.App as Html
import ErlangTerm as ET exposing (ETerm, encodeWithoutTags)
import Json.Encode
import Time exposing (millisecond, Time, every)

main =
    Html.program
    { init = init []
    , update = update
    , view = view
    , subscriptions = \_ -> every (300 * millisecond) ProcessList.Tick
    }
