module ErlangTerm exposing (ETerm(..), EPrim(..), Msg(Filter)
                           , update, decode, view, encodeWithoutTags
                           , setup, dm)

import Html exposing (..)
import Html.Shorthand exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, on, defaultOptions, onWithOptions)
import Task exposing (Task)
import Json.Decode as De exposing(..)
import Json.Encode as En exposing(..)
import String exposing (contains)
import Material.Icons.Content as Material exposing (..)
import Color exposing (..)
import Debug exposing (..)

-- MODEL
-- Missing: Ports, Records, Proplist?
type alias Meta = {id : Int, fold : Bool, highlight: Bool}
dm : Meta
dm = {id = -1, fold = True, highlight = False}

type EPrim = EInt Int
           | EFloat Float
           | EAtom String
           | EBool Bool
           | EPid String
           | EString String
           | EBinaryString String
           | EBinary
           | EFun String
           | ERef String
           | NYI

type ETerm =  ETuple Meta (List ETerm)
           | EList Meta (List ETerm)
           | EMap Meta (List (ETerm, ETerm))
           | EBasic Meta EPrim

initbasic = EBasic dm

setup t =
  setIds (unfold 1 t) 0

metaMap i f term =
  case term of
    ETuple m ts ->
      if m.id == i then ETuple (f m) ts else ETuple m (List.map (metaMap i f) ts)
    EMap m kv ->
      if m.id == i then EMap (f m) kv
      else EMap m (List.map (tuplemap (metaMap i f)) kv)
    EList m l ->
      if m.id == i then EList (f m) l else EList m (List.map (metaMap i f) l)
    EBasic m p ->
      EBasic (f m) p

mapETerm f term =
  case term of
    ETuple m ts -> f (ETuple m (List.map f ts))
    EMap m kv -> f (EMap m (List.map (tuplemap f) kv))
    EList m l -> f (EList m (List.map f l))
    EBasic m p -> f term

tuplemap : (a -> b) -> (a,a) -> (b,b)
tuplemap f t =
 (f <| fst t, f <| snd t)

-- UPDATE

type Msg = Click Int
         | Filter String

update : Msg -> ETerm -> ETerm
update action model =
  case action of
    Click i ->
      metaMap i (\m->{m|fold = not m.fold}) model

    Filter f ->
      (fst<<setHighLight f) model

setHighLight f term =
  case term of
    ETuple m l ->
      let (l', bs) = List.unzip <| List.map (setHighLight f) l
          b = List.any identity bs
      in
        (ETuple {m | highlight = b} l', b)
    EMap m kv ->
      let (l', bs) = mapHelper <| List.map (tuplemap <| setHighLight f) kv
          b = List.any identity bs
      in
        (EMap {m | highlight = b} l', b)
    EList m l ->
      let (l', bs) = List.unzip <| List.map (setHighLight f) l
          b = List.any identity bs
      in
        (EList {m | highlight = b} l', b)
    EBasic m p ->
      let b = matchesFilter f p in
      (EBasic {m | highlight = b} p, b)

mapHelper: List ((ETerm, Bool), (ETerm, Bool)) -> (List (ETerm, ETerm), List Bool)
mapHelper kvs =
  List.unzip <| List.map (\kv -> ((fst>>fst <| kv, snd>>fst <| kv), (fst>>snd <| kv) || (snd>>snd <| kv))) kvs

matchesFilter filter prim =
  if filter == "" then
    False
  else
    let
      filterArgs = String.split " " filter
      f = \s -> List.all (\x -> String.contains x s) filterArgs
    in
      case prim of
        EInt i -> f <| toString i
        EFloat n -> f <| toString n
        EAtom a -> f a
        EBool b -> f <| toString b
        EPid p -> f <| toString p
        EString s -> f <| "\"" ++ s ++ "\""
        EBinary -> f "<<blob>>"
        EBinaryString s -> f s
        EFun b -> f <| toString b
        ERef r -> f r
        _ -> False

-- VIEW

viewEPrim : EPrim -> Html Msg
viewEPrim term =
  case term of
    EInt i -> text <| toString i
    EFloat f -> text <| toString f
    EAtom a -> text a
    EBool b -> text <| toString b
    EPid p -> text <| toString p
    EString s -> text <| "\"" ++ s ++ "\""
    EBinary -> text "<<blob>>"
    EBinaryString s -> text <| "<<\"" ++ s ++ "\">>"
    EFun b -> text <| toString b
    ERef r -> text r
    NYI -> text "NYI"

view : ETerm -> Html Msg
view term =
  case term of
    ETuple m ts -> viewTuple m ts
    EMap m kv -> viewMap m kv
    EList m l -> viewList m l
    EBasic m p -> red m <| viewEPrim p

red m h =
  if m.highlight then
    span [style [( "background-color", "yellow" )]] [h]
  else
    h
clickEvent : Meta -> List (Html Msg) -> Html Msg
clickEvent m h =
  let
    opt = {defaultOptions | stopPropagation = True}
    clck = onWithOptions "click" opt (De.succeed (Click m.id))
  in
    span [clck, style [("cursor", "pointer")]] h

add =
  Material.add_circle_outline Color.lightBlue 12
remove =
  Material.remove_circle_outline Color.lightBlue 12

viewTuple m ts =
  if m.fold && not m.highlight then
    clickEvent m [text "{", add, text "}"]
  else
    div_ <| [clickEvent m [text "{", remove]]
          ++ (join (\x->\s-> span_ [view x, s]) ts)
          ++ [clickEvent m [text "}"]]

viewList m ts =
  if m.fold && not m.highlight then
    clickEvent m [text "[", add, text "]"]
  else
    (ul [style [("margin-left", "20px"), ("padding-left", "15px")]] <|
         [clickEvent m [text "[", remove]]
         ++ (join (\x->\s-> li [style ilStyles] [view x, s]) ts)
         ++ [clickEvent m [text "]" ]])

viewMap m kv =
  if List.isEmpty kv then viewMapEmpty else viewMapNormal m kv

viewMapEmpty =
  text "#{}"
viewMapNormal m kv =
  if m.fold && not m.highlight then
    clickEvent m [text "#{", add, text "}"]
  else
  div [style [("margin-left", "20px")]]
       [ clickEvent m [text "#{", remove]
       , ul [style [("padding-left", "15px")]]
              <| join (\kv->\s-> li [style ilStyles]
                       [view (fst kv)
                       , text " => "
                       , view (snd kv), s]) kv
       , clickEvent m [text "}"] ]

ilStyles =
  [ ("list-style-position", "inside")
  , ("display", "block")
  , ("padding-left", "5px")
  ]

join : (a -> Html Msg -> Html Msg) -> List a -> List (Html Msg)
join f xs =
  let
  sep = span [style []] [text ", "]
  helper =
    \ys ->
      case ys of
        [] -> []
        hd::[] -> [f hd <| text ""]
        hd::tl -> [f hd sep] ++ (helper tl)
  in
    helper xs

sepfloat : Html Msg -> Html Msg
sepfloat s =
  span [style [("display", "inline")]] [s]

-- JSON
encodePrim : EPrim -> En.Value
encodePrim term =
  case term of
    EInt i ->  En.int i
    EFloat f -> En.float f
    EAtom a -> En.string a
    EBool b -> En.bool b
    EPid p -> En.string p
    EString s -> En.string <| "\"" ++ s ++ "\""
    EBinaryString s -> En.string <| "<<\"" ++ s ++ "\">>"
    EBinary -> En.string "<<binary>>"
    EFun b -> En.string b
    ERef r -> En.string ("ref:"++r)
    NYI -> En.string "NYI"

encodeWithoutTags : ETerm -> En.Value
encodeWithoutTags term =
  case term of
    ETuple m ts -> En.list <| List.map encodeWithoutTags ts
    EMap m kv -> En.object <| List.map ( \kv-> (mapKeyToString (fst kv), encodeWithoutTags (snd kv))) kv
    EList m l -> En.list <| List.map encodeWithoutTags l
    EBasic m p -> encodePrim p


mapKeyToString k =
  case k of
    EBasic m (EString s) -> s
    EBasic m (EAtom a) -> a
    _ -> toString k

decode : Decoder ETerm
decode =
  ("tag" := De.string)
  `andThen` decodeTag

decodeTag tag =
  "term" :=
    case tag of
        "int" -> map ( initbasic << EInt ) De.int
        "float" -> map ( initbasic << EFloat ) De.float
        "atom" -> map ( initbasic << EAtom ) De.string
        "bool" -> map ( initbasic << EBool ) De.bool
        "pid" -> map ( initbasic << EPid ) De.string
        "string" -> map ( initbasic << EString ) De.string
        "binary_string" -> map ( initbasic << EBinaryString ) De.string
        "binary" -> De.succeed (initbasic EBinary)
        "fun" -> map ( initbasic << EFun ) De.string
        "ref" -> map ( initbasic << ERef ) De.string
        "tuple" -> map (ETuple dm) (De.list decode)
        "map" -> map (EMap dm) (De.list (tuple2 (,) decode decode))
        "list" -> map (EList dm) (De.list decode)
        r -> De.fail <| toString r

setIds term id =
  let
    (t',newId) = setIds' term id
  in t'

setIds' : ETerm -> Int -> (ETerm, Int)
setIds' t id =
  case t of
    ETuple m l ->
      let (l', id') = setIdsList l <| id+1
      in
      (ETuple {m | id = id} l', id')
    EMap m kv ->
      let (kv', id') = setIdsMap kv <| id+1
      in
      (EMap {m | id = id} kv', id')
    EList m l ->
      let (l', id') = setIdsList l <| id+1
      in
      (EList {m | id = id} l', id')
    EBasic m p ->
      (EBasic {m | id = id} p, id+1)

setIdsMap : List (ETerm, ETerm) -> Int -> (List (ETerm, ETerm), Int)
setIdsMap kv id1 =
        case kv of
          [] -> ([], id1)
          ((k,v)::tl) ->
          let
            (k',id2) = setIds' k id1
            (v',id3) = setIds' v id2
            (ts', id4) = setIdsMap tl id3
       in ((k',v')::ts', id4)

setIdsList : List ETerm -> Int -> (List ETerm, Int)
setIdsList ts id1 =
  case ts of
     [] -> ([], id1)
     (hd::tl) ->
       let
         (t',id2) = setIds' hd id1
         (ts', id3) = setIdsList tl id2
       in (t'::ts', id3)

unfold : Int -> ETerm -> ETerm
unfold i t =
  if i >= 1 then
    case t of
      ETuple m r -> ETuple {m|fold = False} (List.map (unfold <| i-1) r)
      EMap m kv -> EMap {m|fold = False} (List.map (tuplemap <| unfold <| i-1) kv)
      EList m r -> EList {m|fold = False} (List.map (unfold <| i) r) -- Hack for proplists should be i-1
      _ -> t
  else t
