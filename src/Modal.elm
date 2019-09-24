module Modal exposing (modal, modalContent)

import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import Html.Events exposing (on)
import Json.Decode exposing (Decoder, succeed, fail, field, int, andThen)

modal : Bool -> List (Html msg) -> msg -> Html msg
modal opened content onClose =
  div 
    [ class "modal"
    , style "display" <| if opened then "flex" else "none"
    , on "click" <| getTargetClassName onClose
    ] content

getTargetClassName : msg -> Decoder msg
getTargetClassName msg =
  field "eventPhase" int
    |> andThen (\phase -> if phase == 2 then succeed msg else fail "not modal")

modalContent : List (Html msg) -> Html msg
modalContent content = 
  div [ class "modalContent" ] content
