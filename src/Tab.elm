   
module Tab exposing(Tab, InkBarDetails, renderTabs, renderTabNavigation)

import Html exposing (Html, Attribute, div, text)
import Html.Attributes exposing (class, style, classList)
import Html.Events exposing (on)
import Json.Decode exposing (Decoder, field, map, map2, float)

type alias Tab = 
  { label : String
  , id : String
  }

type alias InkBarDetails =
  { left: Float
  , width: Float
  }

renderTabNavigation : List Tab -> InkBarDetails -> String -> (Tab -> InkBarDetails -> msg) -> List (Html msg)
renderTabNavigation tabs inkBarDetails selected onTabSelected =
  (List.map (\n -> renderTabLabel n (selected == n.id) onTabSelected) tabs) ++ [ renderInkBar inkBarDetails ]

renderInkBar : InkBarDetails -> Html msg
renderInkBar elemInfo =
  div 
    [ class "inkBar"
    , style "left" (floatToPixel elemInfo.left)
    , style "width" (floatToPixel elemInfo.width) 
    ] [ ]

floatToPixel : Float -> String
floatToPixel data =
  (String.fromFloat data) ++ "px"

renderTabLabel : Tab -> Bool -> (Tab -> (InkBarDetails) -> msg) -> Html msg
renderTabLabel tab isSelected onTabSelected =
  div 
    [ classList 
      [ ("tabLabel", True)
      , ("active", isSelected) 
      ] 
    , onClickCustom (onTabSelected tab)
    ]
    [ text tab.label ]
          
onClickCustom : (InkBarDetails -> msg) -> Attribute msg
onClickCustom tagger = 
  on "click" (field "target" (map tagger decodeInkBarDetails))

decodeInkBarDetails : Decoder InkBarDetails
decodeInkBarDetails =
  map2 InkBarDetails
    (field "offsetLeft" float)
    (field "offsetWidth" float)

renderTabs : List (String, List (Html msg)) -> String -> List (Html msg)
renderTabs tabList selectedTab =
  List.map (\n -> renderTab (selectedTab == (Tuple.first n)) (Tuple.second n)) tabList

renderTab : Bool -> List (Html msg) -> Html msg
renderTab isSelected tab = 
  if isSelected then
    div [ class "tabContent", style "display" "flex" ] tab
  else
    div [ class "tabContent" ] tab