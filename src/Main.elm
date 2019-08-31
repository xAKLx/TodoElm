import Browser
import Html exposing (Html, Attribute, div, text)
import Html.Attributes exposing (class, style, id)
import List
import VirtualDom exposing (Node, node)
import Tab exposing (Tab, InkBarDetails, renderTabs, renderTabNavigation)
import TodoList exposing (TodoDay, Tag, TodoEntry, partitionTodoDay, renderTodoListPartition)
import TodoList.Decode exposing (decodeTodoList)

main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }

-- MODEL

type alias Model = 
  { tabs : List Tab 
  , selectedTab : String
  , inkBarDetails: InkBarDetails
  , todoList: List TodoDay
  }

init : String -> (Model, Cmd Msg)
init flag =
  ( Model 
      [ Tab "Work" "work", Tab "Personal" "personal" ] 
      "work" 
      (InkBarDetails 0 0) 
      (decodeTodoList flag |> partitionTodoDay)
  , Cmd.none
  )

-- UPDATE

type Msg 
  = SelectTab Tab InkBarDetails
  | SwitchTodoDayClosed TodoDay
  | UpdateClosedStatus TodoDay Int String
  | SetTempStatus TodoDay Int Bool

type alias Updater a = a -> a

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SelectTab tab inkBarDetails ->
      ({ model | selectedTab = tab.id, inkBarDetails = inkBarDetails }, Cmd.none)
    SwitchTodoDayClosed todoDay ->
      ({ model | todoList = List.map (mapTodoDays todoDay) model.todoList}, Cmd.none)
    UpdateClosedStatus todoDayOrigin indexToUpdate property ->
      if property == "opacity" then
        ( { model 
          | todoList = updateTodoListSingleEntry todoDayOrigin indexToUpdate todoEntryClosedUpdater model.todoList
          }
        , Cmd.none
        )
      else
        (model, Cmd.none)
    SetTempStatus todoDayOrigin indexToUpdate status -> 
      ( { model 
        | todoList = updateTodoListSingleEntry todoDayOrigin indexToUpdate (todoEntryCheckBoxUpdater status) model.todoList
        }
      , Cmd.none
      )

todoEntryClosedUpdater : Updater TodoEntry
todoEntryClosedUpdater todoEntry = { todoEntry | closed = todoEntry.closedCheckBox }

todoEntryCheckBoxUpdater : Bool -> Updater TodoEntry
todoEntryCheckBoxUpdater value todoEntry = { todoEntry | closedCheckBox = value }

updateTodoListSingleEntry : TodoDay -> Int -> Updater TodoEntry -> Updater (List TodoDay)
updateTodoListSingleEntry todoDayOrigin indexToUpdate updater todoDays =
  List.map (\todoDay -> 
    if todoDay /= todoDayOrigin then todoDay
    else
      { todoDay 
      | list = List.indexedMap (\index -> \todoEntry -> 
          if index /= indexToUpdate then todoEntry
          else updater todoEntry
        ) todoDay.list 
      }
  ) todoDays 

mapTodoDays : TodoDay -> Updater TodoDay
mapTodoDays todoDayToModify curentTodoDay =
  if todoDayToModify == curentTodoDay then 
    { curentTodoDay | showClosed = not curentTodoDay.showClosed } 
  else curentTodoDay

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ div [ class "background" ] []
    , div [ class "content" ] 
      [ renderTabNavigation model.tabs model.inkBarDetails model.selectedTab SelectTab |> div [ class "navigation" ]
      , customScrollBar [ class "todoList", style "height" "100%" ] 
          ( renderTabs 
              [ ( "work"
                , [ div 
                    [ id "work", style "flex" "1" ] 
                    ( renderTodoList model.todoList )
                  ]
                )
              , ( "personal", [ div [] [ text "personal" ] ] )
              ] 
              model.selectedTab
          )
      ]
    ]

renderTodoList : List TodoDay -> List (Html Msg)
renderTodoList todoList = 
  List.map 
    (renderTodoListPartition SetTempStatus UpdateClosedStatus SwitchTodoDayClosed) 
    todoList
  |> List.concat

customScrollBar : List (Attribute msg) -> List (Node msg) -> Node msg
customScrollBar =
  node "custom-scrollbar"
