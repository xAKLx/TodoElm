import Browser
import Html exposing (Html, Attribute, div, text, span, button)
import Html.Attributes exposing (class, style, id)
import Html.Events exposing (onClick)
import List
import VirtualDom exposing (Node, node)
import Tab exposing (Tab, InkBarDetails, renderTabs, renderTabNavigation)
import TodoList exposing (TodoDay, Tag, TodoEntry, partitionTodoDay, renderTodoListPartition, addEntry)
import TodoList.Decode exposing (decodeTodoList)
import Modal exposing (modal)
import Global exposing (Updater)
import EntryCreatorForm exposing (Form, renderForm, emptyForm)

main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }

-- MODEL

type alias Model = 
  { tabs : List Tab 
  , selectedTab : String
  , inkBarDetails: InkBarDetails
  , todoList: List TodoDay
  , modalOpened: Bool
  , form: Form
  }

init : String -> (Model, Cmd Msg)
init flag =
  ( Model 
      [ Tab "Work" "work", Tab "Personal" "personal" ] 
      "work" 
      (InkBarDetails 0 0) 
      (decodeTodoList flag |> partitionTodoDay)
      False
      emptyForm
  , Cmd.none
  )

-- UPDATE

type Msg 
  = SelectTab Tab InkBarDetails
  | SwitchTodoDayClosed TodoDay
  | UpdateClosedStatus TodoDay Int String
  | SetTempStatus TodoDay Int Bool
  | CloseModal
  | OpenModal
  | AddEntry TodoEntry
  | OnFormCanceled
  | OnFormUpdated Form

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
    CloseModal -> ( { model | modalOpened = False }, Cmd.none )
    OpenModal -> ( { model | form = emptyForm, modalOpened = True }, Cmd.none )
    AddEntry entry -> 
      ( { model | todoList = addEntry entry model.todoList, modalOpened = False }, Cmd.none )
    OnFormCanceled ->
      ( { model | form = emptyForm, modalOpened = False }, Cmd.none )
    OnFormUpdated form ->
      ( { model | form = form }, Cmd.none )

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
    , div [ class "titleBar" ]
      [ span [ class "title" ] [ text "My Tasks"]
      , div [ class "tools" ]
        [ button 
          [ id "newTaskButton"
          , class "standardButton"
          , onClick OpenModal
          ] [ text "New Task" ] ]
      ]
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
    , modal 
        model.modalOpened
        [ div 
            [ class "modalContent" ] 
            [ renderForm model.form OnFormUpdated AddEntry OnFormCanceled ]
        ] 
        CloseModal
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
