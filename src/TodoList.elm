module TodoList exposing (TodoEntry, Tag, TodoDay, partitionTodoDay, renderTodoListPartition, addEntry)

import Date
import Time
import Html exposing (Html, div, text, button, label, input, span)
import Html.Attributes exposing (class, type_, autocomplete, checked, style)
import Html.Events exposing (onClick, on)
import Json.Decode exposing (Decoder, map, field, string, bool)
import Global exposing (Updater)
import Tuple2

type alias TodoDay = 
  { date: Date.Date
  , list: List TodoEntry
  , showClosed: Bool
  }

type alias TodoEntry = 
  { description: String
  , lead: String
  , date: Date.Date
  , dateTime: Time.Posix
  , tags: List Tag
  , closed: Bool
  , closedCheckBox: Bool
  }

type alias Tag =
  { text: String
  , color: String
  }

type alias OnTempStatusChange msg = TodoDay -> Int -> Bool -> msg
type alias OnStatusChange msg = TodoDay -> Int -> String -> msg

addEntry : TodoEntry -> Updater (List TodoDay)
addEntry entryToAdd todoDayList =
  if List.any (\n -> n.date == entryToAdd.date) todoDayList then
    todoDayList
      |> List.map (\n -> if n.date /= entryToAdd.date then n else { n | list = addEntryToListInOrder n.list entryToAdd }) 
  else
    List.sortWith compareTodoDay <| TodoDay entryToAdd.date [entryToAdd] False :: todoDayList

addEntryToListInOrder : List TodoEntry -> TodoEntry -> List TodoEntry
addEntryToListInOrder list todoEntry =
  todoEntry :: list |> List.sortWith compareTodoEntry

compareTodoEntry : TodoEntry -> TodoEntry -> Order
compareTodoEntry entry1 entry2 =
  (entry1, entry2)
    |> Tuple.mapBoth .dateTime .dateTime
    |> Tuple.mapBoth Time.posixToMillis Time.posixToMillis
    |> Tuple2.uncurry compare

compareTodoDay : TodoDay -> TodoDay -> Order
compareTodoDay todoDay1 todoDay2 =
  Date.compare todoDay1.date todoDay2.date

renderTodoListPartition : OnTempStatusChange msg -> OnStatusChange msg -> (TodoDay -> msg) -> TodoDay -> List(Html msg)
renderTodoListPartition onTempStatusChange onStatusChange onClosedSwitch todoDay =
  [ div [ class "todoDayHeader" ] 
      [ div [class "todoOpenedDayHeader"] [ text (renderTodoHeader todoDay) ]
      , renderTodoClosedText todoDay onClosedSwitch
      ]
  ] ++ List.map (renderTodoListEntryShortcut todoDay onTempStatusChange onStatusChange) (filterTodoDayEntriesToShow todoDay)

renderTodoListEntryShortcut : TodoDay -> OnTempStatusChange msg -> OnStatusChange msg -> (Int, TodoEntry) -> Html msg
renderTodoListEntryShortcut todoDay onTempStatusChange onStatusChange indexedTodoEntry = 
  renderTodoListEntry 
    todoDay 
    (Tuple.first indexedTodoEntry) 
    (Tuple.second indexedTodoEntry) 
    onTempStatusChange 
    onStatusChange

filterTodoDayEntriesToShow : TodoDay -> List(Int, TodoEntry)
filterTodoDayEntriesToShow todoDay =
  List.filter 
    (\indexedTodoEntry -> todoDay.showClosed == ( Tuple.second indexedTodoEntry ).closed) 
    (List.indexedMap Tuple.pair todoDay.list)

renderTodoHeader : TodoDay -> String
renderTodoHeader todoDay = 
  "Due " ++ dueOnText todoDay todoDayOpenedLength

todoDayClosedLength : TodoDay -> Int
todoDayClosedLength todoDay =
  List.filter .closed todoDay.list |> List.length

todoDayOpenedLength : TodoDay -> Int
todoDayOpenedLength todoDay =
  List.filter (\n -> not n.closed) todoDay.list |> List.length

dueOnText : TodoDay -> (TodoDay -> Int) -> String
dueOnText todoDay handler =
  "on " ++ Date.toIsoString todoDay.date ++ " (" ++ (handler todoDay |> String.fromInt) ++ ")"

renderTodoClosedText : TodoDay -> (TodoDay -> msg) -> Html msg
renderTodoClosedText todoDay onClosedSwitch=
  button 
    [ class "todoClosedDayHeader", onClick (onClosedSwitch todoDay)] 
    [ "Closed " ++ dueOnText todoDay todoDayClosedLength |> text ]

renderTodoListEntry : TodoDay -> Int -> TodoEntry -> OnTempStatusChange msg -> OnStatusChange msg -> Html msg
renderTodoListEntry todoDay index todoEntry onTempStatusChange onStatusChange =
  div [ class "todoEntry" ]
    [ label [ class "container" ]
        [ input 
            [ type_ "checkbox", autocomplete False
            , checked todoEntry.closedCheckBox
            , onCheckboxChange todoDay index onTempStatusChange |> on "change"
            ] 
            []
        , div 
           [ class "checkmark"
           , transitionEndOpacityDecoder todoDay index onStatusChange |> on "transitionend"
           ] 
           [ div [ class "mark" ] [] ]
        ]
    , div [ class "todoData" ]
        [ div [ ] [ span [] [ text todoEntry.description ] ] 
        , div [ class "metaData" ]
          [ div [ class "publishingData" ]
              [ span [] [ text todoEntry.lead ] 
              , div (List.map class ["circle grayCircle"]) []
              , span [] [ formatTodoTime todoEntry.dateTime |> text ]
              ]
          , List.map renderTag todoEntry.tags |> div [ class "chips" ]
          ]
        ]
    ]

formatTodoTime : Time.Posix -> String
formatTodoTime time =
  ( formatIntTo2DigitString (Time.toHour Time.utc time) 
  ++ ":" 
  ++ formatIntTo2DigitString (Time.toMinute Time.utc time)
  )

formatIntTo2DigitString : Int -> String
formatIntTo2DigitString number =
  if number >= 10 then
    String.fromInt number
  else
    "0" ++ String.fromInt number

onCheckboxChange : TodoDay -> Int -> OnTempStatusChange msg -> Decoder msg
onCheckboxChange todoDay index onTempStatusChange=
  map (\n -> onTempStatusChange todoDay index n) (field "target" (field "checked" bool))

transitionEndOpacityDecoder : TodoDay -> Int -> OnStatusChange msg -> Decoder msg
transitionEndOpacityDecoder todoDay index onStatusChange =
  map (\n -> onStatusChange todoDay index n) (field "propertyName" string)

renderTag : Tag -> Html msg
renderTag tag =
  span [ class "chip", style "background-color" tag.color ] [ text tag.text ]

partitionTodoDay : List TodoEntry -> List TodoDay
partitionTodoDay todoList =
  partition [] todoList

partition : List TodoDay -> List TodoEntry -> List TodoDay
partition todoDays todoList =
  case getLast todoDays of
    Nothing ->
      let
        head = List.head todoList
      in
        case head of
          Nothing -> []
          Just todoEntry -> partition [ TodoDay todoEntry.date [ todoEntry ] False ] (tailOrEmpty todoList)
    Just day -> 
      let
        head = List.head todoList
      in
        case head of
          Nothing -> todoDays
          Just todoEntry ->
            if todoEntry.date /= day.date then
              partition (todoDays ++ [ TodoDay todoEntry.date [ todoEntry ] False ]) (tailOrEmpty todoList)
            else 
              partition 
                (List.take (List.length todoDays - 1) todoDays ++ [ { day | list = day.list ++ [ todoEntry ]} ]) 
                (tailOrEmpty todoList)

sortTodoEntries : List TodoEntry -> List TodoEntry
sortTodoEntries todoEntries =
  List.sortBy (\n -> Time.posixToMillis n.dateTime) todoEntries

getLast : List a -> Maybe a
getLast list =
  List.reverse list |> List.head 

tailOrEmpty : List a -> List a
tailOrEmpty list =
  case List.tail list of
    Nothing -> []
    Just x -> x