module TodoList.Decode exposing (decodeTodoList)

import Json.Decode exposing (Decoder, decodeString, list, succeed, string, map, bool)
import Json.Decode.Pipeline exposing (required, optional)
import TodoList exposing (TodoEntry, Tag)
import Date
import Time
import ISO8601

type alias TempTodoEntry = 
  { description: String
  , lead: String
  , date: Maybe Date.Date
  , dateTime: Maybe Time.Posix
  , tags: List Tag
  , closed: Bool
  }

decodeTodoList : String -> List TodoEntry
decodeTodoList json = 
  filterTempTodoEntry (loadTodoList json)
  
loadTodoList : String -> Result Json.Decode.Error (List (Maybe TempTodoEntry))
loadTodoList todoListJson2 = 
  decodeString (list todoEntryDecoder) todoListJson2
  
todoEntryDecoder : Decoder (Maybe TempTodoEntry)
todoEntryDecoder = 
  Json.Decode.maybe (succeed TempTodoEntry
    |> required "description" string
    |> required "lead" string
    |> optional "dateTime" (map dateResultToMaybe (map stringToDate string)) Nothing
    |> optional "dateTime" (map stringToPoxiv string) Nothing
    |> required "tags" (list tagDecoder)
    |> required "closed" bool)

stringToDate : String -> Result String Date.Date
stringToDate str =
  case List.head (String.split "T" str) of
    Just head -> Date.fromIsoString head
    Nothing -> Err "" 

dateResultToMaybe : Result a b -> Maybe b
dateResultToMaybe result =
  case result of
    Ok data -> Just data
    Err _ -> Nothing

tagDecoder : Decoder Tag
tagDecoder =
  succeed Tag
    |> required "text" string
    |> required "color" string

stringToPoxiv : String -> Maybe Time.Posix
stringToPoxiv date = 
  case ISO8601.fromString date of
    Ok time ->
      Just (ISO8601.toPosix time)
    Err result->
      Nothing

filterTempTodoEntry: Result Json.Decode.Error (List (Maybe TempTodoEntry)) -> List TodoEntry
filterTempTodoEntry data =
  case data of
    Err _ -> []
    Ok list ->
      List.filterMap
        (\n2 ->
          case n2 of
            Nothing -> Nothing
            Just n -> 
              case n.dateTime of
                Nothing -> Nothing
                Just datetime ->
                  case n.date of
                    Nothing -> Nothing
                    Just date ->
                      Just (TodoEntry n.description n.lead date datetime n.tags n.closed n.closed)
        ) list