module TestTodoList exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import TodoList exposing (TodoEntry, TodoDay, addEntry)
import Date exposing (Date, fromOrdinalDate)
import Time exposing (millisToPosix)
import List.Extra exposing (findIndex, elemIndex)

suite : Test
suite =
  describe "TodoList"
    [ describe "addEntry"
        [ test "Adds a new day" <|
            \_ -> 
              let
                newDate = fromOrdinalDate 2019 303
                newTodoEntry = basicTodoEntry newDate
              in
              addEntry
                newTodoEntry
                [ TodoDay 
                    ( fromOrdinalDate 2019 300 )
                    [ basicTodoEntry (fromOrdinalDate 2019 300) ]
                    True
                , TodoDay 
                    ( fromOrdinalDate 2019 301 )
                    [ basicTodoEntry (fromOrdinalDate 2019 301) ]
                    True
                ]
                |> Expect.all
                    [ \result -> List.length result |> Expect.equal 3
                    , \result -> case List.head <| List.reverse result of
                        Nothing -> Expect.fail "List shouldn't be empty"
                        Just value -> Expect.equal value.date newDate
                    ]
        , test "The day is added opened" <|
            \_ -> 
              let
                newDate = fromOrdinalDate 2019 303
                newTodoEntry = basicTodoEntry newDate
              in
              addEntry
                newTodoEntry
                [ ]
                |> Expect.all
                    [ \result -> List.length result |> Expect.equal 1
                    , \result -> case List.head <| List.reverse result of
                        Nothing -> Expect.fail "List shouldn't be empty"
                        Just value -> Expect.equal False value.showClosed
                    ]

        , test "Doesn't add new day" <|
            \_ ->
              let
                newDate = fromOrdinalDate 2019 300
              in
              addEntry
                ( basicTodoEntry newDate )
                [ TodoDay 
                    ( fromOrdinalDate 2019 300 )
                    [ basicTodoEntry (fromOrdinalDate 2019 300) ]
                    True
                , TodoDay 
                    ( fromOrdinalDate 2019 301 )
                    [ basicTodoEntry (fromOrdinalDate 2019 301) ]
                    True
                ]
                |> Expect.all
                    [ \result -> List.length result |> Expect.equal 2
                    , \result -> case List.head result of
                        Nothing -> Expect.fail "List shouldn't be empty"
                        Just value -> Expect.equal 2 <| List.length value.list
                    ]

        , test "TodoDay is added in the proper order" <|
            \_ ->
              let
                newDate = fromOrdinalDate 2019 301
              in
              addEntry
                ( basicTodoEntry newDate )
                [ TodoDay 
                    ( fromOrdinalDate 2019 300 )
                    [ basicTodoEntry (fromOrdinalDate 2019 300) ]
                    True
                , TodoDay 
                    ( fromOrdinalDate 2019 302 )
                    [ basicTodoEntry (fromOrdinalDate 2019 302) ]
                    True
                ]
                |> Expect.all
                    [ \result -> List.length result |> Expect.equal 3
                    , \result -> case findIndex (\n -> n.date == newDate ) result of
                        Nothing -> Expect.fail "The new TodoDay should be part of the list"
                        Just value -> Expect.equal 1 value
                    ]
        , test "Entry is added in the proper order" <|
            \_ -> 
              let
                newDate = fromOrdinalDate 2019 303
                newTodoEntry = timeredTodoEntry (millisToPosix 3) newDate
              in
              addEntry
                newTodoEntry
                [ TodoDay 
                    newDate
                    [ timeredTodoEntry (millisToPosix 1) newDate
                    , timeredTodoEntry (millisToPosix 2) newDate
                    , timeredTodoEntry (millisToPosix 5) newDate
                    , timeredTodoEntry (millisToPosix 10) newDate
                    ]
                    True
                ]
                |> Expect.all
                    [ \result -> List.length result |> Expect.equal 1
                    , \result -> case List.head result of
                        Nothing -> Expect.fail "Result should have 1 element"
                        Just todoDay -> List.length todoDay.list |> Expect.equal 5
                    , \result -> case List.head result of
                        Nothing -> Expect.fail "List shouldn't be empty"
                        Just value -> case elemIndex newTodoEntry value.list of
                          Nothing -> Expect.fail "The new entry should be part of the TodoDay"
                          Just index -> Expect.equal 2 index
                    ]
        ]
    ]

basicTodoEntry : Date -> TodoEntry
basicTodoEntry date =
  TodoEntry 
    "description" 
    "lead"
    date
    (millisToPosix 0)
    []
    True
    True

timeredTodoEntry : Time.Posix -> Date -> TodoEntry
timeredTodoEntry time date =
  TodoEntry 
    "description" 
    "lead"
    date
    time
    []
    True
    True
