module TestEntryCreatorForm exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import EntryCreatorForm exposing (Form, FormField(..), emptyForm, validateForm, updateForm)
import Date exposing (Date, fromOrdinalDate, fromCalendarDate, numberToMonth)
import Time exposing (millisToPosix)
import ISO8601

suite : Test
suite =
  describe "TodoList"
    [ describe "validateForm"
        [ test "Empty form should always be invalid" <| \_ ->
            validateForm emptyForm
              |> Expect.equal Nothing

        , test "Filled form should be valid" <| \_ ->
            let
              date = fromOrdinalDate 2019 303
              time = millisToPosix 1
            in
            Form "title" "lead" (Just date) (Just time)
              |> validateForm
              |> \result -> case result of
                Nothing -> Expect.fail "Valid form should return a TodoEntry"
                Just value -> value
                  |> Expect.all
                    [ \n -> n.description |> Expect.equal "title"
                    , \n -> n.lead |> Expect.equal "lead"
                    , \n -> n.date |> Expect.equal date
                    , \n -> n.dateTime |> Expect.equal time
                    ]

        , test "Semi Filled form should be invalid" <| \_ ->
            let
              date = fromOrdinalDate 2019 303
              time = millisToPosix 1
            in
            Form "title" "" (Just date) (Just time)
              |> validateForm
              |> \result -> Expect.equal Nothing result
        ]

    , describe "updateForm"
        [ test "Should fill date" <| \_ ->
            let
              date = "2019-08-09"
            in
            updateForm FormDate date emptyForm
              |> .date
              |> Expect.equal (Just (fromCalendarDate 2019 (numberToMonth 8) 9))
        ]
    ]

