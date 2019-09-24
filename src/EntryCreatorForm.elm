module EntryCreatorForm exposing (Form, FormField(..), renderForm, emptyForm, validateForm, updateForm)

import Time
import Date exposing (fromIsoString)
import Debug
import TodoList exposing (TodoEntry)
import Html exposing (Html, form, div, label, input, span, text, textarea, button)
import Html.Attributes exposing (class, style, type_, name, checked, placeholder, required, for)
import Html.Events exposing (preventDefaultOn, onInput)
import Json.Decode exposing (succeed)
import Global exposing (Updater)
import ISO8601
import Maybe.Extra exposing (isNothing)

type FormField = Description | Lead | FormDate | FormTime 

type alias Form = 
  { description: String
  , lead: String
  , date: Maybe (Date.Date)
  , time: Maybe (Time.Posix)
  }

renderForm : Form -> (Form -> msg) -> (TodoEntry -> msg) -> msg -> Html msg
renderForm formData onUpdate onCreate onCancel = 
  form
    ( case validateForm formData of 
        Nothing -> []
        Just entry -> [ (onCreate entry, True) |> succeed |> preventDefaultOn "submit" ]
    )
    [ navBar
    , div
        [ class "createEntryBody" ]
        [ div 
            [ class "inputGroup", class "inputGroupColumn" ]
            [ label [] [ text "Title" ]
            , textarea 
                [ name "title"
                , class "textarea"
                , placeholder "Start typing..."
                , style "font-family" "Roboto"
                , style "padding" "10px"
                , required True
                , (\n -> updateForm Description n formData |> onUpdate) |> onInput
                ] []
            ]

        , div 
            [ class "inputGroup", class "inputGroupColumn" ]
            [ label [ for "lead" ] [ text "Title" ]
            , input 
                [ name "lead"
                , class "textarea"
                , required True
                , (\n -> updateForm Lead n formData |> onUpdate) |> onInput
                ] []
            ]
        , div 
            [ class "inputGroup" ]
            [ text "Due on " 
            , input 
                [ name "date"
                , class "standardInput"
                , type_ "date"
                , required True
                , (\n -> updateForm FormDate n formData |> onUpdate) |> onInput
                ] []
            , text " "
            , input 
                [ name "time"
                , class "standardInput"
                , type_ "time"
                , required True 
                , (\n -> updateForm FormTime n formData |> onUpdate) |> onInput
                ] []
            ]
        ]

    , div
        [ class "createEntryActions" ]
        [ button 
            [ name "cancel"
            , class "standardButton"
            , style "background-color" "red"
            , style "margin-right" "10px"
            , (onCancel, True) |> succeed |> preventDefaultOn "click"
            ] [ text "Cancel" ]
        , button
            [ type_ "submit"
            , class "standardButton"
            ] [ text "Create"]
        ]
    ]

emptyForm : Form
emptyForm = Form "" "" Nothing Nothing

{-| Time will only get filled is the date is filled -}
updateForm : FormField -> String -> Updater Form
updateForm field value form = 
  case field of
    Description -> { form | description = value }
    Lead -> { form | lead = value }
    FormDate -> { form | date = fromIsoString value |> Result.toMaybe }
    FormTime -> case form.date of 
      Nothing -> { form | time = Nothing }
      Just date -> 
        { form 
        | time = case Date.toIsoString date ++ "T" ++ value  |> ISO8601.fromString of
            Err _ -> Nothing
            Ok time -> Just <| ISO8601.toPosix time
        }
        
validateForm : Form -> Maybe TodoEntry
validateForm form =
  if List.member True <| List.map String.isEmpty [ form.description , form.lead ] then
    Nothing
  else
    case form.date of
      Nothing -> Nothing
      Just date -> case form.time of
        Nothing -> Nothing
        Just time -> Just <| TodoEntry form.description form.lead date time [] False False

navBar : Html msg
navBar = 
  div 
    [ class "createEntryNav" ]
    [ label
        [ class "radioButtonContainer" ]
        [ input [ type_ "radio", name "entryType", checked True ] []
        , div [ class "radioButtonCheckMark" ] [ div [ class "radioButtonMark" ] []]
        , span [ class "radioButtonLabel" ] [ text "Task" ]
        ]
    , label
        [ class "radioButtonContainer" ]
        [ input [ type_ "radio", name "entryType" ] []
        , div [ class "radioButtonCheckMark" ] [ div [ class "radioButtonMark" ] []]
        , span [ class "radioButtonLabel" ] [ text "Meeting" ]
        ]
    ]
  