module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

-- MAIN
main =  Browser.sandbox {init = init,update = update, view = view} 

-- MODEL


type alias Todo =
    {text: String}

type alias Model = 
    { todos: List Todo, text: String, updateText: String, isFixing: Bool }

init : Model
init = Model  
    [] "" "" False


-- UPDATE

type Msg = ChangeText String | ChangeUpdateText String| Add | Delete Int | Update | Print String

update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeText val ->
            { model | text = val }
        ChangeUpdateText str ->
            { model | updateText = str}
        Add  ->
            { model | todos = (Todo model.text)::model.todos, text = "" }
        Delete idx ->
            {model | todos = 
                model.todos
                    |> List.indexedMap Tuple.pair
                    |> List.filter (\(i, _) -> i /= idx)
                    |> List.map Tuple.second
            }
        Update -> 
            {model | isFixing = True}
        Print str ->
            let
                _ = Debug.log "print" str
            in
                model
        

-- VIEW

view : Model -> Html Msg
view model =
    div[]
    [ div[]
        [ input [ placeholder "input text", value model.text, onInput ChangeText ][]
        , button [ onClick Add ][text "add"]
        ]
    , div[]
        [
            ul[](List.indexedMap viewTodo  model.todos )
        ]
    , viewUpdateArea model
    ]


viewTodo : Int -> Todo -> Html Msg
viewTodo num todo =
    li[]
    [ text todo.text
    , button [ onClick Update ][text "fix"]
    , button [ onClick (Delete num) ][text "delete"] ]
    
viewUpdateArea : Model -> Html Msg
viewUpdateArea model =
    if model.isFixing then
        div[]
        [ input [ placeholder "update text", value model.updateText, onInput ChangeUpdateText][]
        , button [][text "update"]
        ]
    else
        div[][ text "not fixing"]

