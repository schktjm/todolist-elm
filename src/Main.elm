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
    { todos: List Todo, text: String }

init : Model
init = Model  
    [] "" 


-- UPDATE

type Msg = Change String | Add | Delete Int | Print String

update : Msg -> Model -> Model
update msg model =
    case msg of
        Change val ->
            { model | text = val }
        Add  ->
            { model | todos = (Todo model.text)::model.todos, text = "" }
        Delete idx ->
            {model | todos = 
                model.todos
                    |> List.indexedMap Tuple.pair
                    |> List.filter (\(i, _) -> i /= idx)
                    |> List.map Tuple.second
            }
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
        [ input [ placeholder "input text", value model.text, onInput Change ][]
        , button [ onClick Add ][text "add"]
        ]
    , div[]
        [
            ul[](List.indexedMap viewTodo  model.todos )
        ]
    ]


viewTodo : Int -> Todo -> Html Msg
viewTodo num todo =
    li[]
    [ text todo.text
    , button [ onClick (Delete num) ][text "delete"] ]
    