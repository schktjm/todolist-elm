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
    { todos: List Todo, text: String, updateIdx: Int, updateText: String, isFixing: Bool }

init : Model
init = Model  
    [] "" 0 "" False


-- UPDATE

type Msg = ChangeText String | ChangeUpdateText String | UpdateNthItem | Add | Delete Int | Print String | ChangeFixFlg | ChangeUpdateIdx Int

update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeText val ->
            { model | text = val }
        ChangeUpdateText str ->
            { model | updateText = str}
        UpdateNthItem ->
            {model | isFixing = False, updateText = "", todos =
                model.todos
                    |> List.indexedMap Tuple.pair
                    |> Debug.log "before"
                    |> List.map (\(i, x) -> 
                        if model.updateIdx == i then
                            Todo model.updateText 
                        else
                            x
                        )
                    |> Debug.log "after"
            }
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
        ChangeFixFlg ->
            {model | isFixing = not model.isFixing}
        ChangeUpdateIdx num -> 
            {model | updateIdx = num, isFixing = True}

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
    , button [ onClick (ChangeUpdateIdx num) ][text "fix"]
    , button [ onClick (Delete num) ][text "delete"] ]
    
viewUpdateArea : Model -> Html Msg
viewUpdateArea model =
    if model.isFixing then
        div[]
        [ input [ placeholder "update text", value model.updateText, onInput ChangeUpdateText][]
        , button [onClick UpdateNthItem ][text "update"]
        , button [ onClick ChangeFixFlg][text "x"]
        ]
    else
        div[][ text "not fixing"]
