module Main exposing (..)

import Browser
import Html exposing (div, text)
import Html.Attributes exposing (style)
import String exposing (fromChar)
import Browser.Events exposing (onKeyPress)
import Json.Decode as Decode
import Html exposing (button)
import Html.Events exposing (onClick)


type Word = Word (List Char)
-- Use this for word https://github.com/Orasund/elm-static-array
type alias Model =
    { gueses : List Word
    , gues : List Char
    , word : Word
    }
type Msg = Gues | Key Char | DelChar | Won | Nil

init : () -> (Model, Cmd Msg)
init _ = (Model [] [] <| Word ['t', 'e', 's', 't', 'i'], Cmd.none)

main = Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }


subscriptions : Model -> Sub Msg
subscriptions _ = Decode.field "key" Decode.string |> Decode.map keyToMsg |> onKeyPress

keyToMsg : String -> Msg
keyToMsg string = case String.uncons string of
    Just (char, "") -> Key char
    _ -> case string of
       "Enter" -> Gues
       _ -> Nil


validGues l = List.length l == 5


update msg model =
    (case msg of
        Gues -> if (List.reverse model.gues |> Word) == model.word
            then update Won model |> Tuple.first
            else if validGues model.gues
            then { model | gueses = Word (List.reverse model.gues) :: model.gueses, gues = [] }
            else model
        Key  k -> { model | gues = if List.length model.gues < 5 then k :: model.gues else model.gues }
        DelChar -> case model.gues of
           h :: t -> { model | gues = t }
           [] -> model
        Won -> init () |> Tuple.first 
        Nil -> model
    , Cmd.none)

toRow (Word final) (Word gues) = List.map2
    (\y -> \x ->
        if x==y then cell "green" (Just x)
        else if List.member x final then cell "yellow" (Just x)
        else cell "gray" (Just x)
    ) final gues |> row

cell color letter = div
    [ style "color" color -- could be background-color (as in original)

    , style "display" "inline-flex"
    , style "justify-content" "center"
    , style "align-items" "center"

    , style "font-size"   "2rem"
    , style "line-height" "2rem"
    , style "font-weight" "bold"
    , style "text-transform" "uppercase"

    , style "border" "2px solid black"
    
    , style "width" "100%"
    , style "height" "100%"
    ] <| case letter of
        Nothing -> []
        Just l -> [ text (l |> fromChar) ]

row cells = div
    [ style "display" "flex"
    , style "flex-wrap" "nowrap"
    , style "flex-direction" "row"
    , style "justify-content" "space-between"
    , style "grid-gap" "5px"

    , style "height" "100%"
    ] cells

rowFromGues =  List.reverse >> List.map Just >> fill 5 Nothing >> List.map (cell "gray") >> row
rowDefault = rowFromGues []

fill n empty l = l ++ List.repeat (n - List.length l) empty

table model = fill 6 rowDefault <| List.reverse <| rowFromGues model.gues :: List.map (toRow model.word) model.gueses


view model =
  div
    [ style "display" "flex"
    , style "flex-direction" "column"
    , style "flex-wrap" "nowrap"
    , style "grid-gap" "5px"

    , style "width" "350px"
    , style "height" "420px"
    ] (table model ++ [ button [ onClick DelChar] [ text "Delete Char"] ])

