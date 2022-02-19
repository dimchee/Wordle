module Main exposing (..)

import Browser
import Html exposing (div, text)
import Html.Attributes exposing (style)
import String exposing (fromChar)

type Word = Word (List Char)
-- Use this for input https://pianomanfrazier.com/post/elm-calculator-book/08-keypad-input/
-- Use this for word https://github.com/Orasund/elm-static-array

type alias State =
    { gueses : List Word
    , word : Word
    }


-- toColors : State -> List Color
-- toColors

main = Browser.sandbox
    { init = State [] <| Word ['t', 'e', 's', 't', 'i']
    , update = update, view = view
    }

type Msg = Gues Word

update msg model =
  case msg of
      Gues x -> { model | gueses = x :: model.gueses }

toRow (Word a) (Word b) = List.map2
    (\x -> \y ->
        if x==y then cell "green" (Just x)
        else if List.member x b then cell "yellow" (Just x)
        else cell "gray" (Just x)
    ) a b |> row

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

rowDefault = row 
    [ cell "blue" Nothing
    , cell "blue" Nothing
    , cell "blue" Nothing
    , cell "blue" Nothing
    , cell "blue" Nothing
    ]

view model =
  div
    [ style "display" "flex"
    , style "flex-direction" "column"
    , style "flex-wrap" "nowrap"
    , style "grid-gap" "5px"

    , style "width" "350px"
    , style "height" "420px"
    ]
    [ rowDefault
    , toRow (Word ['t', 'e', 's', 't', 'i']) (Word ['t', 'a', 's', 'e', 'o'])
    , rowDefault
    , rowDefault
    , rowDefault
    , rowDefault
    ]
