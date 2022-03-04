module Main exposing (..)

import Browser
import Html exposing (div, text)
import Html.Attributes exposing (style)
import String exposing (fromChar)
import Json.Decode as Decode
import Html exposing (button)
import Html.Events exposing (onClick)
import List exposing (foldl, foldr)
import Process
import Task
import Words
import Random
import Array
import Browser.Events exposing (onKeyDown)

-- TODO make virtual keyboard?
-- TODO animations like this? https://dev.to/lucamug/elm-beginners-tutorial-how-to-make-animated-snackbars-with-zero-css-12g1
-- TODO write tests
-- using this for colors https://www.tedmontgomery.com/tutorial/clrctgBL.html
type Word = Word (List Char)

-- Use this for word https://github.com/Orasund/elm-static-array
type alias State =  
    { gueses : List Word
    , gues : List Char
    , word : Word
    }
type Model = Loading Status | Game Status State
type Status = ShortInput | NotInWordList | Ok | YouWon | YouLose
type Msg = Gues | Key Char | DelChar | Won | Lose | ResetStatus | NewGame Word | Nil
init : Status -> () -> (Model, Cmd Msg)
init status _ = (Loading status, Random.generate (\ind -> Array.get ind Words.commonWords |> Maybe.withDefault "debug" |> String.toList |> Word |> NewGame) <| Random.int 0 <| Array.length Words.commonWords)

main : Program () Model Msg
main = Browser.element
    { init = init Ok
    , update = update
    , view = view
    , subscriptions = subscriptions
    }


subscriptions : Model -> Sub Msg
subscriptions _ = Decode.field "key" Decode.string |> Decode.map keyToMsg |> onKeyDown

keyToMsg : String -> Msg
keyToMsg string = case String.uncons string of
    Just (char, "") -> Key char
    _ -> case string of
       "Enter" -> Gues
       "Backspace" -> DelChar
       _ -> Nil


validGues : List Char -> Bool
validGues l = List.member (List.reverse l |> String.fromList |> String.toLower) Words.allWords

delay : Float -> b -> Cmd b
delay time msg = Process.sleep time |> Task.perform (\_ -> msg)
resetStatus : Cmd Msg
resetStatus = delay 5000 ResetStatus

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case model of
    Loading status -> case msg of
       NewGame w -> (Game status <| State [] [] w, Cmd.none)
       _ -> Debug.todo "Stagod"
    Game status state -> case msg of
        Gues -> if (List.reverse state.gues |> Word) == state.word then update Won <| Game status state
            else if List.length state.gues < 5 then (Game ShortInput state, resetStatus)
            else if not <| validGues state.gues then (Game NotInWordList state, resetStatus)
            else if List.length state.gueses == 5 then update Lose <| Game status state
            else (Game status { state | gueses = Word (List.reverse state.gues) :: state.gueses, gues = [] }, Cmd.none)
        Key  k -> (Game status { state | gues = if List.length state.gues < 5 then k :: state.gues else state.gues }, Cmd.none)
        DelChar -> case state.gues of
           _ :: t -> (Game status { state | gues = t }, Cmd.none)
           [] -> (Game status state, Cmd.none)
        Won -> let newGame = init YouWon ()  in  (newGame |> Tuple.first, Cmd.batch [newGame |> Tuple.second, resetStatus])
        Lose -> let newGame = init YouLose () in (newGame |> Tuple.first, Cmd.batch [newGame |> Tuple.second, resetStatus])
        ResetStatus -> (Game Ok state, Cmd.none)
        Nil -> (model, Cmd.none)
        _ -> Debug.todo "Stagod"


zip : List a -> List b -> List (a, b)
zip = List.map2 Tuple.pair

type Color = X | Y | G
toHtmlColor : Color -> String
toHtmlColor clr = case clr of 
    X -> "gray"
    Y -> "yellow"
    G -> "green"

toColors : List b -> List b -> List Color
toColors ans gues = 
    let
        greens = List.map2 (==) ans gues
        newMask ch list mask = zip mask list |> foldr
            (\(masked, cur) -> \(changed, acc) -> 
                if not changed && not masked && cur == ch 
                then (True, True :: acc) 
                else (changed, masked :: acc)) (False, []
            )
        toColor x y mask = if x==y then G
            else if newMask x ans mask |> Tuple.first then Y
            else X
    in
        zip gues ans |> foldl (\(x, y) -> \(mask, sol) -> 
            (newMask x ans mask |> Tuple.second, toColor x y mask :: sol)
        ) (greens, []) |> Tuple.second |> List.reverse

toRow : Word -> Word -> Html.Html msg
toRow (Word ans) (Word gues) =
    List.map2 (\col -> cell (toHtmlColor col) << Just) (toColors ans gues) gues |> row

cell : String -> Maybe Char -> Html.Html msg
cell color letter = div
    [ style "background-color" color -- could be background-color (as in original)

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

row : List (Html.Html msg) -> Html.Html msg
row cells = div
    [ style "display" "flex"
    , style "flex-wrap" "nowrap"
    , style "flex-direction" "row"
    , style "justify-content" "space-between"
    , style "grid-gap" "5px"

    , style "height" "100%"
    ] cells

bgColor : String
bgColor = "#202020"
rowFromGues : List Char -> Html.Html msg
rowFromGues =  List.reverse >> List.map Just >> fill 5 Nothing >> List.map (cell bgColor) >> row

rowDefault : Html.Html msg
rowDefault = rowFromGues []

fill : Int -> b -> List b -> List b
fill n empty l = l ++ List.repeat (n - List.length l) empty

tableRows : { a | gues : List Char, word : Word, gueses : List Word } -> List (Html.Html msg)
tableRows state = fill 6 rowDefault <| List.reverse <| rowFromGues state.gues :: List.map (toRow state.word) state.gueses

table : { a | gues : List Char, word : Word, gueses : List Word } -> Html.Html Msg
table state = div
    [ style "display" "flex"
    , style "flex-direction" "column"
    , style "flex-wrap" "nowrap"
    , style "grid-gap" "5px"

    , style "width" "350px"
    , style "height" "420px"
    ] (tableRows state ++ [ button [ onClick DelChar] [ text "Delete Char"] ])

view : Model -> Html.Html Msg
view model = div
    [ style "display" "flex"
    , style "justify-content" "center"
    , style "flex-direction" "row"

    , style "width" "100%"
    , style "height" "100%"
    , style "position" "fixed"

    , style "background-color" bgColor 
    ]
    [ div 
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "flex-direction" "column"
        ]
        [ statusDiv model
        , case model of
            Loading _ -> text "Loading...."
            Game _ state -> table state
        , div -- eventualy, will be used for keyboard
            [ style "height" "300px" ]
            []
        ]
    ]

toString : Word -> String
toString (Word l) = String.fromList l

statusDiv : Model -> Html.Html msg
statusDiv model =
    let status = case model of
            Loading stat -> stat
            Game stat _ -> stat
        ans = case model of 
            Loading _ -> "DEBUG"
            Game _ { word } -> toString word
    in case status of
        Ok -> div [ style "height" "100px" ] []
        ShortInput -> div [ style "height" "100px", style "color" "red" ] [ text "Prekratka ti je rec!"] 
        NotInWordList -> div [ style "height" "100px", style "color" "red" ] [ text "Ne znam za takvu rec!"] 
        YouWon -> div [ style "height" "100px", style "color" "green" ] [ text "Bravo! Pobedio si :D" ]
        YouLose -> div [ style "height" "100px", style "color" "green" ] [ text ("Rec je bila " ++ ans ++ " Probaj ponovo :)") ]

-- rec podne  i pomoc
