module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (Html, h1, text)
import Html.Attributes as Attributes
import Json.Decode as D
import Keyboard.Event as KE exposing (KeyboardEvent, decodeKeyboardEvent)
import Keyboard.Key as KK


type alias Model =
    { chords : List Chord
    , count : Int
    , disabled : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( Model chords 0 False, Cmd.none )


type Msg
    = HandleKeyboardEvent KeyboardEvent


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleKeyboardEvent event ->
            case event.keyCode of
                KK.Spacebar ->
                    ( { model | count = model.count + 1 }, Cmd.none )

                KK.Right ->
                    ( { model | count = model.count + 1 }, Cmd.none )

                KK.Left ->
                    ( { model | count = model.count - 1 }, Cmd.none )

                KK.Backspace ->
                    ( { model | count = 0 }, Cmd.none )

                KK.D ->
                    ( { model | disabled = not model.disabled }, Cmd.none )

                -- ignore anything else
                _ ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    onKeyDown (D.map HandleKeyboardEvent decodeKeyboardEvent)


type alias Chord =
    { name : String
    , alt : String
    }

chords : List Chord
chords =
    [ Chord "C" "7"
    , Chord "C" "7"
    , Chord "C" "7"
    , Chord "C" "7"
    , Chord "F" "7"
    , Chord "F" "7"
    , Chord "C" "7"
    , Chord "C" "7"
    , Chord "G" "7"
    , Chord "F" "7"
    , Chord "C" "7"
    , Chord "C" "7"
    ]

viewChord : Int -> Bool -> Int -> Chord -> Html Msg
viewChord count disabled measureNum chord =
    Html.div
        [ Attributes.classList [ ("measure", True), ("highlight", measureNum == modBy (List.length chords) count && not disabled) ] ]
        [ Html.text chord.name, Html.sup [] [ Html.text chord.alt ]]

view : Model -> Html Msg
view model =
    Html.div
        [ Attributes.class "grid" ]
        (List.indexedMap (viewChord model.count model.disabled) model.chords)


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
