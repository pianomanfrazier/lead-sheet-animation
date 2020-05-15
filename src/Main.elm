module Main exposing (main)

import Array
import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (Html, text)
import Html.Attributes as Attributes
import Json.Decode as D
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Keyboard.Key as KK


type alias Model =
    { song : Song
    , count : Int
    , chordCount : Int
    , disabled : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( Model song 0 0 False, Cmd.none )


type Msg
    = HandleKeyboardEvent KeyboardEvent


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleKeyboardEvent event ->
            case event.keyCode of
                KK.Spacebar ->
                    ( increment model , Cmd.none )

                KK.Right ->
                    ( increment model , Cmd.none )

                KK.Left ->
                    ( { model | count = model.count - 1, chordCount = 0 }, Cmd.none )

                KK.Delete ->
                    ( { model | count = 0, chordCount = 0 }, Cmd.none )

                KK.D ->
                    ( { model | disabled = not model.disabled }, Cmd.none )

                -- ignore anything else
                _ ->
                    ( model, Cmd.none )

increment : Model -> Model
increment model =
    let
        currentMeasure = Array.get (modBy (List.length model.song) model.count) (Array.fromList model.song) |> Maybe.withDefault []
        chordCount = List.length currentMeasure - 1
    in
    if model.chordCount < chordCount then
        { model | chordCount = model.chordCount + 1 }
    else
        { model | count = model.count + 1, chordCount = 0 }


subscriptions : Model -> Sub Msg
subscriptions _ =
    onKeyDown (D.map HandleKeyboardEvent decodeKeyboardEvent)


type alias Chord =
    { name : String
    , alt : String
    , space : Int
    }


type alias Measure =
    List Chord


type alias Song =
    List Measure


plainSong : Song
plainSong =
    [ [ Chord "F" "" 1 ]
    , [ Chord "F" "7" 1 ]
    , [ Chord "Bb" "" 1 ]
    , [ Chord "F" "" 1 ]
    , [ Chord "F" "" 1 ]
    , [ Chord "G" "7" 1 ]
    , [ Chord "C" "7" 1 ]
    , [ Chord "-" "" 1 ]
    , [ Chord "F" "" 1 ]
    , [ Chord "F" "7" 1 ]
    , [ Chord "Bb" "" 1 ]
    , [ Chord "F" "" 1 ]
    , [ Chord "F" "" 1 ]
    , [ Chord "C" "7" 1 ]
    , [ Chord "F" "" 1 ]
    , [ Chord "-" "" 1 ]
    ]

plainSongRomans : Song
plainSongRomans =
    [ [ Chord "I" "" 1 ]
    , [ Chord "V7/IV" "" 1 ]
    , [ Chord "IV" "" 1 ]
    , [ Chord "I" "" 1 ]
    , [ Chord "I" "" 1 ]
    , [ Chord "V7/V" "" 1 ]
    , [ Chord "V" "7" 1 ]
    , [ Chord "-" "" 1 ]
    , [ Chord "I" "" 1 ]
    , [ Chord "V/IV" "" 1 ]
    , [ Chord "IV" "" 1 ]
    , [ Chord "I" "" 1 ]
    , [ Chord "I" "" 1 ]
    , [ Chord "V" "7" 1 ]
    , [ Chord "I" "" 1 ]
    , [ Chord "-" "" 1 ]
    ]


reharmSong : Song
reharmSong =
    [ [ Chord "F" "" 1 ]
    , [ Chord "A" "7" 1 ]
    , [ Chord "Bb" "" 2
      , Chord "C(sus4)" "" 1
      ]
    , [ Chord "F" "" 2
      , Chord "A" "7" 1
      ]
    , [ Chord "Dmin" "7" 1 ]
    , [ Chord "G" "7" 1 ]
    , [ Chord "C" "7" 1 ]
    , [ Chord "-" "" 1 ]
    , [ Chord "F" "" 1 ]
    , [ Chord "A" "7" 1 ]
    , [ Chord "Bb" "" 2
      , Chord "Eb" "7" 1
      ]
    , [ Chord "F" "" 2
      , Chord "A7" "" 1
      ]
    , [ Chord "Dmin" "7" 1 ]
    , [ Chord "Gmin" "7" 2
      , Chord "C" "7" 1
      ]
    , [ Chord "F" "" 1 ]
    , [ Chord "C(sus4)" "" 1 ]
    ]

reharmSongRomans : Song
reharmSongRomans =
    [ [ Chord "I" "" 1 ]
    , [ Chord "V7/vi" "" 1 ]
    , [ Chord "IV" "" 2
      , Chord "V(sus4)" "" 1
      ]
    , [ Chord "I" "" 2
      , Chord "V7/vi" "" 1
      ]
    , [ Chord "vi" "" 1 ]
    , [ Chord "V7/V" "" 1 ]
    , [ Chord "V" "7" 1 ]
    , [ Chord "-" "" 1 ]
    , [ Chord "I" "" 1 ]
    , [ Chord "V7/vi" "7" 1 ]
    , [ Chord "IV" "" 2
      , Chord "bVII" "7" 1
      ]
    , [ Chord "I" "" 2
      , Chord "V7/vi" "" 1
      ]
    , [ Chord "vi" "" 1 ]
    , [ Chord "ii" "" 2
      , Chord "V" "7" 1
      ]
    , [ Chord "I" "" 1 ]
    , [ Chord "V(sus4)" "" 1 ]
    ]


song : Song
song =
    -- plainSong
    -- plainSongRomans
    reharmSong
    -- reharmSongRomans

viewChord : Model -> Int -> Int -> Chord -> Html Msg
viewChord model measureCount chordCount chord =
    Html.div
        [ Attributes.classList [("chord", True), ("highlight", modBy (List.length model.song) model.count == measureCount && model.chordCount == chordCount)]
        , Attributes.attribute "data-weight" (String.fromInt chord.space)
        , Attributes.attribute "data-chord" (String.fromInt chordCount)
        ]
        [ Html.text chord.name, Html.sup [] [ Html.text chord.alt ] ]


viewMeasure : Model -> Int -> Measure -> Html Msg
viewMeasure model measureCount measure =
    Html.div
        [ Attributes.class "measure"
        , Attributes.attribute "data-measure" (String.fromInt measureCount)
        ]
        (List.indexedMap (viewChord model measureCount) measure)


view : Model -> Html Msg
view model =
    let
        count = modBy (List.length model.song) model.count
    in
    Html.div
        [ Attributes.class "grid"
        , Attributes.classList [("disabled", model.disabled)]
        , Attributes.attribute "data-count" (String.fromInt count)
        ]
        (List.indexedMap (viewMeasure model) model.song)


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
