module Main exposing (main)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Round
import Array exposing (..)
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import List
import ShapeCreateAssets exposing (..)

sampleList = listToFloat (List.range 0 100)
divisorForList = 3

main =
    gameApp Tick
        { model = init -- init is the value in the field model
        , title = "Shape Creator"
        , view = view
        , update = update
        }

--init : {waves : {amp : Float, freq : Float, points : List (Shape userMsg) }}
init =
    { currentPage = 1,
    amplitude = 1,
    maxAmplitude = 10,
    minAmplitude = 1,
    frequency = 1,
    minFrequency = 0.1,
    waves = []
    }


view model =
        collage 1000 500 <|
            model.waves++[
                polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, 12 ) ] |> filled red |> move (-250, -150) |> notifyTap AmplitudeUp,
                polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, -12 ) ] |> filled red |> move (-250, -170) |> notifyTap AmplitudeDown,
                polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, 12 ) ] |> filled red |> move (-220, -150) |> notifyTap FrequencyUp,
                polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, -12 ) ] |> filled red |> move (-220, -170) |> notifyTap FrequencyDown,
                text (String.fromInt model.amplitude ++ " Sin (" ++ String.fromFloat model.frequency ++ "𝜋)") |> size 10 |> centered |> filled red |> move (-230, -160),
                rect 50 20 |> filled red |> move (-150, -160) |> notifyTap AddWave
            ]

update msg model =
    case msg of
        Tick t _ ->
            { model | currentPage = 2 }
        AmplitudeUp ->
            { model | amplitude = 
                if model.amplitude < model.maxAmplitude then
                    model.amplitude + 1 
                else
                    model.amplitude }
        AmplitudeDown ->
            { model | amplitude = 
                if model.amplitude > model.minAmplitude then
                    model.amplitude - 1
                else
                    model.amplitude }
        FrequencyUp ->
            { model | frequency = Maybe.withDefault 0 (String.toFloat (Round.round 1 (model.frequency + 0.1))) }
        FrequencyDown ->
            { model | frequency = 
                if model.frequency > model.minFrequency then
                    Maybe.withDefault 0 (String.toFloat (Round.round 1 (model.frequency - 0.1))) 
                else
                    model.frequency }
        AddWave ->
            { model | waves =
                model.waves++
                    List.indexedMap (\index y ->
                                makeCircle ((toFloat index)*(-5)) (y*(toFloat model.amplitude))
                                    ) (applySinFunc(divideBy(longerList sampleList)))
                
            }



longerList list = 
    (list ++ list ++ list)

textBigger str =
    str |> text |> selectable |> fixedwidth |> size 9 |> filled black

makeCircle x y =
    circle 3 |> filled black |> move (x, y)

applySinFunc list = 
    List.map sin list

listToFloat list =
    List.map toFloat list

divideBy list =
    List.map divideBy2 list

divideBy2 x = 
    x/divisorForList

type Msg m1
    = Tick Float GetKeyState
    | AmplitudeUp
    | AmplitudeDown
    | FrequencyUp
    | FrequencyDown
    | AddWave