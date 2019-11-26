module Main exposing (main)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Round
import Array exposing (..)
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import List
import List.Extra
import ShapeCreateAssets exposing (..)

waveWidth = 6 * pi
waveGraphWidth = 450
xStep = waveWidth / waveGraphWidth
waveScaleY = 20

freqGraphWidth = 350
freqGraphHeight = 250
yScaleWidth = 8

sampleList = listToFloat (List.range 0 200)
divisorForList = 3


barChart values =
  let
    numBins = List.length values
    maxAmplitude = List.maximum values |> Maybe.withDefault 5
    minAmplitude = List.minimum values |> Maybe.withDefault -5
    scaleHeight = freqGraphHeight / (maxAmplitude - minAmplitude)

    width = (freqGraphWidth - yScaleWidth) / toFloat numBins

    bar idx amplitude =
        group [ GraphicSVG.rect width (scaleHeight * (amplitude - minAmplitude))
                  |> filled blue
                  |> move (0,0.5 * scaleHeight * (amplitude - minAmplitude) )
              ]
          |> move ( toFloat idx * width - 0.5 * freqGraphWidth + 0.5 * width + yScaleWidth
                  , -0.5 * toFloat freqGraphHeight)
  in
    (List.indexedMap bar values)
    ++
    [
        group [
            GraphicSVG.rect 0.5 freqGraphHeight |> filled black
                |> move (-freqGraphWidth / 2 + yScaleWidth / 2, 0),
            GraphicSVG.text "Amplitude" |> size 9 |> centered |> filled black
                |> rotate (degrees 90)
                |> move (-freqGraphWidth / 2 - yScaleWidth / 2, 0),
            GraphicSVG.rect 0.5 freqGraphWidth |> filled black 
                |> rotate (degrees 90)
                |> move (yScaleWidth / 2, -freqGraphHeight / 2),
            GraphicSVG.text "Frequency" |> size 9 |> centered |> filled black
                |> move (0, -freqGraphHeight / 2 - 15)
        ] |> move (0, 0)
    ] |> group

-- Returns a list of sampled values from a given input
getSampledInput numSamples input =
  let
    inc = List.length input // numSamples
    vals = Array.fromList input
  in
    List.map (\x ->
        Array.get (x * inc) vals |> Maybe.withDefault 0
      ) <| (List.range 0 numSamples)

-- Computes the DFT of the given list of sampled real values
dft realval =
  let
    n = List.length realval
  in
    List.map (\k ->
            computeRealDFT realval k
        ) <| (List.range 0 (n - 1))

-- Computes the DFT at entry k
computeRealDFT realval k =
  let
    n = List.length realval
  in
    (List.Extra.indexedFoldl
        (\t x a ->
          let
            angle = 2 * pi * toFloat t * (toFloat k / toFloat n)
          in
            a + ((x * cos (angle)) + (x * sin (angle)))
        ) 0 realval)

getSumWave : List (Wave) -> Float -> Float
getSumWave waves x =
  List.foldl (\wave y ->
      y + wave.amp * sin (x * wave.freq + (wave.phase * pi))
    ) 0 waves


main =
    gameApp Tick
        { model = init -- init is the value in the field model
        , title = "Shape Creator"
        , view = view
        , update = update
        }

init =
    { currentPage = 1,
    amplitude = 1,
    maxAmplitude = 10,
    minAmplitude = 1,
    frequency = 1,
    minFrequency = 0.1,
    phase = 0,
    samplingRate = 50,
    waves = []
    }


view model =
    collage 1000 500 <|
        --model.waves
        --++
        [
            polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, 12 ) ] |> filled red |> move (-250, -150) |> notifyTap AmplitudeUp,
            polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, -12 ) ] |> filled red |> move (-250, -170) |> notifyTap AmplitudeDown,
            polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, 12 ) ] |> filled red |> move (-220, -150) |> notifyTap FrequencyUp,
            polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, -12 ) ] |> filled red |> move (-220, -170) |> notifyTap FrequencyDown,
            polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, 12 ) ] |> filled red |> move (-190, -150) |> notifyTap PhaseUp,
            polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, -12 ) ] |> filled red |> move (-190, -170) |> notifyTap PhaseDown,
            text (String.fromFloat model.amplitude ++ " Sin (" ++ String.fromFloat model.frequency ++ " + " ++ String.fromFloat model.phase ++ "𝜋)") |> size 10 |> centered |> filled red |> move (-230, -160),
            rect 50 20 |> filled red |> move (-150, -160) |> notifyTap AddWave,
            GraphicSVG.text "Frequency Domain" |> size 12 |> centered |> filled black |> move (200, 200),
          group [
            polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, 12 ) ] |> filled red |> move (48, 12) |> notifyTap SamplingUp,
            polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, -12 ) ] |> filled red |> move (48, -6) |> notifyTap SamplingDown,
            GraphicSVG.text ("Sampling Rate (N) = " ++ String.fromInt model.samplingRate) |> size 10 |> centered |> filled black |> move (0, 0)
          ] |> move (200, -175),
          barChart (dft (getSampledInput model.samplingRate (List.map(\x -> getSumWave model.waves (toFloat x * xStep)) <| List.range 0 waveGraphWidth ))) 
            |> move (200, 50) 
        ]
        ++ -- Draw the combination wave at the top
        (List.map (\x ->
            group [
              GraphicSVG.circle 1 |> filled black |> move (toFloat x, (getSumWave model.waves (toFloat x * xStep)) * waveScaleY)
            ] |> move (-480, 200)
          ) <| List.range 0 waveGraphWidth
        )

update msg model =
    case msg of
        Tick t _ ->
            { model | currentPage = 2 
            }
        AmplitudeUp ->
            { model | amplitude = 
                if model.amplitude < model.maxAmplitude then
                    model.amplitude + 0.5
                else
                    model.amplitude 
            }
        AmplitudeDown ->
            { model | amplitude = 
                if model.amplitude > model.minAmplitude then
                    model.amplitude - 0.5
                else
                    model.amplitude 
            }
        FrequencyUp ->
            { model | frequency = Maybe.withDefault 0 (String.toFloat (Round.round 1 (model.frequency + 0.1))) 
            }
        FrequencyDown ->
            { model | frequency = 
                if model.frequency > model.minFrequency then
                    Maybe.withDefault 0 (String.toFloat (Round.round 1 (model.frequency - 0.1))) 
                else
                    model.frequency 
            }
        PhaseUp ->
            { model | phase =
                Maybe.withDefault 0 (String.toFloat (Round.round 1 (model.phase + 0.1))) 
            }
        PhaseDown ->
            { model | phase =
                Maybe.withDefault 0 (String.toFloat (Round.round 1 (model.phase - 0.1))) 
            }
        AddWave ->
            { model | waves =
                model.waves ++ [{amp = model.amplitude, freq = model.frequency, phase = model.phase}]
                    --(List.indexedMap (\index y ->
                    --                    makeCircle ((toFloat index)*(-2) - 50) (y*model.amplitude)
                    --                        ) (applySinFunc (divideBy sampleList)))
            }
        SamplingUp ->
            { model | samplingRate = 
                if model.samplingRate < waveGraphWidth - 1 then
                    model.samplingRate + 1
                else
                    model.samplingRate }
        SamplingDown ->
            { model | samplingRate = 
                if model.samplingRate > 2 then
                    model.samplingRate - 1
                else
                    model.samplingRate }

makeCircle x y =
    circle 2 |> filled black |> move (x, y)

applySinFunc list = 
    List.map sin list

listToFloat list =
    List.map toFloat list

divideBy list =
    List.map divideBy2 list

divideBy2 x = 
    x/divisorForList

type alias Wave =
    {
    amp : Float,
    freq : Float,
    phase : Float
    }

type Msg m1
    = Tick Float GetKeyState
    | AmplitudeUp
    | AmplitudeDown
    | FrequencyUp
    | FrequencyDown
    | PhaseUp
    | PhaseDown
    | AddWave
    | SamplingUp
    | SamplingDown