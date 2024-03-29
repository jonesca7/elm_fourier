{-
User: Mechatronics/Software engineering students currently enrolled in 3MX3 (Signals and Systems).

Problem: Many students are capable of working through the math of the Fourier Transform, but have trouble understanding what
it actually does and why we might want to use it.

Solution: This application gives a visual demonstration of what the Fourier Transform does, in a way that is interactive and
easy to use. Unlike other applications, adding and removing waves instantly changes the time and frequency domain representation
of the signal, rather than requiring the user to submit changes and waiting for the program to process.
-}
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
xStep = waveWidth / waveGraphWidth --Difference between consecutive x values
waveScaleY = 10
waveHeight = 50

freqGraphWidth = 350
freqGraphHeight = 250
yScaleWidth = 8

barChart values =
  let
    numBins = List.length values
    maxAmplitude = values |> List.maximum |> Maybe.withDefault 1
    minAmplitude = values |> List.minimum |> Maybe.withDefault 0
    scaleHeight = if (maxAmplitude - minAmplitude) /= 0 then freqGraphHeight / (maxAmplitude - minAmplitude) else 0

    width = (freqGraphWidth - yScaleWidth) / toFloat numBins

    bar idx amplitude =
        group [ GraphicSVG.rect width (scaleHeight * (amplitude - minAmplitude))
                |> filled blue
                |> move (0,0.5 * scaleHeight * (amplitude - minAmplitude) )
              ] |> move ( toFloat idx * width - 0.5 * freqGraphWidth + 0.5 * width + yScaleWidth, -0.5 * toFloat freqGraphHeight)
  in
    (List.indexedMap bar values)
    ++
    [
        group [
            GraphicSVG.rect 0.5 freqGraphHeight |> filled grey
                |> move (-freqGraphWidth / 2 + yScaleWidth / 2, 0),
            GraphicSVG.text "Amplitude" |> size 9 |> centered |> filled black
                |> rotate (degrees 90)
                |> move (-freqGraphWidth / 2 - yScaleWidth, 0),
            GraphicSVG.rect 0.5 freqGraphHeight |> filled black
                |> move (yScaleWidth / 2, 0),
            GraphicSVG.rect 0.5 freqGraphWidth |> filled black |> rotate (degrees 90)
                |> move (yScaleWidth / 2, -freqGraphHeight / 2),
            GraphicSVG.text "Frequency" |> size 9 |> centered |> filled black
                |> move (0, -freqGraphHeight / 2 - 15),
            GraphicSVG.rect 0.5 freqGraphWidth |> filled black |> rotate (degrees 90)
                |> move (yScaleWidth / 2, if minAmplitude > 0 then 0 else -freqGraphHeight / 2 + scaleHeight * -minAmplitude),
            GraphicSVG.text "0" |> size 8 |> filled black
                |> move (-freqGraphWidth / 2 - 2, (if minAmplitude > 0 then 0 else -freqGraphHeight / 2 + scaleHeight * -minAmplitude) - 2)
        ] |> move (0, 0)
    ] |> group


getComplexComponent model vals =
    if model.graphComponent == "real" then
        List.map .real vals
    else
        List.map .imag vals


-- Returns a list of sampled values of the combined wave function
getSampledInput numSamples waves =
  let
    inc = (10 * pi) / toFloat numSamples
  in
    List.map(\x -> 
        getSumWave waves (toFloat x * inc)
    ) <| List.range 0 numSamples


-- Computes the DFT of the given list of sampled real values
computeDFT realval =
  let
    n = List.length realval
    vals = Array.fromList realval
  in
    List.map (\k ->
        {
            real = (List.foldl(\t sum ->
                        let
                            angle = -2 * pi * toFloat t * toFloat k / toFloat n
                            val = Array.get t vals |> Maybe.withDefault 0
                        in
                            sum + val * cos(angle)
                    ) 0 (List.range 0 n)) / toFloat n,
            imag = (List.foldl(\t sum ->
                        let
                            angle = -2 * pi * toFloat t * toFloat k / toFloat n
                            val = Array.get t vals |> Maybe.withDefault 0
                        in
                            sum + val * sin(angle)
                    ) 0 (List.range 0 n)) / toFloat n
        }
    ) <| (List.range 0 n)


getSumWave waves x =
    List.foldl (\wave y ->
        y + wave.amp * sin (x * wave.freq + (wave.phase * pi))
    ) 0 waves

getWaveIndex model (x,y) =
    List.foldl(\i index -> 
        let
            top = Debug.log "top = " 96 - (toFloat i * waveHeight)
            bottom = Debug.log "bottom = " 76 - (toFloat i * waveHeight)
        in
            -- Super hacky way to get the index of the button pressed based on known positions
            if Debug.log "y = " y < top && y > bottom then
                index + i
            else
                index
    ) 0 <| List.range 0 5


main =
    gameApp Tick
        { model = init -- init is the value in the field model
        , title = "Fourier Transform Visualizer"
        , view = view
        , update = update
        }

--Initialize values
init =
    { currentPage = 1,
    amplitude = 1,
    maxAmplitude = 2.5,
    minAmplitude = 0.1,
    frequency = 1,
    maxFrequency = 10,
    minFrequency = 0.1,
    phase = 0,
    maxPhase = 1.9,
    sampleCount = 100,
    waves = [{amp = 1, freq = 1, phase = 0}],
    maxWaves = 6,
    graphComponent = "real"
    }


view model =
    collage 1000 500 <|
        [
            GraphicSVG.text "Frequency Domain" |> size 12 |> centered |> filled black |> move (240, 200),
            group [
                polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, 12 ) ] |> filled red |> move (54, 12) |> notifyTap SamplingUp,
                polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, -12 ) ] |> filled red |> move (54, -6) |> notifyTap SamplingDown,
                text ("Number of Samples (N) = " ++ String.fromInt model.sampleCount) |> size 10 |> centered |> filled black |> move (0, 0)
            ] |> move (200, -175),
            group [
                text "Show: " |> size 10 |> centered |> filled black |> move (0, 0),
                text "Real" |> size 10 |> centered |> (if model.graphComponent == "real" then filled black else filled grey) |> move (30, 0) |> notifyTap SetGraphReal,
                text "Imag" |> size 10 |> centered |> (if model.graphComponent == "imag" then filled black else filled grey) |> move (60, 0) |> notifyTap SetGraphImag
            ] |> move (320, -175),
            barChart (getComplexComponent model (computeDFT (getSampledInput model.sampleCount model.waves))) |> move (240, 50),
            GraphicSVG.line (0, 0) (waveGraphWidth + 60, 0) |> outlined (solid 1) grey |> move (-510, 125),

            -- Dr. Anand, please consider being able to send custom data with notifyTap so I don't have to write this mess :)
            GraphicSVG.circle 10 |> filled red |> makeTransparent 0.2 |> addOutline (solid 1) red |> move (0, 100) |> notifyTap RemoveWave0,
            text "🗑" |> size 12 |> filled red |> move (-4, 96) |> notifyTap RemoveWave0,
            GraphicSVG.circle 10 |> filled red |> makeTransparent 0.2 |> addOutline (solid 1) red |> move (0, 100 - (1 * waveHeight)) |> notifyTap RemoveWave1,
            text "🗑" |> size 12 |> filled red |> move (-4, 96 - (1 * waveHeight)) |> notifyTap RemoveWave1,
            GraphicSVG.circle 10 |> filled red |> makeTransparent 0.2 |> addOutline (solid 1) red |> move (0, 100 - (2 * waveHeight)) |> notifyTap RemoveWave2,
            text "🗑" |> size 12 |> filled red |> move (-4, 96 - (2 * waveHeight)) |> notifyTap RemoveWave2,
            GraphicSVG.circle 10 |> filled red |> makeTransparent 0.2 |> addOutline (solid 1) red |> move (0, 100 - (3 * waveHeight)) |> notifyTap RemoveWave3,
            text "🗑" |> size 12 |> filled red |> move (-4, 96 - (3 * waveHeight)) |> notifyTap RemoveWave3,
            GraphicSVG.circle 10 |> filled red |> makeTransparent 0.2 |> addOutline (solid 1) red |> move (0, 100 - (4 * waveHeight)) |> notifyTap RemoveWave4,
            text "🗑" |> size 12 |> filled red |> move (-4, 96 - (4 * waveHeight)) |> notifyTap RemoveWave4,
            GraphicSVG.circle 10 |> filled red |> makeTransparent 0.2 |> addOutline (solid 1) red |> move (0, 100 - (5 * waveHeight)) |> notifyTap RemoveWave5,
            text "🗑" |> size 12 |> filled red |> move (-4, 96 - (5 * waveHeight)) |> notifyTap RemoveWave5
        ]
        ++ -- The mess continues... This covers up buttons that shouldn't be active
        (List.map(\i ->
                GraphicSVG.circle 10 |> filled white |> addOutline (solid 2) white |> move (0, 100 - (toFloat i * waveHeight))
            ) <| List.range (List.length model.waves) model.maxWaves
        )
        ++ -- Draw each of the wave components
        (List.Extra.indexedFoldl(\i w a ->
            a
            ++
            [
                GraphicSVG.line (0, 0) (waveGraphWidth, 0) |> outlined (dotted 1) grey |> move (-480, 100 - (toFloat i * waveHeight)),
                GraphicSVG.line (0, 0) (waveGraphWidth + 60, 0) |> outlined (solid 1) grey |> move (-510, 75 - (toFloat i * waveHeight))
            ]
            ++
            (List.map(\x -> 
              let
                p1 = {x = toFloat x, y = w.amp * sin(w.freq * toFloat x * xStep + (w.phase * pi)) * waveScaleY}
                p2 = {x = toFloat x + 1, y = w.amp * sin(w.freq * (toFloat x + 1) * xStep + (w.phase * pi)) * waveScaleY}
              in
                GraphicSVG.line (p1.x, p1.y) (p2.x, p2.y) |> outlined (solid 1) black |> move (-480, 100 - (toFloat i * waveHeight))
            ) <| List.range 0 waveGraphWidth)
          ) [] model.waves
        )
        ++ -- Draw the combination wave at the top
        [
            GraphicSVG.line (0, 0) (waveGraphWidth, 0) |> outlined (dotted 1) grey |> move (-480, 200)
        ]
        ++
        (List.map (\x ->
          let
            p1 = {x = toFloat x, y = (getSumWave model.waves (toFloat x * xStep)) * waveScaleY}
            p2 = {x = toFloat x + 1, y = (getSumWave model.waves ((toFloat x + 1) * xStep)) * waveScaleY}
          in
            GraphicSVG.line (p1.x, p1.y) (p2.x, p2.y) |> outlined (solid 1) black |> move (-480, 200)
          ) <| List.range 0 (waveGraphWidth - 1)
        )
        ++
        [
            --GraphicSVG.line (0, 0) (0, 500) |> outlined (solid 1) grey |> move (0, -250),
            GraphicSVG.rect 500 80 |> filled white |> move (-252, -217),
            group [
                polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, 12 ) ] |> filled red |> move (-5, 12) |> notifyTap AmplitudeUp,
                polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, -12 ) ] |> filled red |> move (-5, -6) |> notifyTap AmplitudeDown,
                polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, 12 ) ] |> filled red |> move (30, 12) |> notifyTap FrequencyUp,
                polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, -12 ) ] |> filled red |> move (30, -6) |> notifyTap FrequencyDown,
                polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, 12 ) ] |> filled red |> move (55, 12) |> notifyTap PhaseUp,
                polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, -12 ) ] |> filled red |> move (55, -6) |> notifyTap PhaseDown,
                text "y(x) = " |> size 10 |> centered |> filled black |> move (-30, 0),
                text (String.fromFloat model.amplitude ++ " sin " ) |> size 10 |> centered |> filled black |> move (0, 0),
                text ("( " ++ String.fromFloat model.frequency ++ "x + ") |> size 10 |> centered |> filled black |> move (30, 0),
                text (String.fromFloat model.phase ++ "𝜋)") |> size 10 |> centered |> filled black |> move (57, 0)
            ] |> move (-320, -210),
            group [
                rect 60 20 |> (if List.length model.waves == model.maxWaves then filled grey else filled blue) |> move (0, 0) |> notifyTap AddWave,
                text "Add wave" |> sansserif |> centered |> filled white |> move (0, -4) |> notifyTap AddWave
            ] |> move (-200, -207),
            group [
                rect 60 20 |> (if List.length model.waves == 0 then filled grey else filled red) |> move (0, 0) |> notifyTap Reset,
                text "Reset" |> sansserif |> centered |> filled white |> move (0, -4) |> notifyTap Reset
            ] |> move (-80, -207)
        ]


update msg model =
    case msg of
        Tick t _ ->
            { model | currentPage = 2 
            }
        AmplitudeUp ->
            { model | amplitude = 
                if model.amplitude < model.maxAmplitude then
                    Maybe.withDefault 0 (String.toFloat (Round.round 1 (model.amplitude + 0.1)))
                else
                    model.amplitude 
            }
        AmplitudeDown ->
            { model | amplitude = 
                if model.amplitude > model.minAmplitude then
                    Maybe.withDefault 0 (String.toFloat (Round.round 1 (model.amplitude - 0.1))) 
                else
                    model.amplitude 
            }
        FrequencyUp ->
            { model | frequency = 
                if model.frequency < model.maxFrequency then
                    Maybe.withDefault 0 (String.toFloat (Round.round 1 (model.frequency + 0.1)))
                else
                    model.frequency
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
                if model.phase < model.maxPhase then
                    Maybe.withDefault 0 (String.toFloat (Round.round 1 (model.phase + 0.1)))
                else
                    0.0
            }
        PhaseDown ->
            { model | phase =
                if model.phase > 0 then
                    Maybe.withDefault 0 (String.toFloat (Round.round 1 (model.phase - 0.1))) 
                else
                    model.maxPhase
            }
        AddWave ->
            { model | waves =
                if List.length model.waves < model.maxWaves then
                    model.waves ++ [{amp = model.amplitude, freq = model.frequency, phase = model.phase}]
                else
                    model.waves
            }
        SamplingUp ->
            { model | sampleCount = 
                if model.sampleCount < waveGraphWidth - 1 then
                    model.sampleCount + 1
                else
                    model.sampleCount
            }
        SamplingDown ->
            { model | sampleCount = 
                if model.sampleCount > 2 then
                    model.sampleCount - 1
                else
                    model.sampleCount 
            }
        Reset ->
            { model | waves = [], amplitude = 1, frequency = 1, phase = 0 }
        SetGraphReal ->
            { model | graphComponent = "real" }
        SetGraphImag ->
            { model | graphComponent = "imag" }
        RemoveWave0 ->
            { model | waves = List.Extra.removeAt 0 model.waves }
        RemoveWave1 ->
            { model | waves = List.Extra.removeAt 1 model.waves }
        RemoveWave2 ->
            { model | waves = List.Extra.removeAt 2 model.waves }
        RemoveWave3 ->
            { model | waves = List.Extra.removeAt 3 model.waves }
        RemoveWave4 ->
            { model | waves = List.Extra.removeAt 4 model.waves }
        RemoveWave5 ->
            { model | waves = List.Extra.removeAt 5 model.waves }

type alias Wave =
    {
        amp : Float,
        freq : Float,
        phase : Float
    }

type alias Complex =
    {
        real: Float,
        imag: Float
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
    | Reset
    | SetGraphReal
    | SetGraphImag
    | RemoveWave0
    | RemoveWave1
    | RemoveWave2
    | RemoveWave3
    | RemoveWave4
    | RemoveWave5
