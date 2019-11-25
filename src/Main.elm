module Main exposing (main)

import Html exposing (..)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid

import Array exposing (..)
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import List
import List.Extra
import ShapeCreateAssets exposing (..)

inputValues : List Float
inputValues = List.map(\x -> sin (toFloat x / 100)) <| (List.range 0 100)

samplingRate = 50

freqGraphWidth = 300
freqGraphHeight = 200
yScaleWidth = 8

barChart values =
  let
    numBins = List.length values
    maxAmplitude = List.maximum values |> Maybe.withDefault 5
    minAmplitude = List.minimum values |> Maybe.withDefault -5
    scaleHeight = freqGraphHeight / (maxAmplitude - minAmplitude)

    width = (freqGraphWidth - yScaleWidth) / toFloat numBins

    bar idx amplitude =
        group [ GraphicSVG.rect width (scaleHeight * (amplitude - minAmplitude))
                  |> filled (rgb 200 0 200)
                  |> move (0,0.5 * scaleHeight * (amplitude - minAmplitude) )
              ]
          |> move ( toFloat idx * width - 0.5 * freqGraphWidth + 0.5 * width + yScaleWidth
                  , -0.5 * toFloat freqGraphHeight)
  in
    (List.indexedMap bar values)
    -- ++ -- Y axis scale
    -- (List.map (\ num ->
    --     group [ GraphicSVG.text (String.fromFloat (minAmplitude + (toFloat num * (maxAmplitude / minAmplitude))))
    --                 |> GraphicSVG.size 7
    --                 |> bold
    --                 |> filled black
    --                 |> rotate (degrees 90)
    --                 |> move (0, (toFloat num * ((freqGraphHeight - 10) / 10)))
    --             ] |> move (-freqGraphWidth / 2, -0.5 * toFloat freqGraphHeight)
    --         ) <| (List.range 0 10)
    -- )
    ++
    [
        group [
            GraphicSVG.rect 0.5 freqGraphHeight |> filled black
                |> move (-freqGraphWidth / 2 + yScaleWidth / 2, 0),
            GraphicSVG.rect 0.5 freqGraphWidth |> filled black 
                |> rotate (degrees 90)
                |> move (yScaleWidth / 2, -freqGraphHeight / 2)
        ] |> move (0, 0)
    ]
    |> group

getSampledInput input =
  let
    inc = List.length input // samplingRate
    vals = Array.fromList input
  in
    List.map (\x ->
        Array.get (x * inc) vals |> Maybe.withDefault 0
      ) <| (List.range 0 samplingRate)

computeRealDFT realval =
  let
    n = List.length realval
  in
    List.map (\k ->
            computeDFT realval k
        ) <| (List.range 0 (n - 1))

computeDFT realval k =
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


main =
    gameApp Tick
        { model = init -- init is the value in the field model
        , title = "Fourier Transform"
        , view = view
        , update = update
        }


init =
    { currentPage = 1
    }


view model =
    Collage 700 500 <| 
        [
            barChart (computeRealDFT (getSampledInput inputValues)) |> move (0, 0)
        ]
        -- ++
        -- (List.indexedMap (\index val -> 
        --     GraphicSVG.text (String.fromFloat val)
        --             |> GraphicSVG.size 7
        --             |> bold
        --             |> filled black
        --             |> move ((toFloat index) * 100, 0)
        --   ) <| (computeRealDFT inputValues)
        -- )


update msg model =
        { model | currentPage = 2}

type Msg m1
    = Tick Float GetKeyState
    | In1