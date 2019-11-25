module Main exposing (main)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid

import Array exposing (..)
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import List
import ShapeCreateAssets exposing (..)

test = [-1, -0.5, 0, 0.5, 1, 0.5, 0]
test2 = [1,2,3,4,5,6,7,8]
main =
    gameApp Tick
        { model = init -- init is the value in the field model
        , title = "Shape Creator"
        , view = view
        , update = update
        }


init =
    { currentPage = 1
    , oneSat = 1
    , sinGraph = []
    }


view model =
    let
        ourTextGroup =
            group
                [ textBigger "Select your modifier!" |> move ( -40, 5 )
                ]
        -- Circle that rotates in time with the sin & cosin waves
    in
    collage 512 380 <|
              let listOfShapes =  (List.indexedMap (\index y ->
                                      makeCircle (toFloat index) (y*5)
                                 ) test)
              in
                    listOfShapes ++ [ circle 15
                        |> filled (oneColour model)
                        |> addOutline (solid 2)
                                orange
                        |> makeTransparent 0.7
                        |> move ( -95, 90 )
                    , wedge 5 0.75 |> filled (oneAccent model) |> makeTransparent 0.7 |> move ( -95, 86 )
                    , ourTextGroup |> move ( 80, 50)
                    ]



{- this funciton isn't being used -}
sinCurve2 model =
    let
        points =
            List.map2 (\x y -> ( x, y )) model.sinGraph (List.drop 1 model.sinGraph)
    in
    List.take (numGraphPoints model) (List.map (\( ( a, b, col1 ), ( c, d, col2 ) ) -> line ( a, b ) ( c, d ) |> outlined (solid 1) col1) points)

{-
TODO generate a list to define a sine wave
-}




makeCircle x y =
    circle 2 |> filled black |> move (x, y)

testCurve x y =
    circle 2 |> filled brown |> move (x,y)

update msg model =
        { model | currentPage = 2}

type Msg m1
    = Tick Float GetKeyState
    | In1

oneColour model =
    hsl (degrees 30) model.oneSat 0.85

oneAccent model =
    hsl (degrees 22) model.oneSat 0.6

textBigger str =
    str |> text |> selectable |> fixedwidth |> size 9 |> filled black

numGraphPoints model =
    round 2505

    {-
            indexedMap Tuple.pair <| initialize 4 (\n -> circle 5 |> filled brown |> move (n+10, n+50))

    (\n -> circle 5 |> filled brown |> move (n+10, n+50))
    -}
