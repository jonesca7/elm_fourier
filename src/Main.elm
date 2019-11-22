module Main exposing (main)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid

import Array exposing (..)
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import List
import ShapeCreateAssets exposing (..)

test = [-1, -0.5, 0, 0.5, 1, 0.5, 0, -0,5]

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
        
                    [ circle 15
                        |> filled (oneColour model)
                        |> addOutline (solid 2)
                                orange
                        |> makeTransparent 0.7
                        |> move ( -95, 90 )
                    , wedge 5 0.75 |> filled (oneAccent model) |> makeTransparent 0.7 |> move ( -95, 86 )
                    , ourTextGroup |> move ( 80, 50)
                    {-, group
                        [ 
                        testCurve 10.0 5.0
                        , testCurve 12.0 5.0
                        , testCurve 14.0 5.0
                        , testCurve 16.0 5.0
                        
                        

                        ]
                        |> move (50, -30 ) -- moves the wave display-}
                    ,   List.indexedMap (\index y ->
                            circle 2 |> filled black |> move (index, y)
                        ) test
                    ]

plotFunc t = 
    List.indexedMap (\index y ->
                        group[
                            circle 2 |> filled black |> move (index, y)
                        ]
                        ) <| t

testCurve x y =
    circle 2 |> filled brown |> move(x,y)
               
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


sinCurve model =
        List.map (\p -> 
            circle 5 |> filled brown |> move (toFloat p, 0)
        ) <| List.range 0 10
    {-
            indexedMap Tuple.pair <| initialize 4 (\n -> circle 5 |> filled brown |> move (n+10, n+50)) 

    (\n -> circle 5 |> filled brown |> move (n+10, n+50)) 
    -}



