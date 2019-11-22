module SinCreator exposing (..)
{-

User:  A young student learning about trigonometric waves and learning about elm and how to implement this into their projects.

Activity:  The user will learn about trigonometric waves and discover how the wave changes when different parameter values are entered

Emotion:  The user should feel encouraged to explore different options and the feedback of their inputs on the page

Tasks:  

Typical Interaction:  After being satisfied with the sine wave and its definition, the user might wonder if there are modifiers that can be applied
to the current shape. Having all of the options visible makes this easy for the user to select different choices. The user would click 
on the options they are interested in and see the expected result

Principle 1:  First of Norman's principles and how it guided your design
-> Discoverability through the use of signifiers
By removing the arrows and instead adding a menu, the discoverability of the modifiers increases.
The signifiers become more visibile and obvious to the user encouraging the user to try out every option through understanding which
options are available and the modifier currently in place.

Principle 2:  Second of Norman's principles and how it guided your design.
-> Mapping
By changing the interface so that the user changes a named parameter which maps by order and colour of 
the parameter values, the user can easily map which parameter corresponds to what changes about the
sine wave, as well as what changes in the sine wave equation. This mapping is more effective than the
previous implementation of arrows over the values in the equation as it helps the user better understand
what they are changing and how it affects the sine wave.

-}

import Array exposing (..)
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import List
import ShapeCreateAssets exposing (..)



init =
    { time = Nothing
    , currentTime = 0
    , notify = NotifyTap
    , uArg = 0
    , vArg = 0
    , editableArg = 0
    , uDilation = 1
    , vDilation = 1
    , editableDilation = 0
    , editableShift = 0
    , uScale = 5
    , vScale = 5
    , editableScale = 0
    , uShift = 0
    , uShiftScale = 1
    , u = 1
    , v = 1
    , rScale = 1
    , gScale = 1
    , bScale = 1
    , rFun = OneFun
    , bFun = UFun
    , gFun = VFun
    , sinGraph = []
    , cosGraph = []
    , vTransparency = 0.5
    , trigCycleU = Sin
    , trigCycleV = Sin
    , latestPointV = ( 0, 0, rgb 160 128 96 )
    , uTransform = ScaleU
    , moveX = ZeroFun
    , moveY = UFunZero
    , moveX1 = UFunZero
    , moveY1 = ZeroFun
    , transformFun = ZeroFun
    , uCosGraph = 0
    , uSinGraph = 0
    , editableYSinForTransforms = 0
    , r = 0
    , g = 0
    , b = 0
    , currentButton = None
    , buttonDownTime = 0
    , transformsRightArrowTransp = 0.25
    , transformsLeftArrowTransp = 0.25
    , transformsTransp1= 0.25
    , transformsTransp2= 0.25
    , transformsTransp3= 0.25
    , transformsTransp4= 0.25
    , transformsTransp5= 0.25
    , transformsTransp6= 0.25
    , transformsTransp7= 0.25
    , transformsTransp8= 0.25
    , transformsTransp9= 0.25

    --, transformsNumTransp = 0.25
    , moveTextX = 0.25
    , moveTextY = 0.25
    , moveTextX1 = 0.25
    , moveTextY1 = 0.25
    , rTransp = 0.25
    , gTransp = 0.25
    , bTransp = 0.25
    , addAnotherFuncTransp = 0.25
    , uTextTransp = 0.5
    , vTextTransp = 0.5
    , maxAmplitude = 40
    , maxFrequency = 10
    , maxShift = 2 * Basics.pi
    , cosWaveLength = 150
    , sinWaveLength = 100
    }


type Msg m
    = Tick Float GetKeyState
    | TransM (m -> m)
    | Notif Notifications
    | R
    | G
    | B
    | UScalePlus
    | UDilationPlus
    | UShiftPlus
    | UScaleMinus
    | UDilationMinus
    | UShiftMinus
    | EditableScalePlus
    | EditableDilationPlus
    | EditableScaleMinus
    | EditableDilationMinus
    | VScalePlus
    | VScaleMinus
    | VDilationPlus
    | VDilationMinus
    | TrigCycleU
    | TrigCycleV
    | UTransforms1
    | UTransforms2
    | UTransforms3
    | UTransforms4
    | UTransforms5
    | UTransforms6
    | UTransforms7
    | UTransforms8
    | UTransforms9
    | UTransforms
    | UTransformsReverse
      --| MoveX
      --| MoveY
      --| MoveX1
      --| MoveY1
      --| TransformsFunctionChange
    | RScalePlus
    | RScaleMinus
    | GScalePlus
    | GScaleMinus
    | BScalePlus
    | BScaleMinus
    | ButtonDown ButtonDir
    | MouseUp


type Notifications
    = NotifyTap
    | NotifyTapAt
    | NotifyEnter
    | NotifyEnterAt
    | NotifyLeave
    | NotifyLeaveAt
    | NotifyMouseMoveAt
    | NotifyMouseDown
    | NotifyMouseDownAt
    | NotifyMouseUp
    | NotifyMouseUpAt
    | NotifyTouchStart
    | NotifyTouchStartAt
    | NotifyTouchEnd
    | NotifyTouchEndAt
    | NotifyTouchMoveAt


type FunType
    = OneFun
    | UFun
    | VFun


type Trig
    = Sin
    | Cos


type ZeroFunType
    = ZeroFun
    | UFunZero
    | NegUFun
    | VFunZero
    | NegVFun


type Transforms
    = ScaleU
    | MoveX
    | MoveY
    | MoveCircle
    | URotate
    | ScaleX
    | ScaleY
    | MakeTransparent
    | EditableXSin


type ButtonDir
    = AmplitudeUp
    | AmplitudeDown
    | FrequencyUp
    | FrequencyDown
    | ShiftUp
    | ShiftDown
    | EditableAmplitudeUp
    | EditableAmplitudeDown
    | EditableFrequencyUp
    | EditableFrequencyDown
    | RedUp
    | RedDown
    | BlueUp
    | BlueDown
    | GreenUp
    | GreenDown
    | None
    | VUP
    | VDown


update msg model =
    case msg of
        Tick t _ ->
            let
                uArg =
                    model.uArg + model.uDilation * (t - (t - 0.05))

                vArg =
                    model.vArg + model.vDilation * (t - (t - 0.05))

                editableArg =
                    model.editableArg + model.editableDilation * (t - (t - 0.05))

                currentTime =
                    case model.time of
                        Nothing ->
                            0

                        Just ct ->
                            ct

                u =
                    model.uScale * evalTrig model.trigCycleU uArg

                v =
                    model.vScale * evalTrig model.trigCycleV vArg

                r =
                    clamp 0 255 (abs (model.rScale * eval model.rFun u v))

                g =
                    clamp 0 255 (abs (model.gScale * eval model.gFun u v))

                b =
                    clamp 0 255 (abs (model.bScale * eval model.bFun u v))

                uSinGraph =
                    model.uScale * sin uArg

                sinGraphPoint =
                    ( 0, uSinGraph, rgb r g b )

                cosGraphPoint =
                    ( uCosGraph, 0, rgb r g b )

                uCosGraph =
                    model.uScale * cos uArg

                editableYSinForTransforms =
                    model.editableScale * cos editableArg
            in
            { model
                | time = Just t
                , uArg = uArg
                , vArg = vArg
                , currentTime = currentTime
                , u = u
                , v = v
                , sinGraph =
                    List.take 2470
                        ([ sinGraphPoint ]
                            ++ List.filterMap
                                (\( xx, yy, cc ) ->
                                    if xx >= model.sinWaveLength then
                                        Nothing

                                    else
                                        Just ( xx + 0.35, yy, cc )
                                )
                                model.sinGraph
                        )
                , cosGraph =
                    List.take 2470
                        ([ cosGraphPoint ]
                            ++ List.filterMap
                                (\( xx, yy, cc ) ->
                                    -- Subtract 130 to account for the ratio of the screen and remove excess
                                    if yy <= -model.cosWaveLength then
                                        Nothing

                                    else
                                        Just ( xx, yy - 0.35, cc )
                                )
                                model.cosGraph
                        )
                , r = r
                , g = g
                , b = b
                , uCosGraph = uCosGraph
                , uSinGraph = uSinGraph

                --, editableYSinForTransforms = editableYSinForTransforms
                , buttonDownTime =
                    case model.currentButton of
                        None ->
                            0

                        _ ->
                            model.buttonDownTime + 0.1
                , uScale =
                    case model.currentButton of
                        AmplitudeUp ->
                            if model.uScale < model.maxAmplitude then
                                model.uScale + curveX model.buttonDownTime

                            else if model.uScale > model.maxAmplitude then
                                model.maxAmplitude

                            else
                                model.uScale

                        AmplitudeDown ->
                            if model.uScale > -model.maxAmplitude then
                                model.uScale - curveX model.buttonDownTime

                            else if model.uScale < -model.maxAmplitude then
                                -model.maxAmplitude

                            else
                                model.uScale

                        _ ->
                            model.uScale
                , uDilation =
                    case model.currentButton of
                        FrequencyUp ->
                            if model.uDilation < model.maxFrequency then
                                model.uDilation + curveX model.buttonDownTime

                            else if model.uDilation > model.maxFrequency then
                                model.maxFrequency

                            else
                                model.uDilation

                        FrequencyDown ->
                            if model.uDilation > -model.maxFrequency then
                                model.uDilation - curveX model.buttonDownTime

                            else if model.uDilation < -model.maxFrequency then
                                -model.maxFrequency

                            else
                                model.uDilation

                        _ ->
                            model.uDilation
                , uShift =
                    case model.currentButton of
                        ShiftUp ->
                            model.uShift + curveX model.buttonDownTime

                        ShiftDown ->
                            model.uShift - curveX model.buttonDownTime

                        _ ->
                            model.uShift
                , editableScale =
                    case model.currentButton of
                        EditableAmplitudeUp ->
                            if model.editableScale < model.maxAmplitude then
                                model.editableScale + curveX model.buttonDownTime

                            else if model.editableScale > model.maxAmplitude then
                                model.maxAmplitude

                            else
                                model.editableScale

                        EditableAmplitudeDown ->
                            if model.editableScale > -model.maxAmplitude then
                                model.editableScale - curveX model.buttonDownTime

                            else if model.editableScale < -model.maxAmplitude then
                                -model.maxAmplitude

                            else
                                model.editableScale

                        _ ->
                            model.editableScale
                , editableDilation =
                    case model.currentButton of
                        EditableFrequencyUp ->
                            if model.editableDilation < model.maxFrequency then
                                model.editableDilation + curveX model.buttonDownTime

                            else if model.editableDilation > model.maxFrequency then
                                model.maxFrequency

                            else
                                model.editableDilation

                        EditableFrequencyDown ->
                            if model.editableDilation > -model.maxFrequency then
                                model.editableDilation - curveX model.buttonDownTime

                            else if model.editableDilation < -model.maxFrequency then
                                -model.maxFrequency

                            else
                                model.editableDilation

                        _ ->
                            model.editableDilation
                , editableShift =
                    case model.currentButton of
                        ShiftUp ->
                            model.editableShift + curveX model.buttonDownTime

                        ShiftDown ->
                            model.editableShift - curveX model.buttonDownTime

                        _ ->
                            model.editableShift
                , rScale =
                    case model.currentButton of
                        RedUp ->
                            if model.rScale < 253 then
                                model.rScale + curveX model.buttonDownTime

                            else
                                model.rScale

                        RedDown ->
                            if model.rScale > 2 then
                                model.rScale - curveX model.buttonDownTime

                            else
                                model.rScale

                        _ ->
                            model.rScale
                , bScale =
                    case model.currentButton of
                        BlueUp ->
                            if model.bScale < 253 then
                                model.bScale + curveX model.buttonDownTime

                            else
                                model.bScale

                        BlueDown ->
                            if model.bScale > 2 then
                                model.bScale - curveX model.buttonDownTime

                            else
                                model.bScale

                        _ ->
                            model.bScale
                , gScale =
                    case model.currentButton of
                        GreenUp ->
                            if model.gScale < 252 then
                                model.gScale + curveX model.buttonDownTime

                            else
                                model.gScale

                        GreenDown ->
                            if model.gScale > 2 then
                                model.gScale - curveX model.buttonDownTime

                            else
                                model.gScale

                        _ ->
                            model.gScale
                , vScale =
                    case model.currentButton of
                        VUP ->
                            if model.vScale < 48 then
                                model.vScale + curveX model.buttonDownTime

                            else
                                model.vScale

                        VDown ->
                            if model.vScale > -48 then
                                model.vScale - curveX model.buttonDownTime

                            else
                                model.vScale

                        _ ->
                            model.vScale
            }

        TransM t ->
            t model

        -- ran out of room for notifications, but left them here for a possible future improvement
        Notif notif ->
            { model | notify = notif }

        R ->
            { model | rFun = cycleFun model.rFun }

        G ->
            { model | gFun = cycleFun model.gFun }

        B ->
            { model | bFun = cycleFun model.bFun }

        RScalePlus ->
            { model
                | rScale =
                    if model.rScale < 255 then
                        model.rScale + 1

                    else
                        model.rScale
            }

        RScaleMinus ->
            { model
                | rScale =
                    if model.rScale > 0 then
                        model.rScale - 1

                    else
                        model.rScale
            }

        GScalePlus ->
            { model
                | gScale =
                    if model.gScale < 255 then
                        model.gScale + 1

                    else
                        model.gScale
            }

        GScaleMinus ->
            { model
                | gScale =
                    if model.gScale > 0 then
                        model.gScale - 1

                    else
                        model.gScale
            }

        BScalePlus ->
            { model
                | bScale =
                    if model.bScale < 255 then
                        model.bScale + 1

                    else
                        model.bScale
            }

        BScaleMinus ->
            { model
                | bScale =
                    if model.bScale > 0 then
                        model.bScale - 1

                    else
                        model.bScale
            }

        UScalePlus ->
            { model
                | uScale =
                    if model.uScale < model.maxAmplitude then
                        model.uScale + 1

                    else
                        model.uScale
            }

        UScaleMinus ->
            { model
                | uScale =
                    if model.uScale > -model.maxAmplitude then
                        model.uScale - 1

                    else
                        model.uScale
            }

        UDilationPlus ->
            { model
                | uDilation =
                    if model.uDilation < model.maxFrequency then
                        model.uDilation + 1

                    else
                        model.uDilation
            }

        UDilationMinus ->
            { model
                | uDilation =
                    if model.uDilation > 0 then
                        model.uDilation - 1

                    else
                        model.uDilation
            }

        UShiftPlus ->
            { model
                | uArg =
                    model.uArg + model.uShiftScale * Basics.pi / 4
                , uShift = model.uShift + model.uShiftScale
            }

        UShiftMinus ->
            { model
                | uArg =
                    model.uArg - model.uShiftScale * Basics.pi / 4
                , uShift = model.uShift - model.uShiftScale
            }

        EditableScalePlus ->
            { model
                | editableScale =
                    if model.editableScale < model.maxAmplitude then
                        model.editableScale + 1

                    else
                        model.editableScale
            }

        EditableScaleMinus ->
            { model
                | editableScale =
                    if model.editableScale > -model.maxAmplitude then
                        model.editableScale - 1

                    else
                        model.editableScale
            }

        EditableDilationPlus ->
            { model
                | editableDilation =
                    if model.editableDilation < model.maxFrequency then
                        model.editableDilation + 1

                    else
                        model.editableDilation
            }

        EditableDilationMinus ->
            { model
                | editableDilation =
                    if model.editableDilation > -model.maxFrequency then
                        model.editableDilation - 1

                    else
                        model.editableDilation
            }

        VScalePlus ->
            { model
                | vScale =
                    if model.vScale < model.maxAmplitude then
                        model.vScale + 1

                    else
                        model.vScale
            }

        VScaleMinus ->
            { model
                | vScale =
                    if model.vScale > -model.maxAmplitude then
                        model.vScale - 1

                    else
                        model.vScale
            }

        VDilationPlus ->
            { model
                | vDilation =
                    if model.vDilation < model.maxFrequency then
                        model.vDilation + 1

                    else
                        model.vDilation
            }

        VDilationMinus ->
            { model | vDilation = model.vDilation - 1 }

        TrigCycleU ->
            { model | trigCycleU = cycleTrig model.trigCycleU }

        TrigCycleV ->
            { model | trigCycleV = cycleTrig model.trigCycleV }

        UTransforms ->
            { model | uTransform = cycleTransforms model.uTransform }

        UTransformsReverse ->
            { model | uTransform = cycleTransformsReverse model.uTransform }

        -- new stuff Lucas
        UTransforms1 ->
            { model | uTransform =  ScaleU }

        UTransforms2->
            { model | uTransform = URotate }

        UTransforms3 ->
            { model | uTransform =  ScaleX }

        UTransforms4->
            { model | uTransform = ScaleY }

        UTransforms5 ->
            { model | uTransform =  MakeTransparent }

        UTransforms6->
            { model | uTransform = MoveX }

        UTransforms7 ->
            { model | uTransform =  MoveY }

        UTransforms8->
            { model | uTransform = MoveCircle }

        UTransforms9->
            { model | uTransform = EditableXSin }


        {-
           MoveX ->
               { model | moveX = cycleFunZero model.moveX }

           MoveY ->
               { model | moveY = cycleFunZero model.moveY }

           MoveX1 ->
               { model | moveX1 = cycleFunZero model.moveX1 }

           MoveY1 ->
               { model | moveY1 = cycleFunZero model.moveY1 }
           TransformsFunctionChange ->
                  { model | transformFun = cycleFunZero model.transformFun }
        -}
        ButtonDown dir ->
            { model | currentButton = dir }

        MouseUp ->
            { model | currentButton = None }



-- make the Collage fit in VGA screen minus menu bars, for Chromebooks and iPads


eval f u v =
    case f of
        OneFun ->
            u

        UFun ->
            u

        VFun ->
            v


showFun f u v =
    case f of
        OneFun ->
            "u"

        UFun ->
            "u"

        VFun ->
            "v"


cycleFun f =
    case f of
        OneFun ->
            UFun

        UFun ->
            VFun

        VFun ->
            OneFun


cycleTrig f =
    case f of
        Sin ->
            Cos

        Cos ->
            Sin


textTrig f =
    case f of
        Sin ->
            "sin"

        Cos ->
            "cos"


evalTrig f u =
    case f of
        Sin ->
            sin u

        Cos ->
            cos u


cycleFunZero f =
    case f of
        ZeroFun ->
            UFunZero

        UFunZero ->
            NegUFun

        NegUFun ->
            VFunZero

        VFunZero ->
            NegVFun

        NegVFun ->
            ZeroFun


moveText mv =
    case mv of
        ZeroFun ->
            "u"

        UFunZero ->
            "u"

        NegUFun ->
            "-u"

        VFunZero ->
            "v"

        NegVFun ->
            "-v"



{-
   moveFun mv model =
       let
           u =
               model.u

           v =
               model.v
       in
       case mv of
           ZeroFun ->
               u

           UFunZero ->
               u

           NegUFun ->
               -u

           VFunZero ->
               v

           NegVFun ->
               -v
-}

cycleTransforms tr =
    case tr of
        ScaleU ->
            URotate

        URotate ->
            ScaleX

        ScaleX ->
            ScaleY

        ScaleY ->
            MakeTransparent

        MakeTransparent ->
            MoveX

        MoveX ->
            MoveY

        MoveY ->
            MoveCircle

        MoveCircle ->
            EditableXSin

        EditableXSin ->
            ScaleU


cycleTransformsReverse tr =
    case tr of
        URotate ->
            ScaleU

        ScaleX ->
            URotate

        ScaleY ->
            ScaleX

        MakeTransparent ->
            ScaleY

        MoveX ->
            MakeTransparent

        MoveY ->
            MoveX

        MoveCircle ->
            MoveY

        EditableXSin ->
            MoveCircle

        ScaleU ->
            EditableXSin


applyTransforms tr model =
    let
        u =
            model.u
    in
    case tr of
        ScaleU ->
            scale ((model.uSinGraph + model.uScale) / 10)

        MoveX ->
            move ( model.uCosGraph, 0 )

        MoveY ->
            move ( 0, model.uSinGraph )

        MoveCircle ->
            move ( model.uCosGraph, model.uSinGraph )

        URotate ->
            rotate (u / 10)

        ScaleX ->
            scaleX ((model.uCosGraph + model.uScale) / 10)

        ScaleY ->
            scaleY ((model.uSinGraph + model.uScale) / 10)

        MakeTransparent ->
            makeTransparent u

        EditableXSin ->
            move ( model.uCosGraph, 0 )


applyTransformsText tr =
    case tr of
        MoveX ->
            " move x "

        MoveY ->
            " move y "

        MoveCircle ->
            " move in a circle "

        ScaleU ->
            " scale "

        URotate ->
            " rotate "

        ScaleX ->
            " scaleX "

        ScaleY ->
            " scaleY "

        MakeTransparent ->
            " makeTransparent "

        EditableXSin ->
            " editable Y Sin "


applyTransformsYourCode model tr =
    case tr of
        MoveX ->
            "|> move (" ++ String.fromFloat model.uScale ++ "*cos(model.time) , 0)"

        MoveY ->
            "|> move (0 , " ++ String.fromFloat model.uScale ++ "*sin(model.time))"

        MoveCircle ->
            "|> move (" ++ String.fromFloat model.uScale ++ "*sin(" ++ cosinString model ++ ", " ++ String.fromFloat model.uScale ++ "*cos(" ++ cosinString model

        ScaleU ->
            "|> scale " ++ String.fromFloat model.uScale ++ "*sin(" ++ cosinString model

        URotate ->
            "|> rotate " ++ String.fromFloat model.uScale ++ "*sin(" ++ cosinString model

        ScaleX ->
            "|> scaleX " ++ String.fromFloat model.uScale ++ "*cos(" ++ cosinString model

        ScaleY ->
            "|> scaleY " ++ String.fromFloat model.uScale ++ "*sin(" ++ cosinString model

        MakeTransparent ->
            "|> makeTransparent " ++ String.fromFloat model.uScale ++ "*sin(" ++ cosinString model

        EditableXSin ->
            "|> move (" ++ String.fromFloat model.editableScale ++ "*cos(model.time) , " ++ "0" ++ ")"



-- change you app's state based on your new messages


numGraphPoints model =
    round 2505


curveX x =
    Basics.toFloat (round (clamp 0 12 (x ^ 2) / 4))


sinCurve model =
    let
        points =
            List.map2 (\x y -> ( x, y )) model.sinGraph (List.drop 1 model.sinGraph)
    in
    List.take (numGraphPoints model) (List.map (\( ( a, b, col1 ), ( c, d, col2 ) ) -> line ( a, b ) ( c, d ) |> outlined (solid 1) col1) points)


cosCurve model =
    let
        points =
            List.map2 (\x y -> ( x, y )) model.cosGraph (List.drop 1 model.cosGraph)
    in
    List.take (numGraphPoints model - 1) (List.map (\( ( a, b, col1 ), ( c, d, col2 ) ) -> line ( a, b ) ( c, d ) |> outlined (solid 1) col1) points)


cosinString model =
    let
        fraction = 
            if (model.uShift / 8 * 2) < 0 then
                showDigits 5 (model.uShift / 8 * 2)

            else
                "+" ++ showDigits 4 (model.uShift / 8 * 2)
    in
    showDigits 2 model.uDilation ++ "*model.time" ++ fraction ++ "*Pi)"


view model =
    let
        uScale =
            model.uScale

        u =
            model.u

        v =
            model.v

        uArg =
            model.uArg

        x1 =
            if model.uTransform == MakeTransparent then
                90

            else
                45

        notTrigCycleU =
            if model.trigCycleU == Sin then
                cos

            else
                sin

        tt str =
            str |> text |> serif |> italic |> size 10 |> filled titleColour

        x2 =
            if model.uTransform == MakeTransparent then
                116

            else
                81

        yourCodeGroup =
            group
                [ rect 190 110 |> filled grey |> addOutline (solid 2) black |> makeTransparent 1 |> move ( 100, 20 )
                , copiable "--Add these new definitions to your code" |> move ( 10, 60 )
                , copiable ("u = " ++ String.fromFloat model.uScale ++ "*" ++ textTrig model.trigCycleU ++ "(" ++ String.fromFloat model.uDilation ++ "*model.time+" ++ String.fromFloat model.uShift ++ ")") |> move ( 10, 50 )
                , copiable "mySquare = square 15" |> move ( 10, 30 )
                , copiable ("  |> outlined (solid 0.25) rgb (" ++ String.fromFloat model.rScale ++ "*" ++ showFun model.rFun u v ++ " " ++ String.fromFloat model.gScale ++ "*" ++ showFun model.gFun u v ++ " " ++ String.fromFloat model.bScale ++ "*" ++ showFun model.bFun u v ++ ")") |> move ( 40, 20 )
                , copiable ("  " ++ applyTransformsYourCode model model.uTransform) |> move ( 40, 10 )
                , copiable ("  |> move(" ++ moveText model.moveX1 ++ "," ++ moveText model.moveY1 ++ ")") |> move ( 40, 0 )
                , copiable "--Add the following code to your shapes:" |> move ( 10, -10 )
                , copiable "mySquare" |> move ( 20, -20 )
                ]
        yourTextGroup =
            group
                [ copiable2 "Select your modifier!" |> move ( -40, 5 )
                ]
        yourTextGroup2 =
            group
                [ copiable "Scale" |> move ( 90, -30)
                , copiable "Scale" |> move ( 90, -30)
                , copiable "Rotate" |> move ( 120, -30)
                , copiable "ScaleX" |> move ( 90,  -60)
                , copiable "ScaleY" |> move ( 120,  -60)
                , copiable "Move X" |> move ( 90, -90)
                , copiable "Transparent" |> move ( 120, -90)
                , copiable "Circle" |> move ( 90, -120)
                , copiable "Move Y" |> move ( 120, -120)
                , copiable "Y Sin" |> move ( 90, -150)
                ]

        shiftCenterGraphicsY number =
            let
                subtractor =
                    25
            in
                number - subtractor

        shiftCenterGraphicsX number =
            let
                subtractor =
                    -30
            in
                number - subtractor

        transformsGraphicsGroup =
            group
                [ rect 250 200 |> outlined (solid 1) red |> makeTransparent 0.25 |> move ( shiftCenterGraphicsX 80, shiftCenterGraphicsY 70 )
                , square 15 |> outlined (solid 1) (rgb model.r model.g model.b) |> applyTransforms model.uTransform model |> move ( shiftCenterGraphicsX 45, shiftCenterGraphicsY 60 )
                , group
                    [ text (applyTransformsText model.uTransform) |> centered |> size 14 |> filled black |> move ( shiftCenterGraphicsX 15, shiftCenterGraphicsY 90 )
                    , circle 8 |> filled (rgb 255 10 10) |> notifyTap UTransforms1 |> move ( shiftCenterGraphicsX 120, shiftCenterGraphicsY 80) |> notifyLeave (TransM (\m -> { m | transformsTransp1 = 0.25 })) |> notifyEnter (TransM (\m -> { m | transformsTransp1 = 1 })) |> makeTransparent model.transformsTransp1
                    , circle 8 |> filled (rgb 255 10 10) |> notifyTap UTransforms2 |> move ( shiftCenterGraphicsX 150, shiftCenterGraphicsY 80) |> notifyLeave (TransM (\m -> { m | transformsTransp2 = 0.25 })) |> notifyEnter (TransM (\m -> { m | transformsTransp2 = 1 })) |> makeTransparent model.transformsTransp2
                    , circle 8 |> filled (rgb 255 10 10) |> notifyTap UTransforms3 |> move ( shiftCenterGraphicsX 120, shiftCenterGraphicsY 50) |> notifyLeave (TransM (\m -> { m | transformsTransp3 = 0.25 })) |> notifyEnter (TransM (\m -> { m | transformsTransp3 = 1 })) |> makeTransparent model.transformsTransp3
                    , circle 8 |> filled (rgb 255 10 10) |> notifyTap UTransforms4 |> move ( shiftCenterGraphicsX 150, shiftCenterGraphicsY 50) |> notifyLeave (TransM (\m -> { m | transformsTransp4 = 0.25 })) |> notifyEnter (TransM (\m -> { m | transformsTransp4 = 1 })) |> makeTransparent model.transformsTransp4
                    , circle 8 |> filled (rgb 255 10 10) |> notifyTap UTransforms5 |> move ( shiftCenterGraphicsX 150, shiftCenterGraphicsY 20) |> notifyLeave (TransM (\m -> { m | transformsTransp5 = 0.25 })) |> notifyEnter (TransM (\m -> { m | transformsTransp5 = 1 })) |> makeTransparent model.transformsTransp5
                    , circle 8 |> filled (rgb 255 10 10) |> notifyTap UTransforms6 |> move ( shiftCenterGraphicsX 120, shiftCenterGraphicsY 20) |> notifyLeave (TransM (\m -> { m | transformsTransp6 = 0.25 })) |> notifyEnter (TransM (\m -> { m | transformsTransp6 = 1 })) |> makeTransparent model.transformsTransp6
                    , circle 8 |> filled (rgb 255 10 10) |> notifyTap UTransforms7 |> move ( shiftCenterGraphicsX 150, shiftCenterGraphicsY -10) |> notifyLeave (TransM (\m -> { m | transformsTransp7 = 0.25 })) |> notifyEnter (TransM (\m -> { m | transformsTransp7 = 1 })) |> makeTransparent model.transformsTransp7
                    , circle 8 |> filled (rgb 255 10 10) |> notifyTap UTransforms8 |> move ( shiftCenterGraphicsX 120, shiftCenterGraphicsY -10) |> notifyLeave (TransM (\m -> { m | transformsTransp8 = 0.25 })) |> notifyEnter (TransM (\m -> { m | transformsTransp8 = 1 })) |> makeTransparent model.transformsTransp8
                    , circle 8 |> filled (rgb 255 10 10) |> notifyTap UTransforms9 |> move ( shiftCenterGraphicsX 120, shiftCenterGraphicsY -40) |> notifyLeave (TransM (\m -> { m | transformsTransp9 = 0.25 })) |> notifyEnter (TransM (\m -> { m | transformsTransp9 = 1 })) |> makeTransparent model.transformsTransp9

                    --, text (moveText model.transformFun) |> size 10 |> filled black |> notifyTap TransformsFunctionChange |> move ( x1, 105 ) |> notifyLeave (TransM (\m -> { m | transformsNumTransp = 0.25 })) |> notifyEnter (TransM (\m -> { m | transformsNumTransp = 1 })) |> makeTransparent model.transformsNumTransp
                    ]
                    |> move ( 30, 50 )
                ]

        {-
           moveGraphicsY =
               group
                   [ rect 120 140 |> outlined (solid 1) red |> makeTransparent 0.25 |> move ( 10, -50 )
                   , square 15 |> filled (rgb model.r model.g model.b) |> move ( moveFun model.moveX model, moveFun model.moveY model ) |> move ( 10, -60 )
                   , text "|>" |> fixedwidth |> size 10 |> filled black |> move ( -40, 0 )
                   , text "move(" |> fixedwidth |> size 10 |> filled black |> move ( -25, 0 )
                   , text (moveText model.moveX) |> fixedwidth |> size 10 |> filled black |> notifyTap MoveX |> move ( 3, 0 ) |> notifyEnter (TransM (\m -> { m | moveTextX = 1 })) |> notifyLeave (TransM (\m -> { m | moveTextX = 0.25 })) |> makeTransparent model.moveTextX
                   , text "," |> fixedwidth |> size 10 |> filled black |> move ( 14, 0 )
                   , text (moveText model.moveY) |> fixedwidth |> size 10 |> filled black |> notifyTap MoveY |> move ( 21, 0 ) |> notifyEnter (TransM (\m -> { m | moveTextY = 1 })) |> notifyLeave (TransM (\m -> { m | moveTextY = 0.25 })) |> makeTransparent model.moveTextY
                   , text ")" |> fixedwidth |> size 10 |> filled black |> move ( 31, 0 )
                   ]

           moveGraphicsX =
               group
                   [ rect 120 140 |> outlined (solid 1) red |> makeTransparent 0.25 |> move ( 10, -220 )
                   , square 15 |> filled (rgb model.r model.g model.b) |> move ( moveFun model.moveX1 model, moveFun model.moveY1 model ) |> move ( 10, -230 )
                   , text "|>" |> fixedwidth |> size 10 |> filled black |> move ( -40, -170 )
                   , text "move(" |> fixedwidth |> size 10 |> filled black |> move ( -25, -170 )
                   , text (moveText model.moveX1) |> fixedwidth |> size 10 |> filled black |> notifyTap MoveX1 |> move ( 3, -170 ) |> notifyEnter (TransM (\m -> { m | moveTextX1 = 1 })) |> notifyLeave (TransM (\m -> { m | moveTextX1 = 0.25 })) |> makeTransparent model.moveTextX1
                   , text "," |> fixedwidth |> size 10 |> filled black |> move ( 14, -170 )
                   , text (moveText model.moveY1) |> fixedwidth |> size 10 |> filled black |> notifyTap MoveY1 |> move ( 21, -170 ) |> notifyEnter (TransM (\m -> { m | moveTextY1 = 1 })) |> notifyLeave (TransM (\m -> { m | moveTextY1 = 0.25 })) |> makeTransparent model.moveTextY1
                   , text ")" |> fixedwidth |> size 10 |> filled black |> move ( 31, -170 )
                   ]
        -}

        waveParamsLabel =
            text "Set your wave function parameters!" |> size 10 |> fixedwidth |> filled black

        waveParams =
            group
                [ text "Amplitude" |> size 11 |> filled black |> move ( 0, 0 )
                , text (String.fromFloat model.uScale) |> size 11 |> filled red |> move ( 58, 0 )
                , upArrow |> notifyTap UScalePlus |> move ( 61, 10 ) |> notifyMouseDown (ButtonDown AmplitudeUp) |> notifyMouseUp (ButtonDown None)
                , downArrow |> notifyTap UScaleMinus |> move ( 61, -5 ) |> notifyMouseDown (ButtonDown AmplitudeDown) |> notifyMouseUp (ButtonDown None)
                , text "Frequency" |> size 11 |> filled black |> move ( 80, 0 )
                , text (String.fromFloat model.uDilation) |> size 11 |> filled green |> move ( 140, 0 )
                , upArrow |> notifyTap UDilationPlus |> move ( 143, 10 ) |> notifyMouseDown (ButtonDown FrequencyUp) |> notifyMouseUp (ButtonDown None)
                , downArrow |> notifyTap UDilationMinus |> move ( 143, -5 ) |> notifyMouseDown (ButtonDown FrequencyDown) |> notifyMouseUp (ButtonDown None)
                , text "Shift" |> size 11 |> filled black |> move ( 160, 0 )
                , text (String.fromFloat model.uShift) |> size 11 |> filled blue |> move ( 188, 0 )
                , upArrow |> notifyTap UShiftPlus |> move ( 191, 10 ) |> notifyMouseDown (ButtonDown ShiftUp) |> notifyMouseUp (ButtonDown None)
                , downArrow |> notifyTap UShiftMinus |> move ( 191, -5 ) |> notifyMouseDown (ButtonDown ShiftDown) |> notifyMouseUp (ButtonDown None)

                --, polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, 12 ) ] |> filled red |> makeTransparent 0.25 |> notifyTap VScalePlus |> move ( 60, -5 ) |> rotate (degrees 0) |> notifyMouseDown (ButtonDown VUP) |> notifyMouseUp (ButtonDown None)
                --, polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, 12 ) ] |> filled red |> makeTransparent 0.25 |> notifyTap VDilationPlus |> move ( 95, -5 )
                --, polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, 12 ) ] |> filled red |> makeTransparent 0.25 |> notifyTap VScaleMinus |> rotate (degrees 180) |> move ( 60, -20 ) |> notifyMouseDown (ButtonDown VDown) |> notifyMouseUp (ButtonDown None)
                --, polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, 12 ) ] |> filled red |> makeTransparent 0.25 |> notifyTap VDilationMinus |> rotate (degrees 180) |> move ( 95, -20 )
                ]

        rgbGraphics =
            group
                [ rect 140 50 |> outlined (solid 1) red |> makeTransparent 0.25 |> move ( 43, -5 )
                , text "rgb " |> fixedwidth |> size 10 |> filled black |> move ( -25, 0 )
                , text ("(" ++ String.fromFloat model.rScale ++ "*" ++ showFun model.rFun u v ++ ")") |> fixedwidth |> size 10 |> filled black |> move ( -5, 0 ) |> notifyTap R |> notifyEnter (TransM (\m -> { m | rTransp = 1 })) |> notifyLeave (TransM (\m -> { m | rTransp = 0.25 })) |> makeTransparent model.rTransp
                , text ("(" ++ String.fromFloat model.gScale ++ "*" ++ showFun model.gFun u v ++ ")") |> fixedwidth |> size 10 |> filled black |> move ( 35, 0 ) |> notifyTap G |> notifyEnter (TransM (\m -> { m | gTransp = 1 })) |> notifyLeave (TransM (\m -> { m | gTransp = 0.25 })) |> makeTransparent model.gTransp
                , text ("(" ++ String.fromFloat model.bScale ++ "*" ++ showFun model.bFun u v ++ ")") |> fixedwidth |> size 10 |> filled black |> move ( 73, 0 ) |> notifyTap B |> notifyEnter (TransM (\m -> { m | bTransp = 1 })) |> notifyLeave (TransM (\m -> { m | bTransp = 0.25 })) |> makeTransparent model.bTransp
                , triangle 8 |> filled (rgb 255 10 10) |> notifyTap RScalePlus |> move ( 5, -10 ) |> rotate (degrees 90) |> notifyMouseDown (ButtonDown RedUp) |> notifyMouseUp (ButtonDown None)
                , triangle 8 |> filled (rgb 180 140 140) |> notifyTap RScaleMinus |> rotate (degrees -90) |> move ( 15, -10 ) |> notifyMouseDown (ButtonDown RedDown) |> notifyMouseUp (ButtonDown None)
                , triangle 8 |> filled (rgb 10 255 10) |> notifyTap GScalePlus |> rotate (degrees 90) |> move ( 43, -10 ) |> notifyMouseDown (ButtonDown GreenUp) |> notifyMouseUp (ButtonDown None)
                , triangle 8 |> filled (rgb 140 180 140) |> notifyTap GScaleMinus |> rotate (degrees -90) |> move ( 53, -10 ) |> notifyMouseDown (ButtonDown GreenDown) |> notifyMouseUp (ButtonDown None)
                , triangle 8 |> filled (rgb 10 10 255) |> notifyTap BScalePlus |> rotate (degrees 90) |> move ( 80, -10 ) |> notifyMouseDown (ButtonDown BlueUp) |> notifyMouseUp (ButtonDown None)
                , triangle 8 |> filled (rgb 140 140 180) |> notifyTap BScaleMinus |> rotate (degrees -90) |> move ( 90, -10 ) |> notifyMouseDown (ButtonDown BlueDown) |> notifyMouseUp (ButtonDown None)
                ]

        -- Circle that rotates in time with the sin & cosin waves
        circleGraphics =
            group
                [ line ( -50, 50 ) ( -50 + model.uScale * notTrigCycleU uArg, 50 + u ) |> outlined (solid 1) (rgb model.r model.g model.b) |> makeTransparent 0.25
                , line ( -50 + model.uScale * notTrigCycleU uArg, 50 + u ) ( 0, 50 + model.uSinGraph ) |> outlined (solid 1) (rgb model.r model.g model.b) |> makeTransparent 0.5
                , line ( -50 + model.uScale * notTrigCycleU uArg, 50 + u ) ( model.uCosGraph - 50, 0 ) |> outlined (solid 1) (rgb model.r model.g model.b) |> makeTransparent 0.5
                , circle 2 |> filled (rgb model.r model.g model.b) |> move ( 0, 50 + model.uSinGraph )
                , circle 2 |> filled (rgb model.r model.g model.b) |> move ( model.uCosGraph - 50, 0 )
                , circle (abs uScale) |> outlined (solid 1) black |> move ( -50, 50 )
                , circle 2 |> filled (rgb model.r model.g model.b) |> move ( -50 + model.uScale * notTrigCycleU uArg, 50 + u )
                ]

        titlesText =
            group
                [ tt "1. Modify your functions!" |> move ( -50, 175 )
                , tt "2. Choose a colour!" |> move ( 140, 125 )
                , tt "3. Apply Transforms!" |> move ( 140, 35 )
                , tt "4. Move it!" |> move ( -220, 5 )
                , text "--The move function below will be in 'Your Code!' " |> serif |> italic |> size 6 |> filled titleColour |> move ( -250, -5 )
                , tt "5. Your Code!" |> move ( 40, -70 )
                ]

        sinLabel =
            group
                [ text (showDigits 2 model.uScale) |> fixedwidth |> size 10 |> filled red |> move ( 0, 0 )
                , text ("*" ++ textTrig model.trigCycleU ++ "(") |> fixedwidth |> size 10 |> filled black |> move ( 13, 0 )
                , text (showDigits 2 model.uDilation) |> fixedwidth |> size 10 |> filled green |> move ( 43, 0 )
                , text "*model.time+" |> fixedwidth |> size 10 |> filled black |> move ( 55, 0 )
                , text (showDigits 5 (model.uShift / 8 * 2)) |> fixedwidth |> size 10 |> filled blue |> move ( 131, 0 )
                , text "*Pi)" |> fixedwidth |> size 10 |> filled black |> move ( 162, 0 )
                ] |> move ( -160, -80 )

        cosLabel =
            group
                [ text (showDigits 2 model.uScale) |> fixedwidth |> size 10 |> filled red |> move ( 0, 0 )
                , text "*cos(" |> fixedwidth |> size 10 |> filled black |> move ( 13, 0 )
                , text (showDigits 2 model.uDilation) |> fixedwidth |> size 10 |> filled green |> move ( 43, 0 )
                , text "*model.time+" |> fixedwidth |> size 10 |> filled black |> move ( 55, 0 )
                , text (showDigits 5 (model.uShift / 8 * 2)) |> fixedwidth |> size 10 |> filled blue |> move ( 131, 0 )
                , text "*Pi)" |> fixedwidth |> size 10 |> filled black |> move ( 162, 0 )
                ] |> rotate (degrees 90) |> move ( -110, -200 ) |> notifyTap (TransM (\m -> { m | trigCycleU = Cos }))
    in
    [ graphPaperCustom 10 1 (rgb 255 137 5) |> makeTransparent 0.25 -- axes and selected coordinate ticks
    , group
        [ rect 1000 0.5 |> filled brown
        , rect 0.5 1000 |> filled brown
        , group (sinCurve model) |> move ( 0, 50 )
        , group (cosCurve model) |> move ( -50, 0 )
        , trigGraphAxis model |> move ( -185, 70 )
        , circleGraphics
        ]
        |> move ( -120, -30 ) -- moves the wave display
    , titlesText |> makeTransparent 0
    , cosLabel |> move ( -127, 77 )
    , sinLabel |> move ( -45, 165 )
    , transformsGraphicsGroup |> move ( 0, -100 )

    --, moveGraphicsX |> move ( 180, 220 )
    --, moveGraphicsY |> move ( 60, 50 )
    --, rgbGraphics |> move ( 140, 90 )
    , waveParamsLabel |> move ( -220, 170 )
    , waveParams |> move ( -210, 135 )
    , yourCodeGroup |> move ( 40, 110 )
    , yourTextGroup |> move ( 80, 50)
    , yourTextGroup2 |> move ( 80, 50)
    ]


upArrow =
    polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, 12 ) ] |> filled (rgba 255 0 0 0.6)


downArrow =
    polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, -12 ) ] |> filled (rgba 255 0 0 0.6)


trigGraphAxis model =
    group
        [ rect 0.5 105 |> filled black |> move ( 185, -18 )
        , rect model.sinWaveLength 0.5 |> filled black |> move ( 185 + model.sinWaveLength / 2, -20 )

        -- Subtract 130 to account for the ratio of the screen and remove excess
        , rect 105 0.5 |> filled black |> move ( 132, -70 )
        , rect 0.5 model.cosWaveLength |> filled black |> move ( 135, -70 - model.cosWaveLength / 2 )
        ]


showDigits width x =
    "      " ++ String.fromFloat x |> String.right width


titleColour =
    rgba 200 0 0 0.95


copiable str =
    str |> text |> serif |> selectable |> fixedwidth |> size 6 |> filled black


copiable2 str =
    str |> text |> selectable |> fixedwidth |> size 9 |> filled black
