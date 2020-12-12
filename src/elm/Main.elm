module Main exposing (main)

import Array exposing (Array)
import Browser exposing (Document)
import Browser.Dom exposing (Viewport)
import Browser.Events
import Css
import Css.Global
import GapBuffer
import Html as H exposing (Attribute, Html)
import Html.Attributes as HA
import Html.Events as HE
import Html.Keyed as Keyed
import Html.Lazy
import Html.Styled
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Random exposing (Generator, Seed)
import Random.Array
import Regex exposing (Regex)
import Task exposing (Task)
import TextBuffer exposing (TextBuffer)
import Time exposing (Posix)


config =
    let
        fontSize =
            15

        lineHeightRatio =
            1.4
    in
    { fontSize = fontSize
    , lineHeightRatio = lineHeightRatio
    , lineHeight = (lineHeightRatio * fontSize) |> floor |> toFloat
    , lineLength = 120
    , numLines = 10000
    , blinkInterval = 400
    }


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { buffer : TextBuffer Tag Tag
    , top : Float
    , height : Float
    , controlCursor : Cursor
    , trackingCursor : Cursor
    , scrollRow : Int
    , targetCol : Int
    , linesPerPage : Int
    , startLine : Int
    , endLine : Int
    , bufferHeight : Float
    , bottomOffset : Float
    , blinker : Bool
    , lastActive : Posix
    , editKey : Int
    }


init _ =
    ( { buffer = TextBuffer.empty initialCtx tagLineFn
      , top = 0
      , height = 0
      , controlCursor = NoCursor
      , trackingCursor = NoCursor
      , scrollRow = 0
      , targetCol = 0
      , linesPerPage = 0
      , startLine = 0
      , endLine = 0
      , bufferHeight = 0.0
      , bottomOffset = 0.0
      , blinker = False
      , lastActive = Time.millisToPosix 0
      , editKey = 0
      }
    , Cmd.batch
        [ Task.perform RandomBuffer (randomBuffer config.lineLength config.numLines |> randomToTask)
        , initEditorSize
        , Browser.Dom.focus "editor-main" |> Task.attempt (always NoOp)
        ]
    )



-- Buffer setup.


type Tag
    = NormalText
    | QuotedText


initialCtx : Tag
initialCtx =
    NormalText


tagLineFn : TextBuffer.TagLineFn Tag Tag
tagLineFn charBuffer startCtx =
    let
        pushTag ( tagAccum, lineAccum, ctx ) =
            ( ( ctx, tagAccum |> List.reverse |> String.fromList ) :: lineAccum |> List.reverse, ctx )

        ( tagged, endCtx ) =
            GapBuffer.indexedFoldl
                (\_ char ( tagAccum, lineAccum, ctx ) ->
                    case ( char, ctx ) of
                        ( '"', NormalText ) ->
                            ( [ char ], ( ctx, tagAccum |> List.reverse |> String.fromList ) :: lineAccum, QuotedText )

                        ( '"', QuotedText ) ->
                            ( [], ( ctx, char :: tagAccum |> List.reverse |> String.fromList ) :: lineAccum, NormalText )

                        ( _, _ ) ->
                            ( char :: tagAccum, lineAccum, ctx )
                )
                ( [], [], startCtx )
                charBuffer
                |> pushTag
    in
    ( tagged, endCtx )



-- Events and event handling.


subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize (\_ _ -> Resize)
        , Time.every config.blinkInterval Blink
        ]


type Msg
    = RandomBuffer (TextBuffer Tag Tag)
    | ContentViewPort (Result Browser.Dom.Error Viewport)
    | Resize
    | EditorChange EditorChangeEvent
    | SelectionChange SelectionChangeEvent
    | Scroll ScrollEvent
    | StartSelecting
    | StopSelecting
    | MoveUp
    | MoveDown
    | MoveLeft
    | MoveRight
    | PageUp
    | PageDown
    | LineHome
    | LineEnd
    | FileHome
    | FileEnd
    | InsertChar Char
    | RemoveCharBefore
    | RemoveCharAfter
    | NewLine
    | Blink Posix
    | Activity Posix
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.controlCursor, msg ) of
        ( _, RandomBuffer buffer ) ->
            ( { model | buffer = buffer }, Cmd.none )

        ( _, ContentViewPort result ) ->
            case result of
                Ok viewport ->
                    ( model, Cmd.none )
                        |> andThen (establishViewport viewport)
                        |> andThen calcViewableRegion

                _ ->
                    ( model, Cmd.none )

        ( _, Resize ) ->
            ( model, initEditorSize )

        ( ActiveCursor pos, EditorChange change ) ->
            ( model, Cmd.none )
                |> andThen (editLine change.characterDataMutations model.controlCursor change.selection)
                |> andThen (moveCursorColBy 1 pos)
                |> andThen rippleBuffer
                |> andThen activity

        ( RegionCursor region, SelectionChange change ) ->
            (case change.selection of
                Range range ->
                    ( model, Cmd.none )
                        |> andThen (trackRangeFocus range region.selectionStart)

                Collapsed domPosition ->
                    ( model, Cmd.none )
                        |> andThen (trackCollapsedCursor domPosition)
                        |> andThen activity

                NoSelection ->
                    ( model, Cmd.none )
                        |> andThen (trackTo NoCursor)
            )
                |> andThen (updateCursorFromControlEvent change.isControl)
                |> andThen clipCursor

        ( currentCursor, SelectionChange change ) ->
            (case change.selection of
                Range range ->
                    ( model, Cmd.none )
                        |> andThen (trackNewRange range)

                Collapsed domPosition ->
                    ( model, Cmd.none )
                        |> andThen (trackCollapsedCursor domPosition)
                        |> andThen activity

                NoSelection ->
                    ( model, Cmd.none )
                        |> andThen (trackTo NoCursor)
            )
                |> andThen (updateCursorFromControlEvent change.isControl)
                |> andThen clipCursor

        ( _, Scroll scroll ) ->
            ( { model
                | top = scroll.scrollTop
                , scrollRow = scroll.scrollTop / config.lineHeight |> round
              }
            , Cmd.none
            )
                |> andThen calcViewableRegion
                |> andThen clipCursor

        ( _, StartSelecting ) ->
            ( model, Cmd.none )

        ( _, StopSelecting ) ->
            ( model, Cmd.none )

        ( ActiveCursor pos, MoveUp ) ->
            ( model, Cmd.none )
                |> andThen (moveCursorRowBy -1 pos)
                |> andThen scrollIfNecessary
                |> andThen calcViewableRegion
                |> andThen activity

        ( ActiveCursor pos, MoveDown ) ->
            ( model, Cmd.none )
                |> andThen (moveCursorRowBy 1 pos)
                |> andThen scrollIfNecessary
                |> andThen calcViewableRegion
                |> andThen rippleBuffer
                |> andThen activity

        ( ActiveCursor pos, MoveLeft ) ->
            let
                lastColPrevRow =
                    TextBuffer.lastColumn model.buffer (pos.row - 1)
            in
            ( model, Cmd.none )
                |> andThen (cursorLeft lastColPrevRow pos)
                |> andThen scrollIfNecessary
                |> andThen calcViewableRegion
                |> andThen activity

        ( ActiveCursor pos, MoveRight ) ->
            ( model, Cmd.none )
                |> andThen (cursorRight pos)
                |> andThen scrollIfNecessary
                |> andThen calcViewableRegion
                |> andThen rippleBuffer
                |> andThen activity

        ( ActiveCursor pos, PageUp ) ->
            ( model, Cmd.none )
                |> andThen (moveCursorRowBy -model.linesPerPage pos)
                |> andThen scrollIfNecessary
                |> andThen calcViewableRegion
                |> andThen activity

        ( ActiveCursor pos, PageDown ) ->
            ( model, Cmd.none )
                |> andThen (moveCursorRowBy model.linesPerPage pos)
                |> andThen scrollIfNecessary
                |> andThen calcViewableRegion
                |> andThen rippleBuffer
                |> andThen activity

        ( ActiveCursor pos, LineHome ) ->
            ( model, Cmd.none )
                |> andThen (moveCursorColBy -pos.col pos)
                |> andThen scrollIfNecessary
                |> andThen calcViewableRegion
                |> andThen activity

        ( ActiveCursor pos, LineEnd ) ->
            ( model, Cmd.none )
                |> andThen (moveCursorColBy (TextBuffer.lastColumn model.buffer pos.row - pos.col) pos)
                |> andThen scrollIfNecessary
                |> andThen calcViewableRegion
                |> andThen activity

        ( ActiveCursor pos, FileHome ) ->
            ( model, Cmd.none )
                |> andThen (ActiveCursor { row = 0, col = 0 } |> setCursor)
                |> andThen scrollIfNecessary
                |> andThen calcViewableRegion
                |> andThen activity

        ( ActiveCursor pos, FileEnd ) ->
            let
                lastRow =
                    TextBuffer.length model.buffer - 1
            in
            ( model, Cmd.none )
                |> andThen
                    (ActiveCursor
                        { row = lastRow
                        , col = TextBuffer.lastColumn model.buffer lastRow
                        }
                        |> setCursor
                    )
                |> andThen scrollIfNecessary
                |> andThen calcViewableRegion
                |> andThen rippleBuffer
                |> andThen activity

        ( ActiveCursor pos, InsertChar char ) ->
            ( model, Cmd.none )
                |> andThen (insertChar char pos)
                |> andThen (moveCursorColBy 1 pos)
                |> andThen rippleBuffer
                |> andThen activity

        ( ActiveCursor pos, RemoveCharBefore ) ->
            let
                lastColPrevRow =
                    TextBuffer.lastColumn model.buffer (pos.row - 1)
            in
            ( model, Cmd.none )
                |> andThen (backspace pos)
                |> andThen (cursorLeft lastColPrevRow pos)
                |> andThen scrollIfNecessary
                |> andThen calcViewableRegion
                |> andThen rippleBuffer
                |> andThen activity

        ( ActiveCursor pos, RemoveCharAfter ) ->
            ( model, Cmd.none )
                |> andThen (delete pos)
                |> andThen scrollIfNecessary
                |> andThen calcViewableRegion
                |> andThen rippleBuffer
                |> andThen activity

        ( ActiveCursor pos, NewLine ) ->
            ( model, Cmd.none )
                |> andThen (newline pos)
                |> andThen (moveCursorRowBy 1 pos)
                |> andThen (moveCursorColBy -pos.col pos)
                |> andThen scrollIfNecessary
                |> andThen calcViewableRegion
                |> andThen rippleBuffer
                |> andThen activity

        ( _, Blink posix ) ->
            if Time.posixToMillis posix - Time.posixToMillis model.lastActive > config.blinkInterval then
                ( { model | blinker = not model.blinker }, Cmd.none )

            else
                ( { model | blinker = True }, Cmd.none )

        ( _, Activity posix ) ->
            ( { model | lastActive = posix, blinker = True }, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


andThen : (model -> ( model, Cmd msg )) -> ( model, Cmd msg ) -> ( model, Cmd msg )
andThen fn ( model, cmd ) =
    let
        ( nextModel, nextCmd ) =
            fn model
    in
    ( nextModel, Cmd.batch [ cmd, nextCmd ] )


setCursor : Cursor -> Model -> ( Model, Cmd Msg )
setCursor cursor model =
    case cursor of
        NoCursor ->
            ( { model
                | controlCursor = cursor
                , targetCol = 0
              }
            , Cmd.none
            )

        ActiveCursor pos ->
            ( { model
                | controlCursor = cursor
                , targetCol = pos.col
              }
            , Cmd.none
            )

        RegionCursor _ ->
            ( { model
                | controlCursor = cursor
                , targetCol = 0
              }
            , Cmd.none
            )


moveCursorRowBy : Int -> RowCol -> Model -> ( Model, Cmd Msg )
moveCursorRowBy val pos model =
    let
        newRow =
            clamp
                0
                (TextBuffer.lastLine model.buffer)
                (pos.row + val)

        newCol =
            clamp 0
                (TextBuffer.lastColumn model.buffer newRow)
                (max pos.col model.targetCol)
    in
    ( { model | controlCursor = ActiveCursor { row = newRow, col = newCol } }
    , Cmd.none
    )


moveCursorColBy : Int -> RowCol -> Model -> ( Model, Cmd Msg )
moveCursorColBy val pos model =
    let
        newCol =
            clamp 0
                (TextBuffer.lastColumn model.buffer pos.row)
                (pos.col + val)
    in
    ( { model
        | controlCursor = ActiveCursor { row = pos.row, col = newCol }
        , targetCol = newCol
      }
    , Cmd.none
    )


cursorLeft : Int -> RowCol -> Model -> ( Model, Cmd Msg )
cursorLeft lastColPrevRow pos model =
    let
        left =
            pos.col - 1

        cursor =
            if left < 0 && pos.row <= 0 then
                { row = 0, col = 0 }

            else if left < 0 then
                { row = pos.row - 1, col = lastColPrevRow }

            else
                { row = pos.row, col = left }
    in
    ( { model | controlCursor = ActiveCursor cursor, targetCol = cursor.col }
    , Cmd.none
    )


cursorRight : RowCol -> Model -> ( Model, Cmd Msg )
cursorRight pos model =
    let
        right =
            pos.col + 1

        rightMost =
            TextBuffer.lastColumn model.buffer pos.row

        cursor =
            if right > rightMost && pos.row >= (TextBuffer.length model.buffer - 1) then
                pos

            else if right > rightMost then
                { row = pos.row + 1, col = 0 }

            else
                { row = pos.row, col = right }
    in
    ( { model | controlCursor = ActiveCursor cursor, targetCol = cursor.col }
    , Cmd.none
    )


scrollIfNecessary : Model -> ( Model, Cmd Msg )
scrollIfNecessary model =
    case model.controlCursor of
        NoCursor ->
            ( model, Cmd.none )

        _ ->
            let
                pos =
                    case model.controlCursor of
                        ActiveCursor scrollPos ->
                            scrollPos

                        RegionCursor { end } ->
                            end

                        _ ->
                            { row = 0, col = 0 }

                ( newScrollRow, scrollCmd ) =
                    if pos.row > (model.scrollRow + model.linesPerPage - 3) then
                        let
                            topRow =
                                min
                                    (TextBuffer.lastLine model.buffer - model.linesPerPage + 1)
                                    (pos.row - model.linesPerPage + 3)
                        in
                        ( topRow, scrollTo ((topRow |> toFloat) * config.lineHeight - model.bottomOffset) )

                    else if pos.row < (model.scrollRow + 2) then
                        let
                            topRow =
                                max
                                    0
                                    (pos.row - 2)
                        in
                        ( topRow, scrollTo ((topRow |> toFloat) * config.lineHeight) )

                    else
                        ( model.scrollRow, Cmd.none )
            in
            ( { model | scrollRow = newScrollRow }
            , scrollCmd
            )


establishViewport : Viewport -> Model -> ( Model, Cmd Msg )
establishViewport viewport model =
    ( { model
        | height = viewport.viewport.height
        , bottomOffset = bottomOffset viewport.viewport.height
        , linesPerPage = linesPerPage viewport.viewport.height
      }
    , Cmd.none
    )


calcViewableRegion : Model -> ( Model, Cmd Msg )
calcViewableRegion model =
    let
        pad =
            -- Ensure there is always 1 full page above and below for page up and down.
            -- -5
            model.linesPerPage + 1

        startLine =
            max 0
                ((model.top / config.lineHeight |> floor) - pad)

        endLine =
            ((model.top + model.height) / config.lineHeight |> floor) + pad

        bufferHeight =
            (TextBuffer.length model.buffer |> toFloat) * config.lineHeight
    in
    ( { model
        | startLine = startLine
        , endLine = endLine
        , bufferHeight = bufferHeight
      }
    , Cmd.none
    )


rippleBuffer : Model -> ( Model, Cmd Msg )
rippleBuffer model =
    ( { model | buffer = TextBuffer.rippleTo (model.scrollRow + model.linesPerPage) model.buffer }
    , Cmd.none
    )


editLine : List TextChange -> Cursor -> Selection -> Model -> ( Model, Cmd Msg )
editLine textChanges cursor selection model =
    case ( cursor, selection ) of
        ( ActiveCursor pos, Collapsed { offset } ) ->
            let
                modifyCharAt charOffset =
                    List.foldl
                        (\textChange accum ->
                            case
                                Tuple.mapSecond (String.toList >> List.drop (charOffset - 1) >> List.head) textChange
                            of
                                ( _ :: row :: _, Just char ) ->
                                    if row + model.startLine == pos.row then
                                        { accum
                                            | buffer =
                                                TextBuffer.insertCharAt char
                                                    pos.row
                                                    pos.col
                                                    accum.buffer
                                        }

                                    else
                                        accum

                                _ ->
                                    accum
                        )
                        model
                        textChanges

                editedModel =
                    modifyCharAt offset
            in
            ( { editedModel | editKey = model.editKey + 1 }, Cmd.none )

        ( NoCursor, _ ) ->
            ( model, Cmd.none )

        ( RegionCursor _, _ ) ->
            ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


insertChar : Char -> RowCol -> Model -> ( Model, Cmd Msg )
insertChar char pos model =
    ( { model | buffer = TextBuffer.insertCharAt char pos.row pos.col model.buffer }
    , Cmd.none
    )


newline : RowCol -> Model -> ( Model, Cmd Msg )
newline pos model =
    let
        buffer =
            TextBuffer.breakLine pos.row pos.col model.buffer
    in
    ( { model | buffer = buffer }
    , Cmd.none
    )


backspace : RowCol -> Model -> ( Model, Cmd Msg )
backspace pos model =
    ( { model | buffer = TextBuffer.deleteCharBefore pos.row pos.col model.buffer }
    , Cmd.none
    )


delete : RowCol -> Model -> ( Model, Cmd Msg )
delete pos model =
    ( { model | buffer = TextBuffer.deleteCharAt pos.row pos.col model.buffer }
    , Cmd.none
    )


activity : Model -> ( Model, Cmd Msg )
activity model =
    ( { model | blinker = True }
    , Time.now |> Task.perform Activity
    )


{-| The difference between the height and the height floored to line height.
-}
bottomOffset : Float -> Float
bottomOffset height =
    height
        - (((height / config.lineHeight) |> floor |> toFloat) * config.lineHeight)


linesPerPage : Float -> Int
linesPerPage height =
    (height / config.lineHeight) |> floor


initEditorSize : Cmd Msg
initEditorSize =
    Browser.Dom.getViewportOf "editor-main" |> Task.attempt ContentViewPort


scrollTo : Float -> Cmd Msg
scrollTo pos =
    Browser.Dom.setViewportOf "editor-main" 0.0 pos |> Task.attempt (always NoOp)


trackTo : Cursor -> Model -> ( Model, Cmd Msg )
trackTo cursor model =
    ( { model | trackingCursor = cursor }, Cmd.none )


trackCollapsedCursor : { offset : Int, node : Path } -> Model -> ( Model, Cmd Msg )
trackCollapsedCursor domPosition model =
    trackTo
        (domPositionToCursor
            model.startLine
            model.buffer
            domPosition.node
            domPosition.offset
        )
        model


trackNewRange :
    { anchorOffset : Int, anchorNode : Path, focusOffset : Int, focusNode : Path }
    -> Model
    -> ( Model, Cmd Msg )
trackNewRange range model =
    trackTo
        (newRangeCursor
            model.startLine
            model.buffer
            range.anchorNode
            range.anchorOffset
            range.focusNode
            range.focusOffset
        )
        model


trackRangeFocus :
    { a | focusOffset : Int, focusNode : Path }
    -> RowCol
    -> Model
    -> ( Model, Cmd Msg )
trackRangeFocus range selectionStart model =
    trackTo
        (moveFocusOfRangeCursor
            model.startLine
            model.buffer
            range.focusNode
            range.focusOffset
            selectionStart
        )
        model


clipCursor : Model -> ( Model, Cmd Msg )
clipCursor model =
    let
        cursor =
            case model.controlCursor of
                NoCursor ->
                    NoCursor

                ActiveCursor pos ->
                    ActiveCursor pos

                RegionCursor region ->
                    let
                        clippedStart =
                            if region.selectionStart.row < model.startLine then
                                { row = model.startLine, col = 0 }

                            else
                                region.selectionStart

                        clippedEnd =
                            if region.selectionEnd.row >= model.endLine then
                                { row = model.endLine, col = 0 }

                            else
                                region.selectionEnd
                    in
                    RegionCursor
                        { region
                            | start = clippedStart
                            , end = clippedEnd
                        }
    in
    ( { model | controlCursor = cursor }, Cmd.none )


updateCursorFromControlEvent : Bool -> Model -> ( Model, Cmd Msg )
updateCursorFromControlEvent isControl model =
    if isControl then
        setCursor model.trackingCursor model

    else
        ( model, Cmd.none )



-- Styling


global : List Css.Global.Snippet
global =
    [ Css.Global.html
        [ Css.pct 100 |> Css.height ]
    , Css.Global.body
        [ Css.pct 100 |> Css.height ]
    , Css.Global.id "editor-main"
        [ Css.position Css.relative
        , Css.fontFamily Css.monospace
        , Css.whiteSpace Css.pre
        , Css.overflowX Css.hidden
        , Css.overflowY Css.scroll
        , Css.pct 100 |> Css.width
        , Css.pct 100 |> Css.height
        ]
    , Css.Global.id "editor-main-inner"
        [ Css.displayFlex
        , Css.flexDirection Css.row
        , Css.outline Css.none
        ]
    , Css.Global.id "content-main"
        [ Css.position Css.relative
        , Css.property "flex" "1"
        , Css.property "user-select" "none"
        , Css.em 1 |> Css.marginLeft
        , Css.em 1 |> Css.marginRight
        , Css.outline3 (Css.px 0) Css.solid Css.transparent
        , Css.property "user-select" "text"
        , Css.property "-moz-user-select" "text"
        , Css.property "-webkit-user-select" "text"
        , Css.property "-ms-user-select" "text"

        --, Css.property "caret-color" "transparent"
        ]
    , Css.Global.class "content-line"
        [ Css.position Css.absolute
        , Css.px 0 |> Css.left
        , Css.px 0 |> Css.right
        , Css.px config.lineHeight |> Css.lineHeight
        , Css.px config.lineHeight |> Css.height

        --, Css.backgroundColor (Css.rgb 190 195 167)
        ]
    , Css.Global.class "cursors"
        [ Css.position Css.relative
        ]
    , Css.Global.class "cursor"
        [ Css.position Css.absolute
        , Css.px config.lineHeight |> Css.height
        , Css.borderLeft3 (Css.px 2.5) Css.solid (Css.rgb 90 95 167)
        ]
    , Css.Global.selector "::selection"
        [ Css.backgroundColor (Css.rgb 196 195 217)
        ]
    ]



-- View


view : Model -> Document Msg
view model =
    { title = "Input Spike"
    , body =
        [ Css.Global.global global |> Html.Styled.toUnstyled
        , editorView model
        ]
    }


editorView : Model -> Html Msg
editorView model =
    H.div
        [ HA.id "editor-main"
        , HE.on "scroll" scrollDecoder
        , HE.preventDefaultOn "keydown" keyDecoder
        ]
        [ H.div
            [ HA.id "editor-main-inner"
            , HA.tabindex 0
            ]
            [ viewContent model ]
        ]


viewCursors : Model -> Html Msg
viewCursors model =
    H.div
        [ HA.class "cursors"
        , if model.blinker then
            HA.style "visibility" "visible"

          else
            HA.style "visibility" "hidden"
        ]
        [ viewCursor model ]


viewCursor : Model -> Html Msg
viewCursor model =
    case model.controlCursor of
        ActiveCursor pos ->
            let
                top =
                    String.fromFloat
                        (toFloat pos.row * config.lineHeight)
                        ++ "px"

                left =
                    String.fromInt pos.col ++ "ch"
            in
            H.div
                [ HA.class "cursor"
                , HA.style "top" top
                , HA.style "left" left
                ]
                [ H.text "" ]

        _ ->
            H.div [] []


viewContent : Model -> Html Msg
viewContent model =
    let
        cursor =
            model.controlCursor
    in
    H.div
        [ HA.id "content-main"
        , HA.style "height" (String.fromFloat model.bufferHeight ++ "px")
        , HA.contenteditable True
        ]
        [ viewCursors model
        , H.node "elm-editable"
            [ HE.on "textchange" editorChangeDecoder
            , HE.on "selectionchange" selectionChangeDecoder
            , HA.attribute "spellcheck" "false"
            , HA.attribute "autocorrect" "off"
            , HA.attribute "autocapitalize" "off"
            ]
            [ keyedViewLines model
            , H.node "selection-handler"
                [ cursorToSelection model.controlCursor model.startLine model.buffer
                    |> selectionEncoder model.startLine
                    |> HA.property "selection"
                ]
                []
            ]
        ]


keyedViewLines : Model -> Html Msg
keyedViewLines model =
    List.range model.startLine model.endLine
        |> List.foldr
            (\idx accum ->
                case TextBuffer.getLine idx model.buffer of
                    Nothing ->
                        accum

                    Just row ->
                        case model.controlCursor of
                            ActiveCursor pos ->
                                if pos.row == idx then
                                    ( "edit-" ++ String.fromInt model.editKey, viewLine idx row ) :: accum

                                else
                                    ( String.fromInt idx, Html.Lazy.lazy2 viewLine idx row ) :: accum

                            _ ->
                                ( String.fromInt idx, Html.Lazy.lazy2 viewLine idx row ) :: accum
            )
            []
        |> Keyed.node "div" []


viewLine : Int -> TextBuffer.Line Tag Tag -> Html Msg
viewLine row line =
    let
        content =
            List.map
                (\( tag, str ) ->
                    case tag of
                        NormalText ->
                            H.span [ HA.style "color" "black" ] [ H.text str ]

                        QuotedText ->
                            H.span [ HA.style "color" "green" ] [ H.text str ]
                )
                line.tagged
    in
    H.div
        [ HA.class "content-line"
        , HA.style "top" (String.fromFloat (toFloat row * config.lineHeight) ++ "px")
        ]
        content



-- Cursor model.
-- This is actually a state machine for the whole editor isn't it?


type Cursor
    = NoCursor
    | ActiveCursor RowCol
    | RegionCursor
        { -- The control cursor, always clipped to the virtual scroll window.
          start : RowCol
        , end : RowCol

        -- The selection within the text model.
        , selectionStart : RowCol
        , selectionEnd : RowCol
        }


type alias RowCol =
    { row : Int
    , col : Int
    }



-- Browser selections.


type Selection
    = NoSelection
    | Collapsed
        { offset : Int
        , node : Path
        }
    | Range
        { anchorOffset : Int
        , anchorNode : Path
        , focusOffset : Int
        , focusNode : Path
        }


type alias Path =
    List Int


selectionDecoder : Decode.Decoder Selection
selectionDecoder =
    let
        range aNode aOffset fNode fOffset =
            Range
                { anchorOffset = aOffset
                , anchorNode = aNode
                , focusOffset = fOffset
                , focusNode = fNode
                }

        collapsed fNode fOffset =
            Collapsed
                { offset = fOffset
                , node = fNode
                }
    in
    Decode.at [ "selection" ] Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "collapsed" ->
                        Decode.succeed collapsed
                            |> andMap (Decode.at [ "node" ] (Decode.list Decode.int))
                            |> andMap (Decode.at [ "offset" ] Decode.int)

                    "range" ->
                        Decode.succeed range
                            |> andMap (Decode.at [ "anchorNode" ] (Decode.list Decode.int))
                            |> andMap (Decode.at [ "anchorOffset" ] Decode.int)
                            |> andMap (Decode.at [ "focusNode" ] (Decode.list Decode.int))
                            |> andMap (Decode.at [ "focusOffset" ] Decode.int)

                    _ ->
                        Decode.succeed NoSelection
            )


selectionEncoder : Int -> Selection -> Encode.Value
selectionEncoder startLine sel =
    case sel of
        NoSelection ->
            [ ( "selection", Encode.string "noselection" )
            , ( "startLine", Encode.int startLine )
            ]
                |> Encode.object

        Collapsed val ->
            [ ( "selection", Encode.string "collapsed" )
            , ( "startLine", Encode.int startLine )
            , ( "node", Encode.list Encode.int val.node )
            , ( "offset", Encode.int val.offset )
            ]
                |> Encode.object

        Range val ->
            [ ( "selection", Encode.string "range" )
            , ( "startLine", Encode.int startLine )
            , ( "anchorNode", Encode.list Encode.int val.anchorNode )
            , ( "anchorOffset", Encode.int val.anchorOffset )
            , ( "focusNode", Encode.list Encode.int val.focusNode )
            , ( "focusOffset", Encode.int val.focusOffset )
            ]
                |> Encode.object



-- Selection and cursor conversion.


domPositionToCursor : Int -> TextBuffer ctx tag -> Path -> Int -> Cursor
domPositionToCursor startLine buffer node offset =
    case node of
        _ :: row :: child :: _ ->
            let
                col =
                    TextBuffer.getLine row buffer
                        |> Maybe.map (\line -> pathOffsetToCol child offset line.tagged)
                        |> Maybe.withDefault 0
            in
            ActiveCursor { row = row + startLine, col = col }

        _ ->
            NoCursor


newRangeCursor : Int -> TextBuffer ctx tag -> Path -> Int -> Path -> Int -> Cursor
newRangeCursor startLine buffer anchorNode anchorOffset focusNode focusOffset =
    case ( anchorNode, focusNode ) of
        ( _ :: anchorRow :: anchorChild :: _, _ :: focusRow :: focusChild :: _ ) ->
            let
                anchorCol =
                    TextBuffer.getLine anchorRow buffer
                        |> Maybe.map (\line -> pathOffsetToCol anchorChild anchorOffset line.tagged)
                        |> Maybe.withDefault 0

                focusCol =
                    TextBuffer.getLine focusRow buffer
                        |> Maybe.map (\line -> pathOffsetToCol focusChild focusOffset line.tagged)
                        |> Maybe.withDefault 0
            in
            RegionCursor
                { start = { row = anchorRow + startLine, col = anchorCol }
                , end = { row = focusRow + startLine, col = focusCol }
                , selectionStart = { row = anchorRow + startLine, col = anchorCol }
                , selectionEnd = { row = focusRow + startLine, col = focusCol }
                }

        _ ->
            NoCursor


moveFocusOfRangeCursor : Int -> TextBuffer ctx tag -> Path -> Int -> RowCol -> Cursor
moveFocusOfRangeCursor startLine buffer focusNode focusOffset selectionStart =
    case focusNode of
        _ :: focusRow :: focusChild :: _ ->
            let
                focusCol =
                    TextBuffer.getLine focusRow buffer
                        |> Maybe.map (\line -> pathOffsetToCol focusChild focusOffset line.tagged)
                        |> Maybe.withDefault 0

                clippedStart =
                    if selectionStart.row < startLine then
                        { row = startLine, col = 0 }

                    else
                        selectionStart
            in
            RegionCursor
                { start = clippedStart
                , end = { row = focusRow + startLine, col = focusCol }
                , selectionStart = selectionStart
                , selectionEnd = { row = focusRow + startLine, col = focusCol }
                }

        _ ->
            NoCursor


pathOffsetToCol : Int -> Int -> List ( tag, String ) -> Int
pathOffsetToCol child offset line =
    case ( child, line ) of
        ( 0, _ ) ->
            offset

        ( _, [] ) ->
            offset

        ( _, tl :: tls ) ->
            pathOffsetToCol (child - 1) (offset + String.length (Tuple.second tl)) tls


cursorToSelection : Cursor -> Int -> TextBuffer ctx tag -> Selection
cursorToSelection cursor startLine buffer =
    case cursor of
        NoCursor ->
            NoSelection

        ActiveCursor pos ->
            let
                sel =
                    TextBuffer.getLine pos.row buffer
                        |> Maybe.map (fullPathOffset (pos.row - startLine) pos.col)
                        |> Maybe.map (\( node, offset ) -> Collapsed { node = node, offset = offset })
                        |> Maybe.withDefault NoSelection
            in
            sel

        RegionCursor { start, end } ->
            let
                maybeStartPathOffset =
                    TextBuffer.getLine start.row buffer
                        |> Maybe.map (fullPathOffset (start.row - startLine) start.col)

                maybeEndPathOffset =
                    TextBuffer.getLine end.row buffer
                        |> Maybe.map (fullPathOffset (end.row - startLine) end.col)

                sel =
                    case ( maybeStartPathOffset, maybeEndPathOffset ) of
                        ( Just ( anchorNode, anchorOffset ), Just ( focusNode, focusOffset ) ) ->
                            Range
                                { anchorNode = anchorNode
                                , anchorOffset = anchorOffset
                                , focusNode = focusNode
                                , focusOffset = focusOffset
                                }

                        _ ->
                            NoSelection
            in
            sel


fullPathOffset : Int -> Int -> TextBuffer.Line tag ctx -> ( Path, Int )
fullPathOffset row col line =
    let
        linePath =
            [ 0, row ]

        cursorPath =
            line
                |> lineToPathOffset col
                |> Tuple.mapFirst (List.append linePath)
    in
    cursorPath


lineToPathOffset : Int -> TextBuffer.Line tag ctx -> ( Path, Int )
lineToPathOffset col line =
    let
        { path, offset } =
            lineToPathOffsetInner
                line.tagged
                { path = 0
                , offset = 0
                , rem = col
                }

        lineToPathOffsetInner taggedStrings accum =
            case taggedStrings of
                [] ->
                    accum

                ( _, string ) :: moreTaggedStrings ->
                    let
                        strlen =
                            String.length string
                    in
                    if accum.rem <= strlen then
                        { path = accum.path, offset = accum.rem, rem = 0 }

                    else
                        lineToPathOffsetInner moreTaggedStrings
                            { path = accum.path + 1, offset = 0, rem = accum.rem - strlen }
    in
    ( [ path, 0 ], offset )



-- Selection change events.


type alias SelectionChangeEvent =
    { selection : Selection
    , isControl : Bool
    , timestamp : Int
    }


selectionChangeDecoder : Decode.Decoder Msg
selectionChangeDecoder =
    Decode.succeed SelectionChangeEvent
        |> andMap (Decode.at [ "detail", "selection" ] selectionDecoder)
        |> andMap (Decode.at [ "detail", "ctrlEvent" ] Decode.bool)
        |> andMap (Decode.at [ "detail", "timestamp" ] Decode.int)
        |> Decode.map SelectionChange



-- Editor mutation events.


type alias EditorChangeEvent =
    { selection : Selection
    , characterDataMutations : List TextChange
    , timestamp : Int
    , isComposing : Bool
    }


type alias TextChange =
    ( Path, String )


editorChangeDecoder : Decode.Decoder Msg
editorChangeDecoder =
    Decode.succeed EditorChangeEvent
        |> andMap (Decode.at [ "detail", "selection" ] selectionDecoder)
        |> andMap (Decode.at [ "detail", "characterDataMutations" ] characterDataMutationsDecoder)
        |> andMap (Decode.at [ "detail", "timestamp" ] Decode.int)
        |> andMap (Decode.at [ "detail", "isComposing" ] (Decode.oneOf [ Decode.bool, Decode.succeed False ]))
        |> Decode.map EditorChange


characterDataMutationsDecoder : Decode.Decoder (List TextChange)
characterDataMutationsDecoder =
    Decode.list
        (Decode.map2 Tuple.pair
            (Decode.field "path" (Decode.list Decode.int))
            (Decode.field "text" Decode.string)
        )



-- Keyboard events.


type alias KeyEvent =
    { keyCode : Int
    , key : String
    , altKey : Bool
    , metaKey : Bool
    , ctrlKey : Bool
    , shiftKey : Bool
    , isComposing : Bool
    }


keyDecoder : Decoder ( Msg, Bool )
keyDecoder =
    Decode.succeed KeyEvent
        |> andMap (Decode.field "keyCode" Decode.int)
        |> andMap (Decode.field "key" Decode.string)
        |> andMap (Decode.field "altKey" Decode.bool)
        |> andMap (Decode.field "metaKey" Decode.bool)
        |> andMap (Decode.field "ctrlKey" Decode.bool)
        |> andMap (Decode.field "shiftKey" Decode.bool)
        |> andMap (Decode.oneOf [ Decode.field "isComposing" Decode.bool, Decode.succeed False ])
        |> Decode.andThen keyToMsg


keyToMsg : KeyEvent -> Decoder ( Msg, Bool )
keyToMsg keyEvent =
    case keyEvent.key of
        "ArrowUp" ->
            Decode.succeed ( MoveUp, True )

        "ArrowDown" ->
            Decode.succeed ( MoveDown, True )

        "ArrowLeft" ->
            Decode.succeed ( MoveLeft, True )

        "ArrowRight" ->
            Decode.succeed ( MoveRight, True )

        "PageUp" ->
            Decode.succeed ( PageUp, True )

        "PageDown" ->
            Decode.succeed ( PageDown, True )

        "Backspace" ->
            Decode.succeed ( RemoveCharBefore, True )

        "Delete" ->
            Decode.succeed ( RemoveCharAfter, True )

        "Enter" ->
            Decode.succeed ( NewLine, True )

        "Home" ->
            if keyEvent.ctrlKey then
                Decode.succeed ( FileHome, True )

            else
                Decode.succeed ( LineHome, True )

        "End" ->
            if keyEvent.ctrlKey then
                Decode.succeed ( FileEnd, True )

            else
                Decode.succeed ( LineEnd, True )

        _ ->
            Decode.succeed ( NoOp, False )



-- Editor paste events.


type alias PasteEvent =
    { text : String
    , html : String
    }


pasteWithDataDecoder : Decoder PasteEvent
pasteWithDataDecoder =
    Decode.succeed PasteEvent
        |> andMap (Decode.at [ "detail", "text" ] Decode.string)
        |> andMap (Decode.at [ "detail", "html" ] Decode.string)



-- Scroll events


type alias ScrollEvent =
    { scrollTop : Float
    , scrollHeight : Float
    , scrollLeft : Float
    , scrollWidth : Float
    }


scrollDecoder : Decoder Msg
scrollDecoder =
    Decode.succeed ScrollEvent
        |> andMap (Decode.at [ "target", "scrollTop" ] Decode.float)
        |> andMap (Decode.at [ "target", "scrollHeight" ] Decode.float)
        |> andMap (Decode.at [ "target", "scrollLeft" ] Decode.float)
        |> andMap (Decode.at [ "target", "scrollWidth" ] Decode.float)
        |> Decode.map Scroll



-- Random buffer initialization.


randomBuffer : Int -> Int -> Generator (TextBuffer Tag Tag)
randomBuffer width length =
    let
        regex =
            Regex.fromString "(\\b[^\\s]+\\b)"
                |> Maybe.withDefault Regex.never

        wordList =
            Regex.find regex lorumIpsum
                |> List.map (.match >> String.toLower)
                |> Array.fromList

        wordGenerator =
            Random.Array.sample wordList

        line curLength curLine generator =
            if curLength >= width then
                String.concat curLine
                    |> Random.constant

            else
                generator
                    |> Random.andThen
                        (\randomWord ->
                            case randomWord of
                                Nothing ->
                                    -- This should not happen.
                                    line curLength curLine generator

                                Just val ->
                                    line (curLength + String.length val + 1)
                                        ((val ++ " ") :: curLine)
                                        generator
                        )
    in
    line 0 [] wordGenerator
        |> Random.Array.array length
        |> Random.map (TextBuffer.fromArray initialCtx tagLineFn)


lorumIpsum : String
lorumIpsum =
    """
  Lorem ipsum dolor sit amet, consectetur adipiscing elit. Cras ut feugiat orci. Ut cursus malesuada nunc id tempor. Nam dignissim luctus mi ac vestibulum. Fusce fermentum purus quis rutrum facilisis. Sed ut justo ac nulla ornare dictum. Ut condimentum pellentesque volutpat. Aliquam sapien eros, ornare eget nisi porta, mattis lobortis mauris. In hac habitasse platea dictumst. Aliquam scelerisque risus sed luctus accumsan.

Mauris posuere pellentesque urna, in consectetur enim tempor volutpat. Nulla convallis, turpis nec convallis eleifend, nisi elit vulputate nibh, nec cursus tellus purus eu mauris. Nulla facilisi. Ut placerat vulputate pharetra. Etiam libero est, eleifend quis semper ac, fringilla vitae arcu. Quisque ut cursus leo. Suspendisse augue tortor, venenatis at ex sit amet, fringilla malesuada ligula. Phasellus nulla nibh, mollis ut vulputate quis, congue vitae mi. Nunc porta, ex quis luctus scelerisque, eros mauris placerat mauris, vitae finibus leo nulla in lacus. Donec ac ex leo. Aliquam ut quam tincidunt, maximus neque et, hendrerit magna.

Integer elementum leo lacinia risus pharetra, sit amet condimentum felis porta. Phasellus sollicitudin mauris at risus semper, in facilisis ex dictum. Vestibulum tincidunt eros a vehicula dignissim. Quisque a ex et arcu bibendum congue. Praesent gravida nulla metus, sed luctus justo fermentum et. Nullam scelerisque, felis tempor placerat eleifend, neque risus dictum nisi, eget ullamcorper massa mauris non metus. Nam rhoncus mollis justo, eu luctus arcu pharetra et. Aenean auctor et massa tempus consectetur. Duis tempus nunc volutpat dolor pellentesque, non imperdiet purus sagittis. Sed ultricies neque vel condimentum tincidunt. Suspendisse ornare sodales risus, sed tincidunt tortor rutrum sed. Suspendisse pellentesque quis quam vel elementum.

Sed blandit orci ut lectus efficitur tempor. Maecenas vitae risus sodales leo fringilla posuere. Sed facilisis magna non eros porttitor, a molestie neque ultricies. Nulla ac sapien lacus. Aliquam erat volutpat. Etiam volutpat sem mauris, vitae luctus neque imperdiet non. Ut condimentum eget ipsum lobortis eleifend. Ut dignissim laoreet fringilla. Ut sit amet metus pharetra, malesuada diam vel, convallis ipsum.

Integer ac pellentesque turpis, id placerat libero. Fusce commodo mauris vitae augue laoreet, id dignissim enim placerat. Suspendisse et tellus semper, dictum nibh quis, tempus est. Etiam sagittis non lectus eu dapibus. Nullam metus nunc, lacinia ut varius et, commodo quis metus. In vel efficitur nisl. Mauris ac mi sed dolor scelerisque vulputate. In gravida urna ut tempus tempor. Quisque pulvinar velit ac lacus gravida vulputate. Integer ante odio, ultricies a posuere ut, sollicitudin ut risus.

Nullam volutpat consequat metus ac gravida. Curabitur iaculis nibh leo, non lacinia velit porta vitae. Aliquam convallis libero sed quam pharetra, eget cursus ex sagittis. Donec sodales in libero et finibus. Nunc rhoncus eleifend odio maximus sollicitudin. Fusce euismod erat quis enim cursus, eget imperdiet lectus rhoncus. Integer aliquet, nunc nec posuere condimentum, ex mauris fringilla urna, sit amet fermentum neque risus eu felis. Suspendisse tortor nibh, commodo et varius a, pulvinar vel urna. Nam porta aliquet egestas. Nam in fringilla ipsum. Praesent gravida nisl nec arcu pretium, pharetra vestibulum dolor placerat. Nullam rutrum in dolor ac mollis. Duis ornare laoreet enim.
  """



-- Helpers


randomToTask : Generator a -> Task x a
randomToTask generator =
    Time.now
        |> Task.map (Tuple.first << Random.step generator << Random.initialSeed << Time.posixToMillis)


andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap =
    Decode.map2 (|>)
