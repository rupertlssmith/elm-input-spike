module TextBuffer exposing
    ( TextBuffer, empty, fromArray
    , breakLine, deleteCharAt, deleteCharBefore, insertCharAt
    , getLine
    , lastColumn, lastLine
    , Line, TagLineFn, length, refocus, rippleTo
    )

{-| An editing buffer.


# Buffers and ways to make one.

@docs TextBuffer, empty, fromArray, fromString, fromList


# Make changes to a buffers contents.

@docs breakLine, deleteCharAt, deleteCharBefore, insertCharAt


# Get the contents from a buffer.

@docs getLine


# Iterate over the buffer contents.

@docs indexedFoldl


# Query size and position information.

@docs lastColumn, lastLine, nextLine, previousLine, numLines, clampColumn
@docs isFirstLine, isFirstColumn, isLastLine, isLastColumn

-}

-- ( TextBuffer, fromString, fromList
-- , breakLine, deleteCharAfter, deleteCharBefore, insertCharAt
-- , range, charAt, contents
-- , indexedFoldl
-- , isFirstLine, isFirstColumn, isLastLine, isLastColumn
-- , lastColumn, lastLine, nextLine, previousLine, numLines, clampColumn
-- )

import Array exposing (Array)
import GapBuffer exposing (GapBuffer)
import Regex
import Set exposing (Set)



-- Buffers and ways to make one.


type alias TextBuffer tag ctx =
    { lines : GapBuffer (Line tag ctx) (GapBuffer Char Char)
    , ripples : Set Int
    }


empty : ctx -> TagLineFn tag ctx -> TextBuffer tag ctx
empty initialCtx tagLineFn =
    { lines = GapBuffer.empty untagLine (tagLine initialCtx tagLineFn)
    , ripples = Set.empty
    }



--
--
-- fromString : ctx -> TagLineFn tag ctx -> String -> TextBuffer tag ctx
-- fromString initialCtx tagLineFn source =
--     { lines =
--         newlineRegex
--             |> (\r -> Regex.split r source)
--             |> GapBuffer.fromList untagLine (tagLine initialCtx tagLineFn)
--     }
--
--
-- fromList : ctx -> TagLineFn tag ctx -> List String -> TextBuffer tag ctx
-- fromList initialCtx tagLineFn lines =
--     { lines = GapBuffer.fromList untagLine (tagLine initialCtx tagLineFn) lines }
--
--


fromArray : ctx -> TagLineFn tag ctx -> Array String -> TextBuffer tag ctx
fromArray initialCtx tagLineFn array =
    let
        arrayOfCharBuffers =
            Array.map stringToCharBuffer array

        ( _, _, rippledArray ) =
            Array.foldl
                (\charBuffer ( ctx, maybePrevLine, accum ) ->
                    let
                        line =
                            tagLine ctx tagLineFn maybePrevLine charBuffer
                    in
                    ( line.end, Just line, Array.push line accum )
                )
                ( initialCtx, Nothing, Array.empty )
                arrayOfCharBuffers
    in
    { lines = GapBuffer.fromArray untagLine (tagLine initialCtx tagLineFn) rippledArray
    , ripples = Set.empty
    }


stringToCharBuffer : String -> GapBuffer Char Char
stringToCharBuffer string =
    String.toList string |> GapBuffer.fromList identity (always identity)



-- The line model.


type alias Line tag ctx =
    { start : ctx
    , end : ctx
    , tagged : List ( tag, String )
    }


type alias TagLineFn tag ctx =
    GapBuffer Char Char -> ctx -> ( List ( tag, String ), ctx )


tagLine : ctx -> TagLineFn tag ctx -> Maybe (Line tag ctx) -> GapBuffer Char Char -> Line tag ctx
tagLine initialContext tagLineFn maybePrevLine charBuffer =
    let
        startCtx =
            case maybePrevLine of
                Just { end } ->
                    end

                Nothing ->
                    initialContext

        ( taggedStrings, endCtx ) =
            tagLineFn charBuffer startCtx
    in
    { start = startCtx
    , end = endCtx
    , tagged = taggedStrings
    }


untagLine : Line tag ctx -> GapBuffer Char Char
untagLine line =
    List.foldr
        (\( _, str ) accum ->
            List.foldr
                (::)
                accum
                (String.toList str)
        )
        []
        line.tagged
        |> GapBuffer.fromList identity (always identity)



-- Rippling


rippleTo : Int -> TextBuffer tag ctx -> TextBuffer tag ctx
rippleTo to buffer =
    let
        ( rippledBuffer, _, pendingRipples ) =
            Set.foldl
                (\from ( accum, marker, set ) ->
                    if (marker == -1) || from <= marker then
                        let
                            ( nextBuffer, outcome ) =
                                GapBuffer.ripple from
                                    to
                                    (\prevLine line -> prevLine.end /= line.start)
                                    accum
                        in
                        case outcome of
                            GapBuffer.Done ->
                                ( nextBuffer, marker, set )

                            GapBuffer.StoppedAt end ->
                                ( nextBuffer, max marker end, Set.insert end set )

                    else if from > to then
                        ( accum, marker, Set.insert from set )

                    else
                        ( accum, marker, set )
                )
                ( buffer.lines, -1, Set.empty )
                buffer.ripples
    in
    { buffer | lines = rippledBuffer, ripples = pendingRipples }


{-| Shift the buffer focues without changing the contents.
-}
refocus : Int -> Int -> TextBuffer tag ctx -> TextBuffer tag ctx
refocus row col buffer =
    let
        lines =
            GapBuffer.updateFocus row
                (\rowBuffer -> GapBuffer.updateFocus col identity rowBuffer)
                buffer.lines
    in
    { buffer | lines = lines }



-- Make changes to a buffers contents


setLineAt : String -> Int -> TextBuffer tag ctx -> TextBuffer tag ctx
setLineAt val row buffer =
    { buffer
        | lines = GapBuffer.setFocus row (stringToCharBuffer val) buffer.lines
        , ripples = Set.insert row buffer.ripples
    }


breakLine : Int -> Int -> TextBuffer tag ctx -> TextBuffer tag ctx
breakLine row col buffer =
    let
        lines =
            case GapBuffer.getFocus row buffer.lines of
                ( _, Nothing ) ->
                    buffer.lines

                ( focussedBuffer, Just rowBuffer ) ->
                    let
                        lineBeforeCursor =
                            GapBuffer.slice 0 col rowBuffer
                                |> GapBuffer.fromArray rowBuffer.toFocus rowBuffer.fromFocus

                        lineAfterCursor =
                            GapBuffer.slice col rowBuffer.length rowBuffer
                                |> GapBuffer.fromArray rowBuffer.toFocus rowBuffer.fromFocus
                    in
                    focussedBuffer
                        |> GapBuffer.setFocus row lineBeforeCursor
                        |> GapBuffer.insertAtFocus (row + 1) lineAfterCursor
    in
    { buffer
        | lines = lines
        , ripples = Set.insert row buffer.ripples
    }


insertCharAt : Char -> Int -> Int -> TextBuffer tag ctx -> TextBuffer tag ctx
insertCharAt char row col buffer =
    let
        lines =
            GapBuffer.updateFocus row
                (\rowBuffer -> GapBuffer.insertAtFocus col char rowBuffer)
                buffer.lines
    in
    { buffer
        | lines = lines
        , ripples = Set.insert row buffer.ripples
    }


insertStringAt : Char -> Int -> Int -> TextBuffer tag ctx -> TextBuffer tag ctx
insertStringAt char row col buffer =
    buffer


deleteCharBefore : Int -> Int -> TextBuffer tag ctx -> TextBuffer tag ctx
deleteCharBefore row col buffer =
    let
        ( lines, ripples ) =
            if isFirstColumn col && isFirstLine row then
                ( buffer.lines, buffer.ripples )

            else if isFirstColumn col then
                case GapBuffer.getFocus row buffer.lines of
                    ( _, Nothing ) ->
                        ( buffer.lines, buffer.ripples )

                    ( focussedBuffer, Just rowBuffer ) ->
                        ( focussedBuffer
                            |> GapBuffer.delete row
                            |> GapBuffer.updateFocus (row - 1)
                                (\prevRowBuffer ->
                                    Array.append
                                        (GapBuffer.slice 0 prevRowBuffer.length prevRowBuffer)
                                        (GapBuffer.slice 0 rowBuffer.length rowBuffer)
                                        |> GapBuffer.fromArray rowBuffer.toFocus rowBuffer.fromFocus
                                )
                        , Set.insert (row - 1) buffer.ripples
                        )

            else
                ( GapBuffer.updateFocus row
                    (\rowBuffer -> GapBuffer.delete (col - 1) rowBuffer)
                    buffer.lines
                , Set.insert row buffer.ripples
                )
    in
    { buffer
        | lines = lines
        , ripples = ripples
    }


deleteCharAt : Int -> Int -> TextBuffer tag ctx -> TextBuffer tag ctx
deleteCharAt row col buffer =
    let
        ( lines, ripples ) =
            if isLastColumn buffer row col && isLastLine buffer row then
                ( buffer.lines, buffer.ripples )

            else if isLastColumn buffer row col then
                case GapBuffer.getFocus (row + 1) buffer.lines of
                    ( _, Nothing ) ->
                        ( buffer.lines, buffer.ripples )

                    ( focussedBuffer, Just nextRowBuffer ) ->
                        ( focussedBuffer
                            |> GapBuffer.delete (row + 1)
                            |> GapBuffer.updateFocus row
                                (\rowBuffer ->
                                    Array.append
                                        (GapBuffer.slice 0 rowBuffer.length rowBuffer)
                                        (GapBuffer.slice 0 nextRowBuffer.length nextRowBuffer)
                                        |> GapBuffer.fromArray rowBuffer.toFocus rowBuffer.fromFocus
                                )
                        , Set.insert row buffer.ripples
                        )

            else
                ( GapBuffer.updateFocus row
                    (\rowBuffer -> GapBuffer.delete col rowBuffer)
                    buffer.lines
                , Set.insert row buffer.ripples
                )
    in
    { buffer
        | lines = lines
        , ripples = ripples
    }



--
--
--
-- Get the contents from a buffer.
--
--
-- range : Int -> Int -> Int -> Int -> (TextBuffer tag ctx) -> String
-- range r1 c1 r2 c2 buffer =
--     let
--         numberOfLines =
--             r2 - r1 + 1
--     in
--     buffer
--         |> toList
--         |> List.drop r1
--         |> List.take numberOfLines
--         |> List.indexedMap
--             (\i line ->
--                 if numberOfLines == 1 then
--                     line
--                         |> String.dropLeft c1
--                         |> String.left (c2 - c1 + 1)
--
--                 else if i == 0 then
--                     String.dropLeft c1 line
--
--                 else if i == numberOfLines - 1 then
--                     String.left (c2 + 1) line
--
--                 else
--                     line
--             )
--         |> String.join "\n"
--
--
-- charAt : Int -> Int -> TextBuffer tag ctx -> String
-- charAt line column buffer =
--     getLine line buffer
--         |> Maybe.withDefault ""
--         |> String.dropLeft column
--         |> String.left 1
--
--
-- contents : TextBuffer tag ctx -> String
-- contents buffer =
--     foldlLines
--         (\line ( isFirst, accum ) ->
--             ( False
--             , if isFirst then
--                 accum ++ line
--
--               else
--                 accum ++ "\n" ++ line
--             )
--         )
--         ( True, "" )
--         buffer
--         |> Tuple.second
--
--
-- Iterate over the buffer contents.


foldlSlice : (Int -> Line tag ctx -> acc -> acc) -> acc -> Int -> Int -> TextBuffer tag ctx -> acc
foldlSlice fn accum from to buffer =
    GapBuffer.foldlSlice fn accum from to buffer.lines



-- Query size and position information.


isFirstLine : Int -> Bool
isFirstLine line =
    line == 0


isLastLine : TextBuffer tag ctx -> Int -> Bool
isLastLine buffer line =
    line == lastLine buffer


isFirstColumn : Int -> Bool
isFirstColumn column =
    column == 0


isLastColumn : TextBuffer tag ctx -> Int -> Int -> Bool
isLastColumn buffer line column =
    column == lastColumn buffer line


lastLine : TextBuffer tag ctx -> Int
lastLine buffer =
    GapBuffer.length buffer.lines - 1


lastColumn : TextBuffer tag ctx -> Int -> Int
lastColumn buffer line =
    lineLength line buffer


previousLine : Int -> Int
previousLine line =
    (line - 1)
        |> max 0


nextLine : TextBuffer tag ctx -> Int -> Int
nextLine buffer line =
    (line + 1)
        |> min (lastLine buffer)


length : TextBuffer tag ctx -> Int
length buffer =
    GapBuffer.length buffer.lines


clampColumn : TextBuffer tag ctx -> Int -> Int -> Int
clampColumn buffer line column =
    column
        |> clamp 0 (lineLength line buffer)



-- Helpers
-- toIndexedList : (TextBuffer tag ctx) -> List ( Int, String )
-- toIndexedList buffer =
--     Array.toIndexedList buffer
--
--
-- get : Int -> (TextBuffer tag ctx) -> Maybe String
-- get line buffer =
--     Array.get line buffer
--
--
-- toList : (TextBuffer tag ctx) -> List String
-- toList buffer =
--     Array.toList buffer
--
--


getLine : Int -> TextBuffer tag ctx -> Maybe (Line tag ctx)
getLine lineNum buffer =
    GapBuffer.get lineNum buffer.lines


getRegion : Int -> Int -> Int -> Int -> TextBuffer tax ctx -> Maybe (List (Line tag ctx))
getRegion fromRow fromCol toRow toCol buffer =
    Nothing


deleteRegion : Int -> Int -> Int -> Int -> TextBuffer tax ctx -> TextBuffer tax ctx
deleteRegion fromRow fromCol toRow toCol buffer =
    buffer


lineLength : Int -> TextBuffer tag ctx -> Int
lineLength lineNum buffer =
    let
        llength line =
            line.tagged
                |> List.unzip
                |> Tuple.second
                |> List.foldl (String.length >> (+)) 0
    in
    getLine lineNum buffer
        |> Maybe.map llength
        |> Maybe.withDefault 0



-- allLines : (TextBuffer tag ctx) -> List String
-- allLines buffer =
--     Array.toList buffer
--
--


foldlLines : (Line tag ctx -> a -> a) -> a -> TextBuffer tag ctx -> a
foldlLines fn init buffer =
    GapBuffer.foldlSlice
        (\_ line acc -> fn line acc)
        init
        0
        (GapBuffer.length buffer.lines)
        buffer.lines



--
--
-- indexedMap : (Int -> String -> String) -> (TextBuffer tag ctx) -> (TextBuffer tag ctx)
-- indexedMap fn buffer =
--     Array.indexedMap fn buffer
--
--


newlineRegex =
    Regex.fromString "\\n"
        |> Maybe.withDefault Regex.never
