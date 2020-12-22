module Clipboard exposing (Copy)


type alias Copy msg =
    String -> Cmd msg
