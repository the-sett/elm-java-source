module Internal.DocUtils
    exposing
        ( break
        , eol
        , flippend
        , maybeDoc
        , nonEmptyDoc
        , stringListToDoc
        , comma
        , commaSpace
        , commaSoftline
        )

import Pretty exposing ((|+), Doc, empty, append, line, char, string, surround, softline, join)


{-| A flipped version of append useful in function chaining situations.
-}
flippend : Doc -> Doc -> Doc
flippend doc =
    flip append doc


break : Doc -> Doc
break =
    surround line line


eol =
    char ';'


comma =
    char ','


commaSpace =
    string ", "


commaSoftline =
    char ',' |+ softline


maybeDoc : (a -> Doc) -> Maybe a -> Doc
maybeDoc pretty val =
    Maybe.map pretty val
        |> Maybe.withDefault empty


nonEmptyDoc : (List a -> Doc) -> List a -> Doc
nonEmptyDoc pretty vals =
    case vals of
        [] ->
            empty

        _ :: _ ->
            pretty vals


stringListToDoc : Doc -> List String -> Doc
stringListToDoc sep vals =
    join sep (List.map string vals)
