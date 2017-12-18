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

import Pretty exposing ((|+), Doc)


{-| A flipped version of append useful in function chaining situations.
-}
flippend : Doc -> Doc -> Doc
flippend doc =
    flip Pretty.append doc


break : Doc -> Doc
break =
    Pretty.surround Pretty.line Pretty.line


eol =
    Pretty.char ';'


comma =
    Pretty.char ','


commaSpace =
    Pretty.string ", "


commaSoftline =
    Pretty.char ',' |+ Pretty.softline


maybeDoc : (a -> Doc) -> Maybe a -> Doc
maybeDoc pretty val =
    Maybe.map pretty val
        |> Maybe.withDefault Pretty.empty


nonEmptyDoc : (List a -> Doc) -> List a -> Doc
nonEmptyDoc pretty vals =
    case vals of
        [] ->
            Pretty.empty

        _ :: _ ->
            pretty vals


stringListToDoc : Doc -> List String -> Doc
stringListToDoc sep vals =
    Pretty.join sep (List.map Pretty.string vals)
