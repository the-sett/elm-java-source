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

import Pretty as Doc exposing ((|+), Doc)


{-| A flipped version of append useful in function chaining situations.
-}
flippend : Doc -> Doc -> Doc
flippend doc =
    flip Doc.append doc


break : Doc -> Doc
break =
    Doc.surround Doc.line Doc.line


eol =
    Doc.char ';'


comma =
    Doc.char ','


commaSpace =
    Doc.string ", "


commaSoftline =
    Doc.char ',' |+ Doc.softline


maybeDoc : (a -> Doc) -> Maybe a -> Doc
maybeDoc pretty val =
    Maybe.map pretty val
        |> Maybe.withDefault Doc.empty


nonEmptyDoc : (List a -> Doc) -> List a -> Doc
nonEmptyDoc pretty vals =
    case vals of
        [] ->
            Doc.empty

        _ :: _ ->
            pretty vals


stringListToDoc : Doc -> List String -> Doc
stringListToDoc sep vals =
    Doc.join sep (List.map Doc.string vals)
