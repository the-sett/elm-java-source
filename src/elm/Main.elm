port module Main exposing (..)

import Html exposing (Html, div, text, pre)
import Doc exposing ((|+), Doc)
import Maybe


main : Html Never
main =
    div []
        [ pre [] [ text doc ] ]


doc : String
doc =
    jFileToDoc javaExample
        |> Doc.toString



-- Reusable Doc functions


break : Doc -> Doc
break =
    Doc.surround Doc.hardline Doc.hardline


eol =
    Doc.char ';'


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


{-| A flipped version of append useful in function chaining situations.
-}
flippend : Doc -> Doc -> Doc
flippend doc =
    flip Doc.append doc



-- Java model


type alias JavaFile =
    { header : Maybe String
    , package : String
    , imports : List String
    , classes : List Class
    }


type alias Class =
    { classComment : String
    , name : String
    , extends : Maybe String
    , implements : List String
    , members : List Member
    }


type alias Method =
    { name : String
    , returnType : String
    , args : List ( String, String )
    , body : List Statement
    }


type alias Field =
    { name : String
    , fieldType : String
    , initialValue : Maybe String
    }


type Member
    = MClass Class
    | MField Field
    | MMethod Method


type Statement
    = Statement String



-- Java Examples


javaExample : JavaFile
javaExample =
    { header = Just "blah"
    , package = "com.thesett.example"
    , imports = [ "org.springframework.core", "java.util.list" ]
    , classes =
        [ { classComment = "Example"
          , name = "Example"
          , extends = (Just "BaseClass")
          , implements = [ "Serializable" ]
          , members =
                [ MField { name = "test", fieldType = "int", initialValue = Nothing }
                , MMethod
                    { name = "main"
                    , returnType = "void"
                    , args = [ ( "String[]", "args" ) ]
                    , body =
                        [ Statement "return"
                        ]
                    }
                , MClass
                    { classComment = "InnerClass"
                    , name = "InnerClass"
                    , extends = Just "InnerBaseClass"
                    , implements = [ "Runnable" ]
                    , members = []
                    }
                ]
          }
        ]
    }



-- ==== Conversion of Java AST to Doc form for pretty printing.


classToDoc : Class -> Doc
classToDoc class =
    commentMultilineToDoc class.classComment
        |+ Doc.hardline
        |+ Doc.string "class "
        |+ Doc.string class.name
        |+ Doc.string " implements "
        |+ Doc.join (Doc.string ", ") (List.map (\interface -> Doc.string interface) class.implements)
        |+ Doc.hardline
        |+ membersToDoc class.members


classesToDoc : List Class -> Doc
classesToDoc classes =
    List.map classToDoc classes
        |> Doc.join (Doc.hardline)
        |> break


memberToDoc : Member -> Doc
memberToDoc member =
    case member of
        MClass innerClass ->
            classToDoc innerClass

        MField field ->
            fieldToDoc field

        MMethod method ->
            methodToDoc method


membersToDoc : List Member -> Doc
membersToDoc members =
    List.map memberToDoc members
        |> Doc.join (Doc.hardline |+ Doc.hardline)
        |> Doc.indent 4
        |> break
        |> Doc.braces


fieldToDoc field =
    Doc.string field.fieldType
        |+ Doc.char ' '
        |+ Doc.string field.name
        |+ eol


methodToDoc method =
    Doc.string method.returnType
        |+ Doc.char ' '
        |+ Doc.string method.name
        |+ argsToDoc method.args
        |+ statementsToDoc method.body


argsToDoc : List ( String, String ) -> Doc
argsToDoc args =
    List.map (\( jType, name ) -> Doc.string jType |+ Doc.char ' ' |+ Doc.string name) args
        |> Doc.join (Doc.char ',')
        |> Doc.parens


statementToDoc : Statement -> Doc
statementToDoc statement =
    case statement of
        Statement expr ->
            Doc.string expr
                |+ eol


statementsToDoc : List Statement -> Doc
statementsToDoc statementList =
    Doc.hardline
        |+ (List.map statementToDoc statementList
                |> Doc.join (Doc.hardline)
                |> Doc.indent 4
                |> break
                |> Doc.braces
           )


jFileToDoc : JavaFile -> Doc
jFileToDoc file =
    maybeDoc (commentMultilineToDoc >> flippend Doc.hardline) file.header
        |+ packageToDoc file.package
        |+ Doc.hardline
        |+ nonEmptyDoc (importsToDoc >> break) file.imports
        |+ classesToDoc file.classes


commentMultilineToDoc : String -> Doc
commentMultilineToDoc comment =
    Doc.string "/* "
        |+ Doc.string comment
        |+ Doc.string " */"


commentToDoc : String -> Doc
commentToDoc comment =
    Doc.string "// "
        |+ Doc.string comment


packageToDoc : String -> Doc
packageToDoc package =
    Doc.string "package "
        |+ Doc.string package
        |+ eol


importToDoc : String -> Doc
importToDoc importName =
    Doc.string "import "
        |+ Doc.string importName
        |+ eol


importsToDoc : List String -> Doc
importsToDoc imports =
    List.map importToDoc imports
        |> Doc.join (Doc.hardline)
