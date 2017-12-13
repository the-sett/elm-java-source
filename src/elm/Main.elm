port module Main exposing (..)

import Html exposing (Html, div, text, pre)
import Doc exposing ((|+), Doc)


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



-- Java model


type alias JavaFile =
    { header : String
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


type Member
    = InnerClass Class
    | Field String String
    | Method String String (List ( String, String )) (List Statement)


type Statement
    = Statement String



-- Java Examples


javaExample : JavaFile
javaExample =
    { header = ""
    , package = "com.thesett.example"
    , imports = [ "org.springframework.core", "java.util.list" ]
    , classes =
        [ { classComment = "Example"
          , name = "Example"
          , extends = (Just "BaseClass")
          , implements = [ "Serializable" ]
          , members =
                [ Field "int" "test"
                , Method "main"
                    "void"
                    [ ( "String[]", "args" ) ]
                    [ Statement "return"
                    ]
                , InnerClass
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
        InnerClass innerClass ->
            classToDoc innerClass

        Field jType name ->
            Doc.string jType
                |+ Doc.char ' '
                |+ Doc.string name
                |+ eol

        Method name returnType args statements ->
            Doc.string returnType
                |+ Doc.char ' '
                |+ Doc.string name
                |+ statementsToDoc statements


membersToDoc : List Member -> Doc
membersToDoc members =
    List.map memberToDoc members
        |> Doc.join (Doc.hardline |+ Doc.hardline)
        |> Doc.indent 4
        |> break
        |> Doc.braces


statementToDoc : Statement -> Doc
statementToDoc statement =
    case statement of
        Statement expr ->
            Doc.string expr
                |+ eol


statementsToDoc : List Statement -> Doc
statementsToDoc statementList =
    List.map statementToDoc statementList
        |> Doc.join (Doc.hardline)
        |> Doc.indent 4
        |> break
        |> Doc.braces


jFileToDoc : JavaFile -> Doc
jFileToDoc file =
    commentMultilineToDoc file.header
        |+ Doc.hardline
        |+ packageToDoc file.package
        |+ Doc.hardline
        |+ importsToDoc file.imports
        |+ Doc.hardline
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
        |> break
