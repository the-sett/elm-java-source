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


comma =
    Doc.char ','


commaSpace =
    Doc.string ", "


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
    { comment : Maybe String
    , accessModifier : Maybe AccessModifier
    , modifiers : Maybe Modifiers
    , name : String
    , extends : Maybe String
    , implements : List String
    , members : List Member
    }


type alias Method =
    { comment : Maybe String
    , accessModifier : Maybe AccessModifier
    , modifiers : Maybe Modifiers
    , name : String
    , returnType : String
    , args : List ( String, String )
    , throws : List String
    , body : List Statement
    }


type alias Field =
    { comment : Maybe String
    , accessModifier : Maybe AccessModifier
    , modifiers : Maybe Modifiers
    , name : String
    , fieldType : String
    , initialValue : Maybe String
    }


type alias Initializer =
    { comment : Maybe String
    , modifiers : Maybe Modifiers
    , body : List Statement
    }


type AccessModifier
    = Private
    | Protected
    | Public


type alias Modifiers =
    { static : Bool
    , final : Bool
    , synchronized : Bool
    , abstract : Bool
    , volatile : Bool
    }


defaultModifiers : Modifiers
defaultModifiers =
    { static = False
    , final = False
    , synchronized = False
    , abstract = False
    , volatile = False
    }


type Member
    = MClass Class
    | MField Field
    | MInitializer Initializer
    | MMethod Method


type Statement
    = Statement String



-- Java Examples


javaExample : JavaFile
javaExample =
    { header = Just "Copyright blah..."
    , package = "com.thesett.example"
    , imports = [ "org.springframework.core", "java.util.list" ]
    , classes =
        [ { comment = Just "Example"
          , accessModifier = Just Public
          , modifiers = Just { defaultModifiers | final = True }
          , name = "Example"
          , extends = (Just "BaseClass")
          , implements = [ "Serializable", "Cloneable" ]
          , members =
                [ MField
                    { comment = Just "This is a field."
                    , accessModifier = Just Private
                    , modifiers = Just { defaultModifiers | volatile = True }
                    , name = "test"
                    , fieldType = "int"
                    , initialValue = Nothing
                    }
                , MInitializer
                    { comment = Just "This is an initializer block."
                    , modifiers = Just { defaultModifiers | static = True }
                    , body =
                        [ Statement "test = 2"
                        ]
                    }
                , MMethod
                    { comment = Just "This is a method."
                    , accessModifier = Just Public
                    , modifiers = Just { defaultModifiers | static = True }
                    , name = "main"
                    , returnType = "void"
                    , args = [ ( "String[]", "args" ) ]
                    , throws = [ "IOException", "ClassNotFoundException" ]
                    , body =
                        [ Statement "return"
                        ]
                    }
                , MClass
                    { comment = Just "InnerClass"
                    , accessModifier = Just Protected
                    , modifiers = Just { defaultModifiers | abstract = True }
                    , name = "InnerClass"
                    , extends = Nothing
                    , implements = [ "Runnable" ]
                    , members = []
                    }
                ]
          }
        ]
    }



-- ==== Conversion of Java AST to Doc form for pretty printing.


accessModifierToDoc : AccessModifier -> Doc
accessModifierToDoc accessModifier =
    Doc.string <|
        case accessModifier of
            Private ->
                "private"

            Protected ->
                "protected"

            Public ->
                "public"


modifiersToDoc : Modifiers -> Doc
modifiersToDoc modifiers =
    let
        flagToValList flag val =
            if flag then
                [ val ]
            else
                []
    in
        flagToValList modifiers.static "static"
            |> List.append (flagToValList modifiers.final "final")
            |> List.append (flagToValList modifiers.abstract "abstract")
            |> List.append (flagToValList modifiers.synchronized "synchronized")
            |> List.append (flagToValList modifiers.volatile "volatile")
            |> stringListToDoc Doc.space


classToDoc : Class -> Doc
classToDoc class =
    maybeDoc (commentMultilineToDoc >> flippend Doc.hardline) class.comment
        |+ maybeDoc (accessModifierToDoc >> flippend Doc.space) class.accessModifier
        |+ maybeDoc (modifiersToDoc >> flippend Doc.space) class.modifiers
        |+ Doc.string "class "
        |+ Doc.string class.name
        |+ maybeDoc (Doc.string >> Doc.append (Doc.string " extends ")) class.extends
        |+ nonEmptyDoc (stringListToDoc commaSpace >> Doc.append (Doc.string " implements ")) class.implements
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

        MInitializer initializer ->
            initializerToDoc initializer

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
    maybeDoc (commentMultilineToDoc >> flippend Doc.hardline) field.comment
        |+ maybeDoc (accessModifierToDoc >> flippend Doc.space) field.accessModifier
        |+ maybeDoc (modifiersToDoc >> flippend Doc.space) field.modifiers
        |+ Doc.string field.fieldType
        |+ Doc.char ' '
        |+ Doc.string field.name
        |+ eol


methodToDoc method =
    maybeDoc (commentMultilineToDoc >> flippend Doc.hardline) method.comment
        |+ maybeDoc (accessModifierToDoc >> flippend Doc.space) method.accessModifier
        |+ maybeDoc (modifiersToDoc >> flippend Doc.space) method.modifiers
        |+ Doc.string method.returnType
        |+ Doc.space
        |+ Doc.string method.name
        |+ argsToDoc method.args
        |+ nonEmptyDoc (stringListToDoc commaSpace >> Doc.append (Doc.string " throws ")) method.throws
        |+ statementsToDoc True method.body


initializerToDoc initializer =
    maybeDoc (commentMultilineToDoc >> flippend Doc.hardline) initializer.comment
        |+ maybeDoc (modifiersToDoc >> flippend Doc.space) initializer.modifiers
        |+ statementsToDoc False initializer.body


argsToDoc : List ( String, String ) -> Doc
argsToDoc args =
    List.map (\( jType, name ) -> Doc.string jType |+ Doc.char ' ' |+ Doc.string name) args
        |> Doc.join comma
        |> Doc.parens


statementToDoc : Statement -> Doc
statementToDoc statement =
    case statement of
        Statement expr ->
            Doc.string expr
                |+ eol


statementsToDoc : Bool -> List Statement -> Doc
statementsToDoc newlineCurlyBrace statementList =
    List.map statementToDoc statementList
        |> Doc.join (Doc.hardline)
        |> Doc.indent 4
        |> break
        |> Doc.braces
        |> Doc.append
            (if newlineCurlyBrace then
                Doc.hardline
             else
                Doc.empty
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
