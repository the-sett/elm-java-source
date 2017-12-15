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
    jFileToDoc
        javaExample
        |> Doc.toString



-- ==== Reusable Doc functions


break : Doc -> Doc
break =
    Doc.surround Doc.hardline Doc.hardline


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


{-| A flipped version of append useful in function chaining situations.
-}
flippend : Doc -> Doc -> Doc
flippend doc =
    flip Doc.append doc



-- ==== Java model


type alias JavaFile =
    { header : Maybe String
    , package : String
    , imports : List String
    , classes : List Class
    }


type alias Class =
    { comment : Maybe String
    , annotations : List Annotation
    , accessModifier : Maybe AccessModifier
    , modifiers : Maybe Modifiers
    , name : String
    , extends : Maybe String
    , implements : List String
    , members : List Member
    }


type alias Method =
    { comment : Maybe String
    , annotations : List Annotation
    , accessModifier : Maybe AccessModifier
    , modifiers : Maybe Modifiers
    , name : String
    , returnType : Maybe String
    , args : List ( String, String )
    , throws : List String
    , body : List Statement
    }


type alias Field =
    { comment : Maybe String
    , annotations : List Annotation
    , accessModifier : Maybe AccessModifier
    , modifiers : Maybe Modifiers
    , name : String
    , fieldType : String
    , initialValue : Maybe String
    }


type alias Annotation =
    { name : String
    , args : List ( Maybe String, AnnotationExpr )
    }


type AnnotationExpr
    = AnnExprString String
    | AnnExpAnnotations (List Annotation)


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


type Member
    = MClass Class
    | MField Field
    | MInitializer Initializer
    | MConstructor Method
    | MMethod Method


type Statement
    = Statement String



-- ==== DSL for constructing Java ASTs.


type Builder
    = BuildFile JavaFile
    | BuildClass Class
    | BuildMethod Method
    | BuildField Field
    | BuildInitializer Initializer
    | BuildConstructor Method


type alias Attribute =
    Builder -> Builder


type alias AnnotationBuilder =
    Annotation -> Annotation


defaultJavaFile : JavaFile
defaultJavaFile =
    { package = ""
    , header = Nothing
    , imports = []
    , classes = []
    }


defaultClass : Class
defaultClass =
    { name = ""
    , comment = Nothing
    , annotations = []
    , accessModifier = Nothing
    , modifiers = Nothing
    , extends = Nothing
    , implements = []
    , members = []
    }


defaultMethod : Method
defaultMethod =
    { name = ""
    , comment = Nothing
    , annotations = []
    , accessModifier = Nothing
    , modifiers = Nothing
    , returnType = Nothing
    , args = []
    , throws = []
    , body = []
    }


defaultField : Field
defaultField =
    { name = ""
    , fieldType = ""
    , comment = Nothing
    , annotations = []
    , accessModifier = Nothing
    , modifiers = Nothing
    , initialValue = Nothing
    }


defaultAnnotation : Annotation
defaultAnnotation =
    { name = ""
    , args = []
    }


defaultInitializer : Initializer
defaultInitializer =
    { comment = Nothing
    , modifiers = Nothing
    , body = []
    }


defaultModifiers : Modifiers
defaultModifiers =
    { static = False
    , final = False
    , synchronized = False
    , abstract = False
    , volatile = False
    }


extractFile : Builder -> Maybe JavaFile
extractFile builder =
    case builder of
        BuildFile file ->
            Just file

        _ ->
            Nothing


extractClass : Builder -> Maybe Class
extractClass builder =
    case builder of
        BuildClass class ->
            Just class

        _ ->
            Nothing


extractMember : Builder -> Maybe Member
extractMember builder =
    case builder of
        BuildClass class ->
            Just <| MClass class

        BuildMethod method ->
            Just <| MMethod method

        BuildField field ->
            Just <| MField field

        BuildInitializer initializer ->
            Just <| MInitializer initializer

        BuildConstructor method ->
            Just <| MConstructor method

        _ ->
            Nothing


file : List Attribute -> List Builder -> JavaFile
file attrs builders =
    List.foldl (\attr -> \builder -> attr builder) (BuildFile defaultJavaFile) attrs
        |> extractFile
        |> Maybe.withDefault defaultJavaFile
        |> (\file -> { file | classes = List.filterMap extractClass builders })


class : String -> List Attribute -> List Builder -> Builder
class name attrs builders =
    List.foldl (\attr -> \builder -> attr builder)
        (BuildClass { defaultClass | members = List.filterMap extractMember builders, name = name })
        attrs


field : String -> String -> List Attribute -> a -> Builder
field jType name attrs _ =
    List.foldl (\attr -> \builder -> attr builder)
        (BuildField { defaultField | name = name, fieldType = jType })
        attrs


initializer : List Attribute -> List Statement -> Builder
initializer attrs body =
    List.foldl (\attr -> \builder -> attr builder)
        (BuildInitializer { defaultInitializer | body = body })
        attrs


constructor : List Attribute -> List Statement -> Builder
constructor attrs body =
    List.foldl (\attr -> \builder -> attr builder)
        (BuildConstructor { defaultMethod | body = body })
        attrs


method : String -> List Attribute -> List Statement -> Builder
method name attrs body =
    List.foldl (\attr -> \builder -> attr builder)
        (BuildMethod { defaultMethod | name = name, body = body })
        attrs


statement : String -> Statement
statement val =
    Statement val


header : String -> Attribute
header val builder =
    case builder of
        BuildFile file ->
            BuildFile { file | header = Just val }

        x ->
            x


package : String -> Attribute
package val builder =
    case builder of
        BuildFile file ->
            BuildFile { file | package = val }

        x ->
            x


imports : List String -> Attribute
imports imports builder =
    case builder of
        BuildFile file ->
            BuildFile { file | imports = imports }

        x ->
            x


comment : String -> Attribute
comment val builder =
    let
        addComment val rec =
            { rec | comment = Just val }
    in
        case builder of
            BuildClass class ->
                BuildClass <| addComment val class

            BuildField field ->
                BuildField <| addComment val field

            BuildInitializer initializer ->
                BuildInitializer <| addComment val initializer

            BuildConstructor constructor ->
                BuildConstructor <| addComment val constructor

            BuildMethod method ->
                BuildMethod <| addComment val method

            x ->
                x


accessModifier : AccessModifier -> Attribute
accessModifier accessModifier builder =
    let
        addAccessModifier val rec =
            { rec | accessModifier = Just val }
    in
        case builder of
            BuildClass class ->
                BuildClass <| addAccessModifier accessModifier class

            BuildMethod method ->
                BuildMethod <| addAccessModifier accessModifier method

            BuildField field ->
                BuildField <| addAccessModifier accessModifier field

            BuildConstructor method ->
                BuildConstructor <| addAccessModifier accessModifier method

            x ->
                x


public : Attribute
public builder =
    accessModifier Public builder


protected : Attribute
protected builder =
    accessModifier Protected builder


private : Attribute
private builder =
    accessModifier Private builder


modifiers : Modifiers -> Attribute
modifiers modifiers builder =
    let
        addModifiers modsToSet rec =
            let
                updatedModifiers =
                    case rec.modifiers of
                        Nothing ->
                            Just modsToSet

                        Just existingMods ->
                            Just
                                { static = existingMods.static || modsToSet.static
                                , final = existingMods.final || modsToSet.final
                                , abstract = existingMods.abstract || modsToSet.abstract
                                , volatile = existingMods.volatile || modsToSet.volatile
                                , synchronized = existingMods.synchronized || modsToSet.synchronized
                                }
            in
                { rec | modifiers = updatedModifiers }
    in
        case builder of
            BuildClass class ->
                BuildClass <| addModifiers modifiers class

            BuildMethod method ->
                BuildMethod <| addModifiers modifiers method

            BuildField field ->
                BuildField <| addModifiers modifiers field

            BuildConstructor method ->
                BuildConstructor <| addModifiers modifiers method

            BuildInitializer initializer ->
                BuildInitializer <| addModifiers modifiers initializer

            x ->
                x


static : Attribute
static builder =
    let
        static =
            { defaultModifiers | static = True }
    in
        case builder of
            BuildClass _ ->
                modifiers static builder

            BuildMethod _ ->
                modifiers static builder

            BuildField _ ->
                modifiers static builder

            BuildInitializer _ ->
                modifiers static builder

            x ->
                x


final : Attribute
final builder =
    let
        final =
            { defaultModifiers | final = True }
    in
        case builder of
            BuildClass _ ->
                modifiers final builder

            BuildMethod _ ->
                modifiers final builder

            BuildField _ ->
                modifiers final builder

            x ->
                x


volatile : Attribute
volatile builder =
    let
        volatile =
            { defaultModifiers | volatile = True }
    in
        case builder of
            BuildField _ ->
                modifiers volatile builder

            x ->
                x


abstract : Attribute
abstract builder =
    let
        abstract =
            { defaultModifiers | abstract = True }
    in
        case builder of
            BuildClass _ ->
                modifiers abstract builder

            BuildMethod _ ->
                modifiers abstract builder

            x ->
                x


synchronized : Attribute
synchronized builder =
    let
        synchronized =
            { defaultModifiers | synchronized = True }
    in
        case builder of
            BuildMethod _ ->
                modifiers synchronized builder

            x ->
                x


extends : String -> Attribute
extends val builder =
    case builder of
        BuildClass class ->
            BuildClass { class | extends = Just val }

        x ->
            x


implements : List String -> Attribute
implements implements builder =
    case builder of
        BuildClass class ->
            BuildClass { class | implements = implements }

        x ->
            x


args : List ( String, String ) -> Attribute
args args builder =
    case builder of
        BuildMethod method ->
            BuildMethod { method | args = args }

        BuildConstructor method ->
            BuildConstructor { method | args = args }

        x ->
            x


returnType : String -> Attribute
returnType returnType builder =
    case builder of
        BuildMethod method ->
            BuildMethod { method | returnType = Just returnType }

        x ->
            x


throws : List String -> Attribute
throws exceptions builder =
    case builder of
        BuildMethod method ->
            BuildMethod { method | throws = exceptions }

        BuildConstructor method ->
            BuildConstructor { method | throws = exceptions }

        x ->
            x


annotation : String -> List AnnotationBuilder -> Annotation
annotation name annBuilders =
    List.foldl (\annBuilder -> \annotation -> annBuilder annotation)
        { defaultAnnotation | name = name }
        annBuilders


annotationList : List Annotation -> AnnotationBuilder
annotationList annotations annotation =
    { annotation | args = List.append annotation.args [ ( Nothing, AnnExpAnnotations annotations ) ] }


annotationNameValue : String -> String -> AnnotationBuilder
annotationNameValue name value annotation =
    { annotation | args = List.append annotation.args [ ( Just name, AnnExprString value ) ] }


annotate : List Annotation -> Attribute
annotate annotations builder =
    let
        addAnnotations annotations rec =
            { rec | annotations = annotations }
    in
        case builder of
            BuildClass class ->
                BuildClass <| addAnnotations annotations class

            BuildMethod method ->
                BuildMethod <| addAnnotations annotations method

            BuildField field ->
                BuildField <| addAnnotations annotations field

            BuildConstructor method ->
                BuildConstructor <| addAnnotations annotations method

            x ->
                x



-- Java Examples


javaExample : JavaFile
javaExample =
    file
        [ header "Copyright blah..."
        , package "com.thesett.example"
        , imports
            [ "org.springframework.core"
            , "java.util.list"
            ]
        ]
        [ class "Example"
            [ comment "Example"
            , public >> final
            , extends "BaseClass"
            , implements [ "Serializable", "Cloneable" ]
            , annotate
                [ annotation "Component" []
                , annotation "Entity" []
                , annotation "NamedQueries"
                    [ annotationList
                        [ annotation "NamedQuery"
                            [ annotationNameValue "name" "\"Country.findAll\""
                            , annotationNameValue "query" "\"SELECT c FROM Country c\""
                            ]
                        , annotation "NamedQuery"
                            [ annotationNameValue "name" "\"Region.findAll\""
                            , annotationNameValue "query" "\"SELECT r FROM Region r\""
                            ]
                        ]
                    ]
                ]
            ]
            [ field "int"
                "test"
                [ comment "This is a field"
                , private >> volatile >> static
                ]
                []
            , initializer
                [ comment "This is an initializer block."
                , static
                ]
                [ statement "test = 2" ]
            , constructor
                [ comment "This is a constructor"
                , public
                , args [ ( "int", "test" ) ]
                ]
                [ statement "this.test = test" ]
            , method "main"
                [ comment "This is a method."
                , public >> static
                , returnType "void"
                , args [ ( "String[]", "args" ) ]
                , throws [ "IOException", "ClassNotFoundException" ]
                , annotate
                    [ annotation "Bean" []
                    , annotation "Timed" []
                    , annotation "UnitOfWork" [ annotationNameValue "context" "\"Mandatory\"" ]
                    ]
                ]
                [ statement "return" ]
            , class "InnerClass"
                [ comment "This is an inner class."
                , protected >> abstract
                , implements [ "Runnable" ]
                ]
                []
            ]
        ]



-- ==== Conversion of Java AST to Doc form for pretty printing.


annotationToDoc : Annotation -> Doc
annotationToDoc annotation =
    Doc.char '@'
        |+ Doc.string annotation.name
        |+ nonEmptyDoc annotationArgsToDoc annotation.args


annotationsToDoc : Doc -> List Annotation -> Doc
annotationsToDoc separator annotations =
    List.map annotationToDoc annotations
        |> Doc.join separator


annotationArgToDoc : ( Maybe String, AnnotationExpr ) -> Doc
annotationArgToDoc ( name, expr ) =
    maybeDoc (Doc.string >> flippend (Doc.string " = ")) name
        |+ annotationExprToDoc expr


annotationArgsToDoc : List ( Maybe String, AnnotationExpr ) -> Doc
annotationArgsToDoc args =
    List.map annotationArgToDoc args
        |> Doc.join (commaSpace)
        |> Doc.parens


annotationExprToDoc : AnnotationExpr -> Doc
annotationExprToDoc expr =
    case expr of
        AnnExprString val ->
            Doc.string val

        AnnExpAnnotations annotations ->
            annotationsToDoc commaSoftline annotations
                |> Doc.indent 4
                |> break
                |> Doc.braces


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
        |+ nonEmptyDoc (annotationsToDoc Doc.hardline >> flippend Doc.hardline) class.annotations
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

        MConstructor method ->
            methodToDoc method

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
        |+ nonEmptyDoc (annotationsToDoc Doc.hardline >> flippend Doc.hardline) field.annotations
        |+ maybeDoc (accessModifierToDoc >> flippend Doc.space) field.accessModifier
        |+ maybeDoc (modifiersToDoc >> flippend Doc.space) field.modifiers
        |+ Doc.string field.fieldType
        |+ Doc.char ' '
        |+ Doc.string field.name
        |+ eol


methodToDoc method =
    maybeDoc (commentMultilineToDoc >> flippend Doc.hardline) method.comment
        |+ nonEmptyDoc (annotationsToDoc Doc.hardline >> flippend Doc.hardline) method.annotations
        |+ maybeDoc (accessModifierToDoc >> flippend Doc.space) method.accessModifier
        |+ maybeDoc (modifiersToDoc >> flippend Doc.space) method.modifiers
        |+ maybeDoc (Doc.string >> flippend Doc.space) method.returnType
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
