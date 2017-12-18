module JavaPrint exposing (javaSourceToString)

{-| A pretty printer for Java code built with the DLS.
@docs javaSourceToString
-}

import Pretty exposing ((|+), Doc)
import Internal.DocUtils
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
import Internal.JavaModel exposing (..)
import Internal.JavaSourceModel as JavaSourceModel


{-| Pretty prints a Java syntax tree as text.
-}
javaSourceToString : JavaSourceModel.JavaSource -> String
javaSourceToString (JavaSourceModel.JavaSource file) =
    jFileToDoc file
        |> Pretty.pretty 120


jFileToDoc : JavaFile -> Doc
jFileToDoc file =
    maybeDoc (commentMultilineToDoc >> flippend Pretty.line) file.header
        |+ packageToDoc file.package
        |+ Pretty.line
        |+ nonEmptyDoc (importsToDoc >> break) file.imports
        |+ classesToDoc file.classes


annotationToDoc : Annotation -> Doc
annotationToDoc annotation =
    Pretty.char '@'
        |+ Pretty.string annotation.name
        |+ nonEmptyDoc annotationArgsToDoc annotation.args


annotationsToDoc : Doc -> List Annotation -> Doc
annotationsToDoc separator annotations =
    List.map annotationToDoc annotations
        |> Pretty.join separator


annotationArgToDoc : ( Maybe String, AnnotationExpr ) -> Doc
annotationArgToDoc ( name, expr ) =
    maybeDoc (Pretty.string >> flippend (Pretty.string " = ")) name
        |+ annotationExprToDoc expr


annotationArgsToDoc : List ( Maybe String, AnnotationExpr ) -> Doc
annotationArgsToDoc args =
    List.map annotationArgToDoc args
        |> Pretty.join (commaSpace)
        |> Pretty.parens


annotationExprToDoc : AnnotationExpr -> Doc
annotationExprToDoc expr =
    case expr of
        AnnExprString val ->
            Pretty.string val

        AnnExpAnnotations annotations ->
            annotationsToDoc commaSoftline annotations
                |> Pretty.indent 4
                |> break
                |> Pretty.braces


accessModifierToDoc : AccessModifier -> Doc
accessModifierToDoc accessModifier =
    Pretty.string <|
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
            |> stringListToDoc Pretty.space


classToDoc : Class -> Doc
classToDoc class =
    maybeDoc (commentMultilineToDoc >> flippend Pretty.line) class.comment
        |+ nonEmptyDoc (annotationsToDoc Pretty.line >> flippend Pretty.line) class.annotations
        |+ maybeDoc (accessModifierToDoc >> flippend Pretty.space) class.accessModifier
        |+ maybeDoc (modifiersToDoc >> flippend Pretty.space) class.modifiers
        |+ Pretty.string "class "
        |+ Pretty.string class.name
        |+ maybeDoc (Pretty.string >> Pretty.append (Pretty.string " extends ")) class.extends
        |+ nonEmptyDoc (stringListToDoc commaSpace >> Pretty.append (Pretty.string " implements ")) class.implements
        |+ Pretty.line
        |+ membersToDoc class.members


classesToDoc : List Class -> Doc
classesToDoc classes =
    List.map classToDoc classes
        |> Pretty.join (Pretty.line)
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
        |> Pretty.join (Pretty.line |+ Pretty.line)
        |> Pretty.indent 4
        |> break
        |> Pretty.braces


fieldToDoc field =
    maybeDoc (commentMultilineToDoc >> flippend Pretty.line) field.comment
        |+ nonEmptyDoc (annotationsToDoc Pretty.line >> flippend Pretty.line) field.annotations
        |+ maybeDoc (accessModifierToDoc >> flippend Pretty.space) field.accessModifier
        |+ maybeDoc (modifiersToDoc >> flippend Pretty.space) field.modifiers
        |+ Pretty.string field.fieldType
        |+ Pretty.char ' '
        |+ Pretty.string field.name
        |+ maybeDoc (Pretty.string >> Pretty.append (Pretty.string " = ")) field.initialValue
        |+ eol


methodToDoc method =
    maybeDoc (commentMultilineToDoc >> flippend Pretty.line) method.comment
        |+ nonEmptyDoc (annotationsToDoc Pretty.line >> flippend Pretty.line) method.annotations
        |+ maybeDoc (accessModifierToDoc >> flippend Pretty.space) method.accessModifier
        |+ maybeDoc (modifiersToDoc >> flippend Pretty.space) method.modifiers
        |+ maybeDoc (Pretty.string >> flippend Pretty.space) method.returnType
        |+ Pretty.string method.name
        |+ argsToDoc method.args
        |+ nonEmptyDoc (stringListToDoc commaSpace >> Pretty.append (Pretty.string " throws ")) method.throws
        |+ statementsToDoc True method.body


initializerToDoc initializer =
    maybeDoc (commentMultilineToDoc >> flippend Pretty.line) initializer.comment
        |+ maybeDoc (modifiersToDoc >> flippend Pretty.space) initializer.modifiers
        |+ statementsToDoc False initializer.body


argsToDoc : List ( String, String ) -> Doc
argsToDoc args =
    List.map (\( jType, name ) -> Pretty.string jType |+ Pretty.char ' ' |+ Pretty.string name) args
        |> Pretty.join comma
        |> Pretty.parens


statementToDoc : Statement -> Doc
statementToDoc statement =
    case statement of
        Statement expr ->
            Pretty.string expr
                |+ eol


statementsToDoc : Bool -> List Statement -> Doc
statementsToDoc newlineCurlyBrace statementList =
    List.map statementToDoc statementList
        |> Pretty.join (Pretty.line)
        |> Pretty.indent 4
        |> break
        |> Pretty.braces
        |> Pretty.append
            (if newlineCurlyBrace then
                Pretty.line
             else
                Pretty.empty
            )


commentMultilineToDoc : String -> Doc
commentMultilineToDoc comment =
    Pretty.string "/* "
        |+ Pretty.string comment
        |+ Pretty.string " */"


commentToDoc : String -> Doc
commentToDoc comment =
    Pretty.string "// "
        |+ Pretty.string comment


packageToDoc : String -> Doc
packageToDoc package =
    Pretty.string "package "
        |+ Pretty.string package
        |+ eol


importToDoc : String -> Doc
importToDoc importName =
    Pretty.string "import "
        |+ Pretty.string importName
        |+ eol


importsToDoc : List String -> Doc
importsToDoc imports =
    List.map importToDoc imports
        |> Pretty.join (Pretty.line)
