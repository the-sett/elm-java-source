module JavaPrint exposing (javaSourceToString)

{-| A pretty printer for Java code built with the DLS.
@docs javaSourceToString
-}

import Doc exposing ((|+), Doc)
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
        |> Doc.toString


jFileToDoc : JavaFile -> Doc
jFileToDoc file =
    maybeDoc (commentMultilineToDoc >> flippend Doc.hardline) file.header
        |+ packageToDoc file.package
        |+ Doc.hardline
        |+ nonEmptyDoc (importsToDoc >> break) file.imports
        |+ classesToDoc file.classes


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
        |+ maybeDoc (Doc.string >> Doc.append (Doc.string " = ")) field.initialValue
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
