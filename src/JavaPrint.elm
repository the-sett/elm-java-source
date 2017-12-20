module JavaPrint exposing (javaSourceToString)

{-| A pretty printer for Java code built with the DLS.
@docs javaSourceToString
-}

import Pretty
    exposing
        ( (|+)
        , Doc
        , empty
        , append
        , line
        , char
        , string
        , surround
        , softline
        , join
        , pretty
        , parens
        , space
        , indent
        , braces
        )
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


{-| Pretty prints a Java syntax tree as text.
-}
javaSourceToString : JavaSource -> String
javaSourceToString (JavaSource file) =
    jFileToDoc file
        |> pretty 120


jFileToDoc : JavaFile -> Doc
jFileToDoc file =
    maybeDoc (commentMultilineToDoc >> flippend line) file.header
        |+ packageToDoc file.package
        |+ line
        |+ nonEmptyDoc (importsToDoc >> break) file.imports
        |+ classesToDoc file.classes


annotationToDoc : Annotation -> Doc
annotationToDoc annotation =
    char '@'
        |+ string annotation.name
        |+ nonEmptyDoc annotationArgsToDoc annotation.args


annotationsToDoc : Doc -> List Annotation -> Doc
annotationsToDoc separator annotations =
    List.map annotationToDoc annotations
        |> join separator


annotationArgToDoc : ( Maybe String, AnnotationExpr ) -> Doc
annotationArgToDoc ( name, expr ) =
    maybeDoc (string >> flippend (string " = ")) name
        |+ annotationExprToDoc expr


annotationArgsToDoc : List ( Maybe String, AnnotationExpr ) -> Doc
annotationArgsToDoc args =
    List.map annotationArgToDoc args
        |> join (commaSpace)
        |> parens


annotationExprToDoc : AnnotationExpr -> Doc
annotationExprToDoc expr =
    case expr of
        AnnExprString val ->
            string val

        AnnExpAnnotations annotations ->
            annotationsToDoc commaSoftline annotations
                |> indent 4
                |> break
                |> braces


accessModifierToDoc : AccessModifier -> Doc
accessModifierToDoc accessModifier =
    string <|
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
            |> stringListToDoc space


classToDoc : Class -> Doc
classToDoc class =
    maybeDoc (commentMultilineToDoc >> flippend line) class.comment
        |+ nonEmptyDoc (annotationsToDoc line >> flippend line) class.annotations
        |+ maybeDoc (accessModifierToDoc >> flippend space) class.accessModifier
        |+ maybeDoc (modifiersToDoc >> flippend space) class.modifiers
        |+ string "class "
        |+ string class.name
        |+ maybeDoc (string >> append (string " extends ")) class.extends
        |+ nonEmptyDoc (stringListToDoc commaSpace >> append (string " implements ")) class.implements
        |+ line
        |+ membersToDoc class.members


classesToDoc : List Class -> Doc
classesToDoc classes =
    List.map classToDoc classes
        |> join (line)
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
        |> join (line |+ line)
        |> indent 4
        |> break
        |> braces


fieldToDoc field =
    maybeDoc (commentMultilineToDoc >> flippend line) field.comment
        |+ nonEmptyDoc (annotationsToDoc line >> flippend line) field.annotations
        |+ maybeDoc (accessModifierToDoc >> flippend space) field.accessModifier
        |+ maybeDoc (modifiersToDoc >> flippend space) field.modifiers
        |+ string field.fieldType
        |+ space
        |+ string field.name
        |+ maybeDoc (string >> append (string " = ")) field.initialValue
        |+ eol


methodToDoc method =
    maybeDoc (commentMultilineToDoc >> flippend line) method.comment
        |+ nonEmptyDoc (annotationsToDoc line >> flippend line) method.annotations
        |+ maybeDoc (accessModifierToDoc >> flippend space) method.accessModifier
        |+ maybeDoc (modifiersToDoc >> flippend space) method.modifiers
        |+ maybeDoc (string >> flippend space) method.returnType
        |+ string method.name
        |+ argsToDoc method.args
        |+ nonEmptyDoc (stringListToDoc commaSpace >> append (string " throws ")) method.throws
        |+ statementsToDoc True method.body


initializerToDoc initializer =
    maybeDoc (commentMultilineToDoc >> flippend line) initializer.comment
        |+ maybeDoc (modifiersToDoc >> flippend space) initializer.modifiers
        |+ statementsToDoc False initializer.body


argsToDoc : List ( String, String, List Annotation ) -> Doc
argsToDoc args =
    List.map
        (\( jType, name, annotations ) ->
            nonEmptyDoc (annotationsToDoc softline >> flippend space) annotations
                |+ string jType
                |+ space
                |+ string name
        )
        args
        |> join commaSpace
        |> parens


commentMultilineToDoc : String -> Doc
commentMultilineToDoc comment =
    string "/* "
        |+ string comment
        |+ string " */"


commentToDoc : String -> Doc
commentToDoc comment =
    string "// "
        |+ string comment


packageToDoc : String -> Doc
packageToDoc package =
    string "package "
        |+ string package
        |+ eol


importToDoc : String -> Doc
importToDoc importName =
    string "import "
        |+ string importName
        |+ eol


importsToDoc : List String -> Doc
importsToDoc imports =
    List.map importToDoc imports
        |> join (line)


statementsToDoc : Bool -> List Statement -> Doc
statementsToDoc newlineCurlyBrace statementList =
    List.map statementToDoc statementList
        |> join (line)
        |> indent 4
        |> break
        |> braces
        |> append
            (if newlineCurlyBrace then
                line
             else
                empty
            )


statementToDoc : Statement -> Doc
statementToDoc statement =
    case statement of
        For init check next body ->
            forToDoc init check next body

        Assign var expr ->
            assignToDoc var expr
                |+ string ";"

        Invoke method args ->
            invokeToDoc method args
                |+ string ";"

        Return expr ->
            returnToDoc expr
                |+ string ";"


forToDoc : Statement -> Expr -> Expr -> List Statement -> Doc
forToDoc init check next body =
    string "for ("
        |+ statementToDoc init
        |+ softline
        |+ exprToDoc check
        |+ string ";"
        |+ softline
        |+ exprToDoc next
        |+ string ")"
        |+ statementsToDoc True body


assignToDoc : String -> Expr -> Doc
assignToDoc var expr =
    string var
        |+ string " = "
        |+ exprToDoc expr


invokeToDoc : String -> List Expr -> Doc
invokeToDoc method args =
    string method
        |+ (exprsToDoc commaSpace args |> parens)


returnToDoc : Expr -> Doc
returnToDoc expr =
    string "return"
        |+ softline
        |+ exprToDoc expr


exprsToDoc : Doc -> List Expr -> Doc
exprsToDoc separator exprs =
    List.map exprToDoc exprs
        |> join separator


exprToDoc : Expr -> Doc
exprToDoc expr =
    case expr of
        ExprStringLit val ->
            string "\"" |+ string val |+ string "\""

        ExprLit val ->
            string val

        ExprBinary op expr expr2 ->
            exprToDoc expr
                |+ space
                |+ string op
                |+ softline
                |+ exprToDoc expr2
