module JavaBuilder
    exposing
        ( JavaSource
        , Builder
        , Attribute
        , file
        , class
        , field
        , initializer
        , constructor
        , method
        , header
        , package
        , imports
        , importStatics
        , comment
        , public
        , protected
        , private
        , static
        , final
        , volatile
        , abstract
        , synchronized
        , extends
        , implements
        , args
        , arg
        , annArg
        , returnType
        , throws
        , initialValue
        , annotation
        , annotationList
        , annotationNameValue
        , annotationDefaultValue
        , annotate
          -- ===
        , var
        , string
        , int
        , for
        , assign
        , lt
        , plus
        , invoke
        , postIncr
        , return
        , codeComment
        )

{-| A DSL for building Java code as an abstract syntax tree.
@docs JavaSource, Builder, Attribute
@docs file, class, field, initializer, constructor, method
@docs header, package, imports, importStatics, comment
@docs public, protected, private
@docs static, final, volatile, abstract, synchronized
@docs extends, implements
@docs args, arg, annArg, returnType, throws
@docs initialValue
@docs annotation, annotationList, annotationNameValue, annotationDefaultValue, annotate
@docs var, string, int, for, assign, lt, plus, invoke, postIncr, return, codeComment
-}

import Internal.JavaModel exposing (..)


{-| The type of Java source code as an abstract syntax tree.
-}
type alias JavaSource =
    Internal.JavaModel.JavaSource



-- ==== Internal types and helper functions.


{-| Constructors for the major parts of a Java source file, top-level file details,
classes, fields, methods, constructors and initializer blocks.
-}
type Builder
    = BuildFile JavaFile
    | BuildClass Class
    | BuildMethod Method
    | BuildField Field
    | BuildInitializer Initializer
    | BuildConstructor (String -> Method)


{-| Attributes as functions over the major parts of a Java source file. Applying
these functions modifies the builder if the attribute in question is allowed to be
applied to that builder. If the attribute in question is not allowed to be applied
to a builder, it is ignored.
-}
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


{-| Extracts the members of a class from a builder.
The class name is passed as the first argument, as it must be passed the
constructor builder, in order that the constrctor names match the class
in which they appear.
-}
extractMember : String -> Builder -> Maybe Member
extractMember name builder =
    case builder of
        BuildClass class ->
            Just <| MClass class

        BuildMethod method ->
            Just <| MMethod method

        BuildField field ->
            Just <| MField field

        BuildInitializer initializer ->
            Just <| MInitializer initializer

        BuildConstructor consBuilder ->
            Just <| MConstructor <| consBuilder name

        _ ->
            Nothing


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

            BuildConstructor consBuilder ->
                BuildConstructor <| consBuilder >> (addAccessModifier accessModifier)

            x ->
                x


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

            BuildConstructor consBuilder ->
                BuildConstructor <| consBuilder >> (addModifiers modifiers)

            BuildInitializer initializer ->
                BuildInitializer <| addModifiers modifiers initializer

            x ->
                x



-- === The DSL for building Java Source code models.
-- Part1 - Files, classes, methods.


{-| Creates a file layout for a file containing Java source code.
-}
file : List Attribute -> List Builder -> JavaSource
file attrs builders =
    List.foldl (\attr -> \builder -> attr builder) (BuildFile defaultJavaFile) attrs
        |> extractFile
        |> Maybe.withDefault defaultJavaFile
        |> (\file -> { file | classes = List.filterMap extractClass builders })
        |> JavaSource


{-| Defines a class.
-}
class : String -> List Attribute -> List Builder -> Builder
class name attrs builders =
    List.foldl (\attr -> \builder -> attr builder)
        (BuildClass { defaultClass | members = List.filterMap (extractMember name) builders, name = name })
        attrs


{-| Defines a field.
-}
field : String -> String -> List Attribute -> Builder
field jType name attrs =
    List.foldl (\attr -> \builder -> attr builder)
        (BuildField { defaultField | name = name, fieldType = jType })
        attrs


{-| Defines an initializer block.
-}
initializer : List Attribute -> List Statement -> Builder
initializer attrs body =
    List.foldl (\attr -> \builder -> attr builder)
        (BuildInitializer { defaultInitializer | body = body })
        attrs


{-| Defines a constructor.
-}
constructor : List Attribute -> List Statement -> Builder
constructor attrs body =
    List.foldl (\attr -> \builder -> attr builder)
        (BuildConstructor (\name -> { defaultMethod | name = name, body = body }))
        attrs


{-| Defines a method.
-}
method : String -> List Attribute -> List Statement -> Builder
method name attrs body =
    List.foldl (\attr -> \builder -> attr builder)
        (BuildMethod { defaultMethod | name = name, body = body })
        attrs


{-| Defines a header comment for a file.
-}
header : String -> Attribute
header val builder =
    case builder of
        BuildFile file ->
            BuildFile { file | header = Just val }

        x ->
            x


{-| Sets the package name for a file.
-}
package : String -> Attribute
package val builder =
    case builder of
        BuildFile file ->
            BuildFile { file | package = val }

        x ->
            x


{-| Adds a list of imports to a file.
-}
imports : List String -> Attribute
imports imports builder =
    let
        nonStaticImports =
            List.map (\imp -> ( imp, False )) imports
    in
        case builder of
            BuildFile file ->
                BuildFile { file | imports = nonStaticImports :: file.imports }

            x ->
                x


{-| Adds a list of static imports to a file.
-}
importStatics : List String -> Attribute
importStatics imports builder =
    let
        staticImports =
            List.map (\imp -> ( imp, True )) imports
    in
        case builder of
            BuildFile file ->
                BuildFile { file | imports = staticImports :: file.imports }

            x ->
                x


{-| Adds a comment to a class, initializer block, method or constructor.
-}
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

            BuildConstructor consBuilder ->
                BuildConstructor <| consBuilder >> (addComment val)

            BuildMethod method ->
                BuildMethod <| addComment val method

            x ->
                x


{-| Sets the 'public' access modifier.
-}
public : Attribute
public builder =
    accessModifier Public builder


{-| Sets the 'protected' access modifier.
-}
protected : Attribute
protected builder =
    accessModifier Protected builder


{-| Sets the 'private' access modifier.
-}
private : Attribute
private builder =
    accessModifier Private builder


{-| Adds the 'static' modifier.
-}
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


{-| Adds the 'final' modifier.
-}
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


{-| Adds the 'volatile' modifier.
-}
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


{-| Adds the 'abstract' modifier.
-}
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


{-| Adds the 'synchronized' modifier.
-}
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


{-| Adds a class that is being extended.
-}
extends : String -> Attribute
extends val builder =
    case builder of
        BuildClass class ->
            BuildClass { class | extends = Just val }

        x ->
            x


{-| Adds a list of interfaces that are being implemented.
-}
implements : List String -> Attribute
implements implements builder =
    case builder of
        BuildClass class ->
            BuildClass { class | implements = implements }

        x ->
            x


{-| Adds arguments to a method or constructor.
-}
args : List ( String, String, List Annotation ) -> Attribute
args args builder =
    case builder of
        BuildMethod method ->
            BuildMethod { method | args = args }

        BuildConstructor consBuilder ->
            BuildConstructor <| consBuilder >> (\method -> { method | args = args })

        x ->
            x


{-| Creates an argument with no annotation
-}
arg : ( String, String ) -> ( String, String, List Annotation )
arg ( jType, name ) =
    ( jType, name, [] )


{-| Creates an argument with annotations
-}
annArg : ( String, String, List Annotation ) -> ( String, String, List Annotation )
annArg =
    identity


{-| Sets a return type on a method.
-}
returnType : String -> Attribute
returnType returnType builder =
    case builder of
        BuildMethod method ->
            BuildMethod { method | returnType = Just returnType }

        x ->
            x


{-| Adds a list of exceptions that can be thrown to a method or constructor.
-}
throws : List String -> Attribute
throws exceptions builder =
    case builder of
        BuildMethod method ->
            BuildMethod { method | throws = exceptions }

        BuildConstructor consBuilder ->
            BuildConstructor <| consBuilder >> (\method -> { method | throws = exceptions })

        x ->
            x


{-| Sets the intial value of a field.
-}
initialValue : String -> Attribute
initialValue val builder =
    case builder of
        BuildField field ->
            BuildField { field | initialValue = Just val }

        x ->
            x


{-| Defines an annotation.
-}
annotation : String -> List AnnotationBuilder -> Annotation
annotation name annBuilders =
    List.foldl (\annBuilder -> \annotation -> annBuilder annotation)
        { defaultAnnotation | name = name }
        annBuilders


{-| Adds a list of annotations inside a parent annotation.
-}
annotationList : List Annotation -> AnnotationBuilder
annotationList annotations annotation =
    { annotation | args = List.append annotation.args [ ( Nothing, AnnExpAnnotations annotations ) ] }


{-| Adds an argument to an annotation.
-}
annotationNameValue : String -> String -> AnnotationBuilder
annotationNameValue name value annotation =
    { annotation | args = List.append annotation.args [ ( Just name, AnnExprString value ) ] }


{-| Adds a default argument to an annotation.
-}
annotationDefaultValue : String -> AnnotationBuilder
annotationDefaultValue value annotation =
    { annotation | args = List.append annotation.args [ ( Nothing, AnnExprString value ) ] }


{-| Annotates a class, field, constructor or method.
-}
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

            BuildConstructor consBuilder ->
                BuildConstructor <| consBuilder >> (addAnnotations annotations)

            x ->
                x



-- ====
-- Part2 - Statements and expressions.


{-| Variables
-}
var : String -> Expr
var name =
    ExprLit name


{-| String literals
-}
string : String -> Expr
string val =
    ExprStringLit val



-- float
-- double
-- char
-- long
-- true/false
-- array initializers


{-| Int literals
-}
int : Int -> Expr
int x =
    ExprLit <| toString x



--while
--do/while
-- foreach


{-| For loops
-}
for : ( Statement, Expr, Expr ) -> List Statement -> Statement
for ( init, check, step ) body =
    For init check step body



-- if elseif else
-- ternary
-- switch/case
-- break
-- continue
-- labels
-- synchronized


{-| Assignments
-}
assign : String -> Expr -> Statement
assign var expr =
    Assign var expr



-- try/catch/finally - try with resources
-- throw
-- assert
-- math


{-| Less than
-}
lt : Expr -> Expr -> Expr
lt left right =
    ExprBinary "<" left right


{-| Plus operator.
-}
plus : Expr -> Expr -> Expr
plus left right =
    ExprBinary "+" left right


{-| Method invocation
-}
invoke : String -> List Expr -> Statement
invoke method args =
    Invoke method args



-- unary


{-| Post-inrement
-}
postIncr : String -> Expr
postIncr var =
    ExprLit <| var ++ "++"



-- new
-- generics with new
-- generics with method calls
-- super
-- this


{-| Return
-}
return : Expr -> Statement
return expr =
    Return expr



-- instanceof
-- casts


{-| Inline code comments.
-}
codeComment : String -> Statement
codeComment comment =
    Comment comment
