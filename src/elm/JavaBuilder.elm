module JavaBuilder
    exposing
        ( JavaSource
        , file
        , class
        , field
        , initializer
        , constructor
        , method
        , statement
        , header
        , package
        , imports
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
        , returnType
        , throws
        , annotation
        , annotationList
        , annotationNameValue
        , annotate
        )

import Internal.JavaModel exposing (..)
import Internal.JavaSourceModel as JavaSourceModel


type alias JavaSource =
    JavaSourceModel.JavaSource


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


file : List Attribute -> List Builder -> JavaSource
file attrs builders =
    List.foldl (\attr -> \builder -> attr builder) (BuildFile defaultJavaFile) attrs
        |> extractFile
        |> Maybe.withDefault defaultJavaFile
        |> (\file -> { file | classes = List.filterMap extractClass builders })
        |> JavaSourceModel.JavaSource


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
