module Internal.JavaModel
    exposing
        ( JavaFile
        , Class
        , Method
        , Field
        , Initializer
        , Annotation
        , AnnotationExpr(..)
        , Statement(..)
        , Member(..)
        , Modifiers
        , AccessModifier(..)
        )


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
