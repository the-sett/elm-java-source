module Internal.JavaModel
    exposing
        ( JavaSource(..)
        , JavaFile
        , Class
        , Method
        , Field
        , Initializer
        , JavaDoc
        , Annotation
        , AnnotationExpr(..)
        , Member(..)
        , Modifiers
        , AccessModifier(..)
        , Expr(..)
        , Statement(..)
        )


type JavaSource
    = JavaSource JavaFile


type alias JavaFile =
    { header : Maybe String
    , package : String
    , imports : List (List ( String, Bool ))
    , classes : List Class
    }


type alias Class =
    { comment : Maybe JavaDoc
    , annotations : List Annotation
    , accessModifier : Maybe AccessModifier
    , modifiers : Maybe Modifiers
    , name : String
    , extends : Maybe String
    , implements : List String
    , members : List Member
    }


type alias Method =
    { comment : Maybe JavaDoc
    , annotations : List Annotation
    , accessModifier : Maybe AccessModifier
    , modifiers : Maybe Modifiers
    , name : String
    , returnType : Maybe String
    , args : List ( String, String, List Annotation )
    , throws : List String
    , body : List Statement
    }


type alias Field =
    { comment : Maybe JavaDoc
    , annotations : List Annotation
    , accessModifier : Maybe AccessModifier
    , modifiers : Maybe Modifiers
    , name : String
    , fieldType : String
    , initialValue : Maybe String
    }


type alias JavaDoc =
    { text : String
    , tags : List ( String, List String )
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


type Expr
    = ExprStringLit String
    | ExprLit String
    | ExprBinary String Expr Expr


type Statement
    = For Statement Expr Expr (List Statement)
    | Assign String Expr
    | Invoke String (List Expr)
    | Return Expr
    | Comment String
