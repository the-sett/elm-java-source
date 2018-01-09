port module Main exposing (..)

import Html exposing (Html, div, text, pre)
import JavaBuilder exposing (..)
import JavaPrint exposing (javaSourceToString)


main : Html Never
main =
    div []
        [ pre [] [ text <| javaSourceToString javaExample ] ]



-- Java Examples


javaExample : JavaSource
javaExample =
    file
        [ header "Copyright blah..."
        , package "com.thesett.example"
        , imports
            [ "org.springframework.core.Component"
            , "java.util.List"
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
                , namedQueries
                ]
            ]
            [ innerClass
            , staticField
            , mainMethod
            , staticInitBlock
            , methodArgsWithAnnotation
            , consWithArg
            ]
        ]


namedQueries =
    annotation "NamedQueries"
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


staticField =
    field "int"
        "test"
        [ comment "This is a field"
        , private >> volatile >> static
        , initialValue "0"
        ]


staticInitBlock =
    initializer
        [ comment "This is an initializer block."
        , static
        ]
        [ assign "test" (int 2) ]


consWithArg =
    constructor
        [ comment "This is a constructor"
        , public
        , args [ arg ( "int", "test" ) ]
        ]
        [ assign "this.test" (var "test") ]


mainMethod =
    method "main"
        [ comment "This is a method."
        , public >> static
        , returnType "void"
        , args [ arg ( "String[]", "args" ) ]
        , throws [ "IOException", "ClassNotFoundException" ]
        , annotate
            [ annotation "Bean" []
            , annotation "Timed" []
            , annotation "UnitOfWork" [ annotationNameValue "context" "\"Mandatory\"" ]
            ]
        ]
        []


methodArgsWithAnnotation =
    method "funkyDooDa"
        [ comment "This is a method."
        , public
        , returnType "int"
        , args [ arg ( "int", "val" ), annArg ( "String", "id", [ annotation "NotNull" [] ] ) ]
        ]
        [ forloop ]


innerClass =
    class "InnerClass"
        [ comment "This is an inner class."
        , protected >> abstract
        , implements [ "Runnable" ]
        ]
        []



-- interface
-- enum
-- generics - declaration


forloop =
    for ( assign "i" (int 0), lt (var "i") (int 20), postIncr "i" )
        [ assign "next" (plus (var "i") (int 1))
        , invoke "System.out.println" [ plus (string "i is ") (var "i") ]
        ]
