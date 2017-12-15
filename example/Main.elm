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
