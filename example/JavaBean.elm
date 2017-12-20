module JavaBean exposing (..)

import Html exposing (Html, div, text, pre)
import JavaBuilder exposing (..)
import JavaPrint exposing (javaSourceToString)


-- Javadoc comments on class, fields and methods.
-- @author, @param, @throws etc in javadocs.
-- Customizing comments.


main : Html Never
main =
    div []
        [ pre [] [ text <| javaSourceToString javaExample ] ]


javaExample : JavaSource
javaExample =
    file
        [ package "com.analog.metermodel.model"
        , imports
            [ "java.io.Serializable"
            , "com.fasterxml.jackson.annotation.JsonIgnoreProperties"
            , "com.fasterxml.jackson.annotation.JsonInclude"
            , "com.thesett.util.entity.Entity"
            , "io.swagger.annotations.ApiModelProperty"
            , "com.fasterxml.jackson.annotation.JsonIgnoreProperties"
            , "com.fasterxml.jackson.annotation.JsonInclude"
            , "com.thesett.util.entity.Entity"
            , "io.swagger.annotations.ApiModelProperty"
            ]
        , importStatics
            [ "com.fasterxml.jackson.annotation.JsonInclude.Include"
            ]
        ]
        [ class "SomeEntity"
            [ comment "Generated code from catalogue model."
            , public
            , implements [ "Entity<Long>", "Serializable" ]
            , annotate
                [ annotation "JsonIgnoreProperties" [ annotationNameValue "ignoreUnknown" "true" ]
                , annotation "JsonInclude" [ annotationNameValue "" "Include.NON_NULL" ]
                ]
            ]
            [ idField
            , consNoArg
            , getter "id" "Long"
            , setter "id" "Long"
            ]
        ]


idField =
    field "Long"
        "id"
        [ comment "Holds the database surrogate id."
        , private
        ]


consNoArg =
    constructor
        [ comment "No-arg constructor for serialization."
        , public
        ]
        [ codeComment "No-arg constructor for serialization."
        ]


getter field jtype =
    method ("get" ++ field)
        [ comment ("Gets the " ++ field ++ ".")
        , public
        , returnType jtype
        ]
        [ return (var "id")
        ]


setter field jtype =
    method ("set" ++ field)
        [ comment ("Sets the " ++ field ++ ".")
        , public
        , returnType "void"
        , args [ arg ( jtype, field ) ]
        ]
        [ assign ("this." ++ field) (var field)
        ]
