module JavaBean exposing (..)

import Html exposing (Html, div, text, pre)
import JavaBuilder exposing (..)
import JavaPrint exposing (javaSourceToString)
import String.Case exposing (..)


main : Html Never
main =
    div []
        [ pre [] [ text <| javaSourceToString javaExample ] ]


ccu =
    toCamelCaseUpper


cc =
    toCamelCaseLower


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
                , annotation "JsonInclude" [ annotationDefaultValue "Include.NON_NULL" ]
                ]
            ]
            [ consNoArg
            , withField "id" "Long" |> comment "Holds the database surrogate id."
            , getter "id" "Long" |> comment "Gets the database surrogate id."
            , setter "id" "Long" |> comment "Sets the database surrogate id."
            ]
        ]


consNoArg =
    constructor
        [ comment "No-arg constructor for serialization."
        , public
        ]
        [ codeComment "No-arg constructor for serialization."
        ]


withField : String -> String -> Builder
withField fieldName jtype =
    field "Long"
        "id"
        [ comment "Holds the id."
        , private
        ]


getter : String -> String -> Builder
getter fieldName jtype =
    let
        ccuFieldName =
            ccu fieldName

        ccFieldName =
            cc fieldName
    in
        method ("get" ++ ccuFieldName)
            [ comment ("Gets the " ++ ccFieldName ++ ".")
            , jdocReturns <| "The " ++ ccFieldName ++ "."
            , public
            , returnType jtype
            ]
            [ return (var ccFieldName)
            ]


setter : String -> String -> Builder
setter fieldName jtype =
    let
        ccuFieldName =
            ccu fieldName

        ccFieldName =
            cc fieldName
    in
        method ("set" ++ ccuFieldName)
            [ comment ("Sets the " ++ ccFieldName ++ ".")
            , jdocParam ccFieldName "The new value to set."
            , public
            , returnType "void"
            , args [ arg ( jtype, ccFieldName ) ]
            ]
            [ assign ("this." ++ ccFieldName) (var ccFieldName)
            ]
