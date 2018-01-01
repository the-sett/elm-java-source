module JavaBean exposing (..)

import Html exposing (Html, div, text, pre)
import JavaBuilder exposing (..)
import JavaPrint exposing (javaSourceToString)
import String.Case exposing (..)


-- Default values in annotations, get rid of base " = "
-- Ordering of class contents: fields, init blocks, constructors, publid, protected, private methods,
--     inner classes.
-- @author, @param, @throws etc in javadocs.


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
                , annotation "JsonInclude" [ annotationNameValue "" "Include.NON_NULL" ]
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


getter fieldName jtype =
    method ("get" ++ (ccu fieldName))
        [ comment ("Gets the " ++ fieldName ++ ".")
        , public
        , returnType jtype
        ]
        [ return (var "id")
        ]


setter fieldName jtype =
    method ("set" ++ (ccu fieldName))
        [ comment ("Sets the " ++ fieldName ++ ".")
        , public
        , returnType "void"
        , args [ arg ( jtype, fieldName ) ]
        ]
        [ assign ("this." ++ fieldName) (var fieldName)
        ]
