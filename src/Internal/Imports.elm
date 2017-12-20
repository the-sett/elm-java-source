module Internal.Imports exposing (sortUniqueAndGroup)

import List.Extra as LE


sortUniqueAndGroup : List (List ( String, Bool )) -> List (List ( String, Bool ))
sortUniqueAndGroup stringList =
    List.concat stringList
        |> List.map (split 2)
        |> sortByGroup
        |> group
        |> List.map sortByRemainder
        |> List.map uniqueByRemainder
        |> List.map (List.map (\( group, remainder, isStatic ) -> ( group ++ "." ++ remainder, isStatic )))


{-| Splits an import up into a group by depth and a reamainder part.
For example to depth 2, the import "java.util.List" is split into
("java.util", "List").
If an import is not long enough for the specified depth, the group part is
always shortened so that there is a reaminder. For example to depth 3,
"java.util.List" would still be ("java.util", "List").
-}
split : Int -> ( String, Bool ) -> ( String, String, Bool )
split depth ( imp, isStatic ) =
    let
        splits =
            (String.split "." imp)

        ( groupList, remainderList ) =
            LE.splitAt (min depth ((List.length splits) - 1)) splits
    in
        ( String.join "." groupList, String.join "." remainderList, isStatic )


group : List ( String, String, Bool ) -> List (List ( String, String, Bool ))
group imps =
    LE.groupWhile (\( group1, _, _ ) -> \( group2, _, _ ) -> group1 == group2) imps


sortByRemainder : List ( String, String, Bool ) -> List ( String, String, Bool )
sortByRemainder imps =
    List.sortWith
        (\( _, remainder1, isStatic1 ) ->
            \( _, remainder2, isStatic2 ) ->
                case ( isStatic1, isStatic2 ) of
                    ( True, False ) ->
                        GT

                    ( False, True ) ->
                        LT

                    ( _, _ ) ->
                        compare remainder1 remainder2
        )
        imps


uniqueByRemainder : List ( String, String, Bool ) -> List ( String, String, Bool )
uniqueByRemainder imps =
    LE.uniqueBy (\( _, remainder, _ ) -> remainder) imps


sortByGroup : List ( String, String, Bool ) -> List ( String, String, Bool )
sortByGroup imps =
    List.sortBy (\( group, _, _ ) -> group) imps
