module Internal.MemberOrdering exposing (sortMembers)

{-| Defines a standard ordering of class contents: fields, init blocks,
constructors, methods, inner classes. Fields, constructors and methods are
ordered: package, public, protected, private.
-}

import Internal.JavaModel as JavaModel exposing (Member(..), AccessModifier(..))
import List


sortMembers : List Member -> List Member
sortMembers members =
    List.sortBy memberToComparable members


memberToComparable : Member -> comparable
memberToComparable member =
    case member of
        MClass class ->
            40 + (accessModifierToComparable class.accessModifier)

        MField field ->
            0 + (accessModifierToComparable field.accessModifier)

        MInitializer initializer ->
            10

        MConstructor method ->
            20 + (accessModifierToComparable method.accessModifier)

        MMethod method ->
            30 + (accessModifierToComparable method.accessModifier)


accessModifierToComparable : Maybe AccessModifier -> comparable
accessModifierToComparable modifier =
    case modifier of
        Just accessModifier ->
            case accessModifier of
                Private ->
                    3

                Protected ->
                    2

                Public ->
                    1

        Nothing ->
            0
