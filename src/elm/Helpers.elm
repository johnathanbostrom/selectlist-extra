module Helpers exposing (..)

import SelectList exposing (SelectList)
import List.Extra exposing (splitWhen)
import Types exposing (StepDirection)


step : SelectList a -> StepDirection -> SelectList a
step sList direction =
    let
        newSelected =
            case direction of
                Forward ->
                    List.head (SelectList.after sList)

                Backward ->
                    List.Extra.last (SelectList.before workList)

        newSelected_ =
            Maybe.withDefault (SelectList.selected sList) newSelected
    in
        SelectList.select ((==) newSelected_) workList


fromList : List a -> a -> Maybe (SelectList a)
fromList lst selected =
    case splitWhen ((==) selected) lst of
        Just ( before, after ) ->
            Just <|
                SelectList.fromLists
                    before
                    selected
                    (List.drop 1 after)
        Nothing ->
            Nothing
