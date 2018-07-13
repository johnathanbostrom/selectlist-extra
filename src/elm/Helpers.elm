module Helpers exposing (..)

import SelectList exposing (SelectList)
import List.Extra exposing (splitWhen, getAt, splitAt)
import Types exposing (StepDirection)


step : StepDirection -> SelectList a -> SelectList a
step direction sList =
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


fromList : a -> List a -> Maybe (SelectList a)
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

selectAt : Int -> SelectList a -> SelectList a
selectAt index sLst =
    let
        lst = SelectList.toList sLst
        selected = getAt index lst
    in
        case selected of
            Just selected_ ->
                let
                    (before, after) = splitAt index lst
                in
                    SelectList.fromLists
                        before
                        selected
                        (List.drop 1 after)
            Nothing ->
                sLst