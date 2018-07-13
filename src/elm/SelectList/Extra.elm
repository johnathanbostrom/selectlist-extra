module SelectList.Extra exposing 
    ( step
    , fromList
    , selectAt
    , StepDirection(Forwards,Backwards)
    )

{-| A library of SelectList helpers.

# Helpers
@docs step, fromList, selectAt

# Types
@docs StepDirection
-}

import SelectList exposing (SelectList)
import List.Extra exposing (splitWhen, getAt, splitAt)

{-| Step your selection forwards (increment by one) or backwards (decrement by one) through your list.
If there are no more elements in the step direction, returns an unmodified select list.

    sList = fromLists ["a", "b", "c"] "d" []
    
    step Forwards sList -- ["a", "b", "c"] "d" []
    step Backwards sList  -- ["a", "b"] "c" ["d"]
 -}
step : StepDirection -> SelectList a -> SelectList a
step direction sList =
    let
        newSelected =
            case direction of
                Forwards ->
                    List.head (SelectList.after sList)

                Backwards ->
                    List.Extra.last (SelectList.before sList)

        newSelected_ =
            Maybe.withDefault (SelectList.selected sList) newSelected
    in
        SelectList.select ((==) newSelected_) sList

{-| Build a select list from a single list and a selected element. 

    sList = fromList "b" ["a", "b", "c"]  --["a"] "b" ["c"]
 -}
fromList : a -> List a -> Maybe (SelectList a)
fromList selected lst =
    case splitWhen ((==) selected) lst of
        Just ( before, after ) ->
            Just <|
                SelectList.fromLists
                    before
                    selected
                    (List.drop 1 after)
        Nothing ->
            Nothing


{-| Select the element at the given index. 
returns unchaged select list if index is invalid.

    sList = fromLists ["a", "b", "c"] "d" []

    selectAt -1 sList  -- ["a", "b", "c"] "d" []
    selectAt 1 sList  -- ["a"] "b" ["c", "d"]
 -}
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
                        selected_
                        (List.drop 1 after)
            Nothing ->
                sLst

{-| Step direction used with step function. 
-}
type StepDirection
    = Forwards
    | Backwards