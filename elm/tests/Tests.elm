module Tests exposing (..)

import SelectList exposing (SelectList, fromLists)
import SelectList.Extra exposing (step, StepDirection(..), fromList, selectAt)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


stepTests : Test
stepTests =
    let
        sList =
            fromLists [ "a", "b", "c" ] "d" [ "e" ]

        dupSList =
            fromLists [ "e", "b", "c" ] "d" [ "e" ]
    in
        describe "The step function"
            [ test "step forward" <|
                \_ ->
                    let
                        newSList =
                            step Forwards sList

                        expected =
                            fromLists [ "a", "b", "c", "d" ] "e" []
                    in
                        Expect.equal newSList expected
            , test "step forward into invalid state returns same sList" <|
                \_ ->
                    let
                        newSList =
                            step Forwards <| step Forwards sList

                        expected =
                            fromLists [ "a", "b", "c", "d" ] "e" []
                    in
                        Expect.equal newSList expected
            , test "step backward" <|
                \_ ->
                    let
                        newSList =
                            step Backwards sList

                        expected =
                            fromLists [ "a", "b" ] "c" [ "d", "e" ]
                    in
                        Expect.equal newSList expected
            , test "step backward into invalid state returns same sList" <|
                \_ ->
                    let
                        newSList =
                            step Backwards <| step Backwards <| step Backwards <| step Backwards <| sList

                        expected =
                            fromLists [] "a" [ "b", "c", "d", "e" ]
                    in
                        Expect.equal newSList expected
            , test "step with duplicates" <|
                \_ ->
                    let
                        newSList =
                            step Forwards dupSList

                        expected =
                            fromLists [ "e", "b", "c", "d" ] "e" []
                    in
                        Expect.equal newSList expected
            ]


fromListTests : Test
fromListTests =
    let
        lst =
            [ "a", "b", "c", "d" ]
    in
        describe "The fromList function"
            [ mkFromListTest "middle element" "b" lst <| Just <| fromLists [ "a" ] "b" [ "c", "d" ]
            , mkFromListTest "first element" "a" lst <| Just <| fromLists [] "a" [ "b", "c", "d" ]
            , mkFromListTest "last element" "d" lst <| Just <| fromLists [ "a", "b", "c" ] "d" []
            , mkFromListTest "non extant element returns nothing " "z" lst Nothing
            , mkFromListTest "singleton" "a" [ "a" ] <| Just <| fromLists [] "a" []
            ]


mkFromListTest : String -> a -> List a -> Maybe (SelectList a) -> Test
mkFromListTest description element list expected =
    test description <|
        \_ ->
            let
                sList =
                    fromList element list
            in
                Expect.equal sList expected


selectAtTests : Test
selectAtTests =
    let
        sList =
            fromLists [ "a", "b", "c" ] "d" [ "e" ]

        singleton =
            fromLists [] "a" []

        startSelected =
            fromLists [] "a" [ "b", "c", "d", "e" ]

        endSelected =
            fromLists [ "a", "b", "c", "d" ] "e" []
    in
        describe "selectAt function"
            [ mkSelectAtTest "select middle" 2 sList <|
                fromLists [ "a", "b" ] "c" [ "d", "e" ]
            , mkSelectAtTest "select when start selected" 3 startSelected <|
                fromLists [ "a", "b", "c" ] "d" [ "e" ]
            , mkSelectAtTest "select when end selected" 2 endSelected <|
                fromLists [ "a", "b" ] "c" [ "d", "e" ]
            , mkSelectAtTest "singleton" 0 singleton singleton
            , mkSelectAtTest "invalid index" -1 sList sList
            ]


mkSelectAtTest : String -> Int -> SelectList a -> SelectList a -> Test
mkSelectAtTest description element sList expected =
    test description <|
        \_ ->
            let
                newSList =
                    selectAt element sList
            in
                Expect.equal newSList expected
