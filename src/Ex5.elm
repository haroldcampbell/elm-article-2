module Ex5 exposing (..)

import Browser
import Dict exposing (merge, update)
import Html exposing (Html, div, h3, h4, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List.Extra as LE


type alias Team =
    { id : String
    , name : String
    , boards : List Board
    }


type alias Board =
    { id : String
    , name : String
    , stages : List Stage
    }


type alias Stage =
    { id : String
    , boardID : String
    , name : String
    , cards : List Card
    }


type alias Card =
    { id : String
    , teamID : String
    , boardID : String
    , stageID : String
    , desc : String
    }


type Model
    = Model Team



-- UPDATE


type Msg
    = NoOp
    | NewCard Stage


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        NewCard stage ->
            addCardToStage stage model


addCardToStage : Stage -> Model -> Model
addCardToStage stage (Model team) =
    let
        cardID : String
        cardID =
            "c"
                ++ String.fromInt
                    (1
                        + (List.sum <|
                            List.map
                                (\board ->
                                    List.sum <|
                                        List.map (\s -> List.length s.cards) board.stages
                                )
                                team.boards
                          )
                    )

        newCard =
            { id = cardID, teamID = team.id, boardID = "?", stageID = stage.id, desc = "NEW CARD" }

        updatedBoards =
            updateIf (\board -> board.id == stage.boardID)
                (\board ->
                    { board
                        | stages =
                            updateIf (\s -> s.id == stage.id)
                                (\s -> { s | cards = s.cards ++ [ { newCard | boardID = board.id } ] })
                                board.stages
                    }
                )
                team.boards

        newTeam =
            { team | boards = updatedBoards }
    in
    Model newTeam


updateIf : (a -> Bool) -> (a -> a) -> List a -> List a
updateIf predicate updateFnc list =
    List.map
        (\item ->
            if predicate item then
                updateFnc item

            else
                item
        )
        list


updateIfIndexWithDefault : (Int -> Bool) -> (a -> c) -> (a -> c) -> List a -> List c
updateIfIndexWithDefault predicate updateFnc defaultFnc list =
    List.indexedMap
        (\index item ->
            if predicate index then
                updateFnc item

            else
                defaultFnc item
        )
        list


view : Model -> Html Msg
view (Model team) =
    div
        [ style "padding" "20px"
        ]
        [ h3
            [ style "margin-bottom" "10px"
            ]
            [ text ("Team Name: " ++ team.name) ]
        , viewBoardAt 1 team
        ]


viewBoardAt : Int -> Team -> Html Msg
viewBoardAt boardIndex team =
    div
        [ style "padding" "20px"
        ]
        (team.boards
            |> takeOnlyIndex boardIndex
            |> bHtml
        )


cInnerHtml : Card -> Html Msg
cInnerHtml item =
    viewCard item


sInnerHtml : Stage -> Html Msg
sInnerHtml item =
    viewStage item (cHtml item.cards)


bInnerHtml : Board -> Html Msg
bInnerHtml item =
    viewBoard item (sHtml item.stages)


cHtml : List Card -> List (Html Msg)
cHtml cards =
    List.map viewCard cards


sHtml : List Stage -> List (Html Msg)
sHtml stages =
    List.map sInnerHtml stages


bHtml : List Board -> List (Html Msg)
bHtml boards =
    List.map bInnerHtml boards


takeOnlyIndex boardIndex list =
    list
        |> List.indexedMap (\index item -> ( index, item ))
        |> List.filter (\( index, _ ) -> index == boardIndex)
        |> List.map (\( _, item ) -> item)


viewBoard : Board -> List (Html Msg) -> Html Msg
viewBoard board stagesHml =
    div
        []
        [ h4 [] [ text <| board.name ++ " Board" ]
        , div
            [ style "display" "flex"
            , style "flex-direction" "row"
            , style "align-items" "flex-start"
            ]
            stagesHml
        ]


viewStage : Stage -> List (Html Msg) -> Html Msg
viewStage stage cardsHtml =
    div
        [ style "display" "inline-block"
        , style "margin" "5px"
        , style "width" "150px"
        ]
        [ div
            [ style "font-weight" "bold"
            ]
            [ text stage.name ]
        , div
            [ onClick <| NewCard stage
            , style "margin" "10px 0"
            , style "font-size" "0.8em"
            , style "text-decoration" "underline"
            , style "cursor" "pointer"
            ]
            [ text "Add Card" ]
        , div
            [ style "background-color" "#f0f0f0"
            , style "padding" "5px 10px"
            , style "border-radius" "10px"
            ]
            cardsHtml
        ]


viewCard : Card -> Html Msg
viewCard card =
    div
        [ style "margin" "10px 0"
        , style "background-color" "#fff"
        , style "border-radius" "5px"
        , style "padding" "5px"
        ]
        [ span
            [ style "padding-right" "10px"
            , style "font-size" "0.75em"
            , style "color" "#ccc"
            ]
            [ text <| card.id ]
        , span [] [ text card.desc ]
        ]



-- INIT


init : Model
init =
    Model
        (Team "t1"
            "happy-people"
            [ Board "b1"
                "Development"
                [ { id = "s1"
                  , boardID = "b1"
                  , name = "Backlog"
                  , cards =
                        [ Card "c6" "t1" "b1" "s1" "fff"
                        , Card "c7" "t1" "b1" "s1" "ggg"
                        ]
                  }
                , { id = "s2"
                  , boardID = "b1"
                  , name = "In Progress"
                  , cards =
                        [ Card "c3" "t1" "b1" "s2" "ccc"
                        , Card "c4" "t1" "b1" "s2" "ddd"
                        ]
                  }
                , { id = "s3"
                  , boardID = "b1"
                  , name = "Done"
                  , cards =
                        [ Card "c1" "t1" "b1" "s3" "aaa"
                        , Card "c2" "t1" "b1" "s3" "bbb"
                        , Card "c5" "t1" "b1" "s3" "eee"
                        ]
                  }
                ]
            , Board
                "b2"
                "Research"
                [ { id = "s4"
                  , boardID = "b2"
                  , name = "Prospect"
                  , cards =
                        [ Card "c13" "t1" "b2" "s4" "Tslala Ivory"
                        ]
                  }
                , { id = "s5"
                  , boardID = "b2"
                  , name = "Planned Interviews"
                  , cards =
                        [ Card "c10" "t1" "b2" "s5" "John Gruunder"
                        , Card "c11" "t1" "b2" "s5" "Harry Bill-Murry"
                        ]
                  }
                , Stage "s6" "b2" "Scheduled Interviews" [ Card "c12" "t1" "b2" "s6" "Coven Mayeh" ]
                , Stage "s7" "b2" "Interviews" []
                , Stage "s8" "b2" "Synthesis" []
                , { id = "s9"
                  , boardID = "b2"
                  , name = "Completed"
                  , cards =
                        [ Card "c9" "t1" "b2" "s9" "Holly May"
                        , Card "c8" "t1" "b2" "s9" "Jill Smith"
                        ]
                  }
                ]
            , Board "b3"
                "Issues"
                [ { id = "s10"
                  , boardID = "b3"
                  , name = "New"
                  , cards =
                        [ Card "c14" "t1" "b3" "s10" "Missing page on load"
                        , Card "c20" "t1" "b3" "s10" "Next page link doesn't work on the search page"
                        ]
                  }
                , { id = "s11"
                  , boardID = "b3"
                  , name = "Investigation"
                  , cards =
                        [ Card "c15" "t1" "b3" "s11" "Wrong url on shopping page"
                        , Card "c16" "t1" "b3" "s11" "Logo not visible on contacts"
                        , Card "c17" "t1" "b3" "s11" "Update pricing to reflect new prices"
                        ]
                  }
                , { id = "s12"
                  , boardID = "b3"
                  , name = "In Development"
                  , cards =
                        [ Card "c18" "t1" "b3" "s12" "It doesn't work"
                        , Card "c19" "t1" "b3" "s12" "Can't click back button"
                        ]
                  }
                , Stage "s13" "b3" "Review" []
                , { id = "s14"
                  , boardID = "b3"
                  , name = "Closed"
                  , cards =
                        [ Card "c21" "t1" "b3" "s13" "Missing aside note"
                        , Card "c22" "t1" "b3" "s13" "Black Friday banner should be removed"
                        ]
                  }
                ]
            ]
        )



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
