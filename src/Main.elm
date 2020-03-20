module Main exposing (main)

import Browser
import Html exposing (Html, div, i, input, li, main_, text, ul)
import Html.Attributes exposing (class, placeholder)
import Html.Events exposing (onClick, onInput)



-- MAIN


main : Program (List USState) Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias USState =
    { name : String
    , abbreviation : String
    }


type alias Model =
    { leftSearchWord : String
    , rightSearchWord : String
    , leftListbox : List USState
    , rightListbox : List USState
    }


init : List USState -> ( Model, Cmd Msg )
init allUsStateList =
    ( { leftSearchWord = ""
      , rightSearchWord = ""
      , leftListbox = allUsStateList
      , rightListbox = []
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = MoveRight USState
    | MoveLeft USState
    | MoveAllRight
    | MoveAllLeft
    | ExchangeList
    | UpdateLeftSearchWord String
    | UpdateRightSearchWord String


deleteItemFromList : a -> List a -> List a
deleteItemFromList deletedItem list =
    List.filter (\item -> item /= deletedItem) list


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { leftListbox, rightListbox } =
            model
    in
    case msg of
        UpdateLeftSearchWord word ->
            ( { model | leftSearchWord = word }, Cmd.none )

        UpdateRightSearchWord word ->
            ( { model | rightSearchWord = word }, Cmd.none )

        MoveRight usState ->
            ( { model
                | leftListbox =
                    deleteItemFromList usState model.leftListbox
                , rightListbox = rightListbox ++ [ usState ]
              }
            , Cmd.none
            )

        MoveLeft usState ->
            ( { model
                | leftListbox =
                    leftListbox ++ [ usState ]
                , rightListbox =
                    deleteItemFromList usState rightListbox
              }
            , Cmd.none
            )

        MoveAllRight ->
            ( { model
                | leftListbox =
                    []
                , rightListbox =
                    rightListbox ++ leftListbox
              }
            , Cmd.none
            )

        MoveAllLeft ->
            ( { model
                | leftListbox =
                    leftListbox ++ rightListbox
                , rightListbox =
                    []
              }
            , Cmd.none
            )

        ExchangeList ->
            ( { model
                | leftListbox = rightListbox
                , rightListbox = leftListbox
              }
            , Cmd.none
            )



-- VIEW


type BoxDirection
    = Left
    | Right


view : Model -> Html Msg
view model =
    let
        { leftSearchWord, leftListbox, rightSearchWord, rightListbox } =
            model
    in
    main_ [ class "ly_cont" ]
        [ div [ class "bl_dualListbox" ]
            [ searchableList Left leftSearchWord leftListbox
            , ul [ class "bl_actionList" ]
                [ li [ onClick MoveAllLeft ] [ i [ class "el_icon fas fa-angle-double-left el_icon__green" ] [] ]
                , li [ onClick ExchangeList ] [ i [ class "el_icon fas fa-exchange-alt" ] [] ]
                , li [ onClick MoveAllRight ] [ i [ class "el_icon fas fa-angle-double-right el_icon__green" ] [] ]
                ]
            , searchableList Right rightSearchWord rightListbox
            ]
        ]


searchableList : BoxDirection -> String -> List USState -> Html Msg
searchableList direction searchWord usStateList =
    let
        searchPlaceholder =
            case direction of
                Left ->
                    "Search States"

                Right ->
                    "Search selected States"

        updateSearchWord =
            case direction of
                Left ->
                    UpdateLeftSearchWord

                Right ->
                    UpdateRightSearchWord

        moveItem usState =
            case direction of
                Left ->
                    MoveRight usState

                Right ->
                    MoveLeft usState
    in
    div [ class "bl_listbox" ]
        [ input [ class "el_input", placeholder searchPlaceholder, onInput updateSearchWord ] []
        , ul [ class "el_list" ] <|
            (usStateList
                |> List.filter
                    (\usState ->
                        String.contains (String.toLower searchWord) (String.toLower usState.name)
                            || String.contains (String.toLower searchWord) (String.toLower usState.abbreviation)
                    )
                |> List.map (\usState -> li [ onClick <| moveItem usState ] [ text usState.name ])
            )
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
