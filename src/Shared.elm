module Shared exposing
    ( Bucket
    , Cents
    , Expense
    , Flags
    , Model
    , Msg(..)
    , Transfer
    , bucketTransfers
    , init
    , solidColorToColor
    , subscriptions
    , totalNetCents
    , update
    )

import Dict exposing (Dict)
import Element
import Json.Decode as Json
import Model.YearMonth
import Request exposing (Request)
import SolidColor
import Time exposing (Posix)


type alias Flags =
    Json.Value


type alias Cents =
    Int


type alias Model =
    { buckets : Dict String Bucket
    , transfers : List Transfer
    , currentYearMonth : Model.YearMonth.YearMonth
    }


type alias Transfer =
    { id : String
    , fromBucketId : String
    , toBucketId : String
    , amount : Cents
    , timeCreated : Posix
    }


type alias Bucket =
    { id : String
    , initialAmount : Cents
    , expenses : Dict String Expense
    }


type alias Expense =
    { id : String
    , description : String
    , cost : Cents
    , timeCreated : Posix
    , yearMonth : Model.YearMonth.YearMonth
    }


bucketTransfers : String -> List Transfer -> { out : List Transfer, in_ : List Transfer }
bucketTransfers bucketId transfers =
    let
        allTransfers =
            List.map
                (\transfer ->
                    if transfer.fromBucketId == bucketId then
                        { out = [ transfer ], in_ = [] }

                    else if transfer.toBucketId == bucketId then
                        { out = [], in_ = [ transfer ] }

                    else
                        { out = [], in_ = [] }
                )
                transfers
    in
    { out =
        allTransfers
            |> List.concatMap .out
    , in_ =
        allTransfers
            |> List.concatMap .in_
    }


totalNetCents : Bucket -> List Transfer -> Int
totalNetCents bucket transfers =
    let
        { out, in_ } =
            bucketTransfers bucket.id transfers
    in
    bucket.initialAmount
        + (in_
            |> List.map .amount
            |> List.sum
          )
        - (out
            |> List.map .amount
            |> List.sum
          )
        - (Dict.values bucket.expenses
            |> List.map .cost
            |> List.sum
          )


type Msg
    = UpsertExpense String String Time.Posix { description : String, cost : Cents, yearMonth : Model.YearMonth.YearMonth }
    | DeleteExpense String String


init : Request -> Flags -> ( Model, Cmd Msg )
init _ _ =
    ( { buckets =
            [ "wedding"
            , "car"
            , "house"
            , "emergency"
            , "car insurance"
            , "house #2"
            , "invisalign"
            , "poop"
            ]
                |> List.indexedMap
                    (\i id ->
                        ( id
                        , { id = id
                          , initialAmount = (i + 1) * 100000
                          , expenses =
                                Dict.fromList
                                    (List.range 0 (i * 2)
                                        |> List.map
                                            (\j ->
                                                let
                                                    expenseId =
                                                        String.left (j * 2) ("Dummy-Transactionnnnnnnnnnnnnnnnnnnnn-" ++ String.fromInt j)
                                                in
                                                ( expenseId
                                                , { id = expenseId
                                                  , description = "Description of " ++ expenseId
                                                  , cost =
                                                        j
                                                            * 10100
                                                            * (if modBy 2 j == 0 then
                                                                -1

                                                               else
                                                                1
                                                              )
                                                  , yearMonth = Model.YearMonth.fromMonthAndYear Time.Apr 2025
                                                  , timeCreated = Time.millisToPosix 1744508328823
                                                  }
                                                )
                                            )
                                    )
                          }
                        )
                    )
                |> Dict.fromList
      , transfers =
            [ { id = "String"
              , fromBucketId = "wedding"
              , toBucketId = "poop"
              , amount = 123456
              , timeCreated = Time.millisToPosix 1744508328823
              }
            ]
      , currentYearMonth = Model.YearMonth.fromMonthAndYear Time.Jul 2025
      }
    , Cmd.none
    )


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        UpsertExpense bucketId expenseId currentTime formExpense ->
            ( { model
                | buckets =
                    model.buckets
                        |> Dict.update bucketId
                            (\bucket ->
                                -- TODO error on non-existent bucket/expense
                                bucket
                                    |> Maybe.map
                                        (\existingBucket ->
                                            { existingBucket
                                                | expenses =
                                                    existingBucket.expenses
                                                        |> Dict.update expenseId
                                                            (\expense ->
                                                                case expense of
                                                                    Nothing ->
                                                                        Just
                                                                            { id = expenseId
                                                                            , description = formExpense.description
                                                                            , cost = formExpense.cost
                                                                            , yearMonth = formExpense.yearMonth
                                                                            , timeCreated = currentTime
                                                                            }

                                                                    Just oldExpense ->
                                                                        Just
                                                                            { id = oldExpense.id
                                                                            , description = formExpense.description
                                                                            , cost = formExpense.cost
                                                                            , yearMonth = formExpense.yearMonth
                                                                            , timeCreated = oldExpense.timeCreated
                                                                            }
                                                            )
                                            }
                                        )
                            )
              }
            , Cmd.none
            )

        DeleteExpense bucketId expenseId ->
            ( { model
                | buckets =
                    model.buckets
                        |> Dict.update bucketId
                            (\bucket ->
                                bucket
                                    |> Maybe.map
                                        (\existingBucket ->
                                            { existingBucket
                                                | expenses =
                                                    Dict.remove expenseId existingBucket.expenses
                                            }
                                        )
                            )
              }
            , Cmd.none
            )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none


solidColorToColor : SolidColor.SolidColor -> Element.Color
solidColorToColor =
    SolidColor.toRGB >> (\( r, g, b ) -> Element.rgb (r / 255.0) (g / 255.0) (b / 255.0))
