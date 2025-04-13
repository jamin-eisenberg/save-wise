module Shared exposing
    ( Bucket
    , Cents
    , Expense
    , Flags
    , Model
    , Msg
    , Transfer
    , bucketTransfers
    , init
    , solidColorToColor
    , subscriptions
    , totalNetCents
    , update
    )

import Date
import Dict exposing (Dict)
import Element
import Json.Decode as Json
import Request exposing (Request)
import SolidColor


type alias Flags =
    Json.Value


type alias Cents =
    Int


type alias Model =
    { buckets : Dict String Bucket
    , transfers : List Transfer
    }


type alias Transfer =
    { id : String
    , fromBucketId : String
    , toBucketId : String
    , amount : Cents
    , date : Date.Date
    }


type alias Bucket =
    { id : String
    , initialAmount : Cents
    , expenses : List Expense
    }


type alias Expense =
    { id : String
    , description : String
    , cost : Cents
    , date : Date.Date
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
        - (bucket.expenses
            |> List.map .cost
            |> List.sum
          )


type Msg
    = NoOp


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
                                List.range 0 (i * 2)
                                    |> List.map
                                        (\j ->
                                            { id = String.left (j * 2) ("Dummy Transactionnnnnnnnnnnnnnnnnnnnn " ++ String.fromInt j)
                                            , description = String.left (j * 2) ("Dummy Transactionnnnnnnnnnnnnnnnnnnnn " ++ String.fromInt j)
                                            , cost =
                                                j
                                                    * 10100
                                                    * (if modBy 2 j == 0 then
                                                        -1

                                                       else
                                                        1
                                                      )
                                            , date = Date.fromRataDie 739348
                                            }
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
              , date = Date.fromRataDie 739347
              }
            ]
      }
    , Cmd.none
    )


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none


solidColorToColor : SolidColor.SolidColor -> Element.Color
solidColorToColor =
    SolidColor.toRGB >> (\( r, g, b ) -> Element.rgb (r / 255.0) (g / 255.0) (b / 255.0))
