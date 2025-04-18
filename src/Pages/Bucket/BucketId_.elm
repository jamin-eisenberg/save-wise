module Pages.Bucket.BucketId_ exposing (page)

import Dict
import Element
import Element.Font as Font
import Gen.Params.Bucket.BucketId_ exposing (Params)
import Gen.Route as Route
import Model.YearMonth
import Page exposing (Page)
import Palette.X11 as X11
import Request
import Shared
import Time exposing (Posix)
import Url
import View exposing (View)
import Views.BucketBreakdown
import Views.Money


page : Shared.Model -> Request.With Params -> Page
page shared req =
    let
        selectedBucket =
            req.params.bucketId
                |> Url.percentDecode
                |> Maybe.andThen (\id -> Dict.get id shared.buckets)
                |> Maybe.withDefault { id = "Not Found", initialAmount = 0, expenses = Dict.empty }

        -- TODO reroute home
    in
    Page.static
        { view = view selectedBucket shared.buckets shared.transfers }


view : Shared.Bucket -> Dict.Dict String Shared.Bucket -> List Shared.Transfer -> View msg
view bucket buckets transfers =
    { title = bucket.id
    , element =
        Element.column
            [ Font.size 80, Font.center, Element.centerX, Element.width Element.fill, Element.paddingXY 0 20 ]
            [ Element.paragraph [] [ Element.text (Views.Money.format { dollarSign = True, cents = False, alwaysSign = False } (Shared.totalNetCents bucket transfers)) ]
            , Views.BucketBreakdown.view (Dict.values buckets) transfers (Just bucket.id)
            , viewSubtotals bucket.id (Dict.values bucket.expenses) transfers
            , viewTransactions bucket transfers
            ]
    }


viewSubtotals : String -> List Shared.Expense -> List Shared.Transfer -> Element.Element msg
viewSubtotals bucketId transactions transfers =
    let
        ( expensesWithdrawn, expensesDeposited ) =
            transactions
                |> List.map .cost
                |> List.partition ((>) 0)

        bucketTransfers =
            Shared.bucketTransfers bucketId transfers
    in
    Element.column [ Element.width Element.fill ]
        [ Element.paragraph [ Font.size 20 ] [ Element.text "Expenses" ]
        , Element.row [ Font.size 40, Font.center, Element.width Element.fill, Element.paddingXY 0 15 ]
            [ Element.paragraph [ Font.color (transactionColor 1) ]
                [ Element.text (Views.Money.format { dollarSign = True, cents = False, alwaysSign = True } (List.sum expensesDeposited)) ]
            , Element.paragraph [ Font.color (transactionColor -1) ]
                [ Element.text (Views.Money.format { dollarSign = True, cents = False, alwaysSign = True } (List.sum expensesWithdrawn)) ]
            ]
        , Element.paragraph [ Font.size 20 ] [ Element.text "Transfers" ]
        , Element.row [ Font.size 40, Font.center, Element.width Element.fill, Element.paddingXY 0 15 ]
            [ Element.paragraph [ Font.color (transactionColor 1) ]
                [ Element.text
                    (Views.Money.format { dollarSign = True, cents = False, alwaysSign = True }
                        (bucketTransfers.in_
                            |> List.map .amount
                            |> List.sum
                        )
                    )
                ]
            , Element.paragraph [ Font.color (transactionColor -1) ]
                [ Element.text
                    (Views.Money.format { dollarSign = True, cents = False, alwaysSign = True }
                        (bucketTransfers.out
                            |> List.map .amount
                            |> List.sum
                            |> (*) -1
                        )
                    )
                ]
            ]
        ]


type alias Transaction =
    { href : String
    , depositAmount : Shared.Cents
    , description : String
    , yearMonth : Model.YearMonth.YearMonth
    , timeCreated : Posix
    }


viewTransactions : Shared.Bucket -> List Shared.Transfer -> Element.Element msg
viewTransactions bucket transfers =
    let
        expenseTransactions : List Transaction
        expenseTransactions =
            Dict.values bucket.expenses
                |> List.map
                    (\expense ->
                        { href =
                            Route.toHref
                                (Route.Bucket__BucketId___Expense__ExpenseId_
                                    { bucketId = Url.percentEncode bucket.id, expenseId = Url.percentEncode expense.id }
                                )
                        , depositAmount = -expense.cost
                        , description = expense.description
                        , yearMonth = expense.yearMonth
                        , timeCreated = expense.timeCreated
                        }
                    )

        { out, in_ } =
            Shared.bucketTransfers bucket.id transfers

        transferToTransaction withdrawal transfer =
            { href =
                Route.toHref
                    (Route.Transfer__TransferId_
                        { transferId = Url.percentEncode transfer.id }
                    )
            , depositAmount =
                if withdrawal then
                    -transfer.amount

                else
                    transfer.amount
            , description = "Transfer from " ++ transfer.fromBucketId ++ " to " ++ transfer.toBucketId
            , timeCreated = transfer.timeCreated
            , yearMonth = transfer.timeCreated |> Model.YearMonth.fromPosix
            }

        transferTransactions : List Transaction
        transferTransactions =
            (out
                |> List.map (transferToTransaction True)
            )
                ++ (in_
                        |> List.map (transferToTransaction False)
                   )

        transactions : List Transaction
        transactions =
            (expenseTransactions
                ++ transferTransactions
            )
                |> List.sortBy (.timeCreated >> Time.posixToMillis)
    in
    Element.table [ Element.padding 10, Element.width Element.fill, Font.size 18 ]
        { data = transactions
        , columns =
            [ { header = Element.none
              , width = Element.shrink
              , view =
                    \transaction ->
                        wrapWithLink
                            transaction.href
                            (Element.paragraph
                                [ Font.color (transactionColor transaction.depositAmount)
                                , Font.center
                                ]
                                [ Element.text
                                    (if transaction.depositAmount >= 0 then
                                        "+"

                                     else
                                        "-"
                                    )
                                ]
                            )
              }
            , { header = Element.none
              , width = Element.shrink
              , view =
                    \transaction ->
                        wrapWithLink
                            transaction.href
                            (Element.paragraph
                                [ Font.color (transactionColor transaction.depositAmount)
                                , Font.alignLeft
                                ]
                                [ Element.text (Views.Money.format { dollarSign = True, cents = True, alwaysSign = False } (abs transaction.depositAmount)) ]
                            )
              }
            , { header = Element.none
              , width = Element.fill
              , view = \transaction -> wrapWithLink transaction.href (Element.paragraph [ Font.alignLeft, Element.padding 10 ] [ Element.text transaction.description ])
              }
            , { header = Element.none
              , width = Element.shrink
              , view = \transaction -> wrapWithLink transaction.href (Element.paragraph [ Font.alignRight ] [ Element.text (Model.YearMonth.displayMMYY transaction.yearMonth) ])
              }
            ]
        }


wrapWithLink href el =
    Element.link [ Element.centerY, Element.width Element.fill, Element.height Element.fill ]
        { url = href
        , label = el
        }


transactionColor depositAmount =
    Shared.solidColorToColor
        (if depositAmount >= 0 then
            X11.green

         else
            X11.red
        )
