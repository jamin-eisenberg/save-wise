module Pages.Bucket.BucketId_.Expense.ExpenseId_ exposing (Model, Msg, page)

import Dict
import Dropdown
import Effect
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Form.Decoder as Decoder
import Gen.Params.Bucket.BucketId_.Expense.ExpenseId_ exposing (Params)
import Gen.Route
import Html.Attributes
import Model.YearMonth as YearMonth exposing (YearMonth, getMonth, getYear, monthToString)
import Page
import Palette.X11 as X11
import Request
import Shared
import Task
import Time
import Url
import View exposing (View)
import Views.Money


type alias Year =
    Int


type alias Model =
    { id : String
    , description : String
    , cost : Maybe Int
    , year : Maybe Year
    , yearDropdownState : Dropdown.State Year
    , month : Maybe Time.Month
    , monthDropdownState : Dropdown.State Time.Month
    , saveState : SaveState
    , decodeErrors : List DecodeError
    , new : Bool
    }


type Msg
    = EditDescription String
    | EditCost String
    | YearDropdownMsg (Dropdown.Msg Year)
    | EditYear (Maybe Int)
    | MonthDropdownMsg (Dropdown.Msg Time.Month)
    | EditMonth (Maybe Time.Month)
    | Delete
    | Save
    | SaveResponded Time.Posix FormExpense


type SaveState
    = NotStarted
    | Saving
    | Saved


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    let
        bucket =
            req.params.bucketId
                |> Url.percentDecode
                |> Maybe.andThen (\bucketId -> Dict.get bucketId shared.buckets)
                |> Maybe.withDefault { id = "Not Found", initialAmount = 0, expenses = Dict.empty }

        expenseId =
            req.params.expenseId
                |> Url.percentDecode
                |> Maybe.withDefault ""

        expense =
            Dict.get expenseId bucket.expenses
    in
    Page.advanced
        { init = init expenseId expense shared.currentYearMonth
        , update = update (getYear shared.currentYearMonth) req
        , view = view (getYear shared.currentYearMonth) bucket.id
        , subscriptions = \_ -> Sub.none
        }


init : String -> Maybe Shared.Expense -> YearMonth -> ( Model, Effect.Effect Msg )
init expenseId e currentYearMonth =
    ( case e of
        Nothing ->
            { id = expenseId
            , description = ""
            , cost = Nothing
            , year = Just (getYear currentYearMonth)
            , yearDropdownState = Dropdown.init "year-dropdown"
            , month = Just (getMonth currentYearMonth)
            , monthDropdownState = Dropdown.init "month-dropdown"
            , saveState = NotStarted
            , decodeErrors = []
            , new = True
            }

        Just expense ->
            { id = expense.id
            , description = expense.description
            , cost = Just expense.cost
            , year = Just (YearMonth.getYear expense.yearMonth)
            , yearDropdownState = Dropdown.init "year-dropdown"
            , month = Just (YearMonth.getMonth expense.yearMonth)
            , monthDropdownState = Dropdown.init "month-dropdown"
            , saveState = NotStarted
            , decodeErrors = []
            , new = False
            }
    , Effect.none
    )


update : Int -> Request.With Params -> Msg -> Model -> ( Model, Effect.Effect Msg )
update currentYear req msg model =
    case msg of
        EditDescription newDescription ->
            ( { model | description = newDescription }, Effect.none )

        EditCost newCostStr ->
            let
                newCostStrNeg =
                    if String.filter ((==) '-') newCostStr == "-" then
                        "-" ++ String.filter ((/=) '-') newCostStr

                    else
                        String.filter ((/=) '-') newCostStr
            in
            ( { model
                | cost =
                    newCostStrNeg
                        |> String.filter (\c -> not (List.member c [ '$', '.', ',', ' ' ]))
                        |> String.toInt
              }
            , Effect.none
            )

        MonthDropdownMsg subMsg ->
            let
                ( state, cmd ) =
                    Dropdown.update monthDropdownConfig subMsg model model.monthDropdownState
            in
            ( { model | monthDropdownState = state }, Effect.fromCmd cmd )

        EditMonth newMonth ->
            ( { model | month = newMonth }, Effect.none )

        YearDropdownMsg subMsg ->
            let
                ( state, cmd ) =
                    Dropdown.update (yearDropdownConfig currentYear) subMsg model model.yearDropdownState
            in
            ( { model | yearDropdownState = state }, Effect.fromCmd cmd )

        EditYear newYear ->
            ( { model | year = newYear }, Effect.none )

        Delete ->
            ( { model | saveState = Saving }
            , Effect.batch
                [ Request.pushRoute (Gen.Route.Bucket__BucketId_ { bucketId = req.params.bucketId }) req
                    |> Effect.fromCmd
                , Effect.fromShared (Shared.DeleteExpense req.params.bucketId req.params.expenseId)
                ]
            )

        Save ->
            -- TODO actually save to DB
            let
                decodedExpense =
                    Decoder.run formDecoder { description = model.description, cost = model.cost, year = model.year, month = model.month }
            in
            case decodedExpense of
                Ok formExpense ->
                    ( { model | saveState = Saving }
                    , Time.now
                        |> Task.perform (\currentTime -> SaveResponded currentTime formExpense)
                        |> Effect.fromCmd
                    )

                Err decodeErrors ->
                    ( { model | decodeErrors = decodeErrors }, Effect.none )

        SaveResponded currentTime formExpense ->
            ( { model | saveState = Saved }
            , Effect.batch
                [ Request.pushRoute (Gen.Route.Bucket__BucketId_ { bucketId = req.params.bucketId }) req
                    |> Effect.fromCmd
                , Effect.fromShared (Shared.UpsertExpense req.params.bucketId req.params.expenseId currentTime formExpense)
                ]
            )


view : Int -> String -> Model -> View Msg
view currentYear bucketId model =
    { title = model.description
    , floatingElements = []
    , element =
        Element.column
            [ Element.width Element.fill
            , Element.padding 20
            , Element.spacing 20
            , Font.color
                (Shared.solidColorToColor
                    (if model.saveState == Saving then
                        X11.slateGray

                     else
                        X11.black
                    )
                )
            ]
            [ Element.textColumn [ Element.spacing 10, Element.width Element.fill ]
                (List.map
                    (\error ->
                        error
                            |> decodeErrorToString
                            |> Element.text
                            |> List.singleton
                            |> Element.paragraph
                                [ Element.padding 15
                                , Border.rounded 10
                                , Background.color (Shared.solidColorToColor X11.lightPink)
                                , Font.color (Shared.solidColorToColor X11.red)
                                , Element.width Element.fill
                                ]
                    )
                    model.decodeErrors
                )
            , Element.text ("Bucket: " ++ bucketId)
            , Input.text
                []
                { onChange = EditDescription
                , text = model.description
                , placeholder = Nothing
                , label = Input.labelAbove [] (Element.text "Description")
                }
            , Input.text [ Element.htmlAttribute (Html.Attributes.attribute "inputmode" "decimal") ]
                { onChange = EditCost
                , text =
                    model.cost
                        |> Maybe.map (Views.Money.format { dollarSign = True, alwaysSign = False, cents = True })
                        |> Maybe.withDefault ""
                , placeholder = Nothing
                , label = Input.labelAbove [] (Element.text "Cost")
                }
            , Element.row [ Element.width Element.fill, Element.spacing 16 ]
                [ Element.column [ Element.width Element.fill, Element.spacing 8 ]
                    [ Element.text "Month"
                    , Dropdown.view monthDropdownConfig model model.monthDropdownState
                    ]
                , Element.column [ Element.width Element.fill, Element.spacing 8 ]
                    [ Element.text "Year"
                    , Dropdown.view (yearDropdownConfig currentYear) model model.yearDropdownState
                    ]
                ]
            , Element.row [ Element.width Element.fill, Element.spacing 10 ]
                [ Input.button
                    [ Element.width Element.fill
                    , Background.color (Shared.solidColorToColor X11.springGreen)
                    , Border.rounded 6
                    ]
                    { onPress = Just Save
                    , label =
                        Element.paragraph
                            [ Font.center, Element.padding 12 ]
                            [ Element.text "Save" ]
                    }
                , if model.new then
                    Element.none

                  else
                    Input.button
                        [ Element.width Element.fill
                        , Background.color (Shared.solidColorToColor X11.tomato)
                        , Border.rounded 6
                        ]
                        { onPress = Just Delete
                        , label =
                            Element.paragraph
                                [ Font.center, Element.padding 12 ]
                                [ Element.text "Delete" ]
                        }
                ]
            ]
    }


type alias Form =
    { description : String, cost : Maybe Shared.Cents, month : Maybe Time.Month, year : Maybe Year }


type alias FormExpense =
    { description : String, cost : Shared.Cents, yearMonth : YearMonth }


formDecoder : Decoder.Decoder Form DecodeError FormExpense
formDecoder =
    Decoder.top FormExpense
        |> Decoder.field description_
        |> Decoder.field cost_
        |> Decoder.field yearMonthDecoder


description_ =
    Decoder.lift .description
        (Decoder.identity
            |> Decoder.assert (Decoder.minLength DescriptionAbsent 1)
        )


cost_ =
    Decoder.lift .cost (presentDecoder CostAbsent)


year_ =
    Decoder.lift .year (presentDecoder YearAbsent)


month_ =
    Decoder.lift .month (presentDecoder MonthAbsent)


yearMonthDecoder =
    Decoder.map2 YearMonth.fromMonthAndYear month_ year_


presentDecoder : x -> Decoder.Decoder (Maybe a) x a
presentDecoder errorIfAbsent =
    Decoder.custom
        (\maybeA ->
            case maybeA of
                Nothing ->
                    Err [ errorIfAbsent ]

                Just a ->
                    Ok a
        )


type DecodeError
    = DescriptionAbsent
    | CostAbsent
    | MonthAbsent
    | YearAbsent


decodeErrorToString decodeError =
    case decodeError of
        DescriptionAbsent ->
            "description is missing"

        CostAbsent ->
            "cost is missing"

        MonthAbsent ->
            "month is missing"

        YearAbsent ->
            "year is missing"


monthDropdownConfig =
    Dropdown.filterable
        { itemsFromModel = \_ -> [ Time.Jan, Time.Feb, Time.Mar, Time.Apr, Time.May, Time.Jun, Time.Jul, Time.Aug, Time.Sep, Time.Oct, Time.Nov, Time.Dec ]
        , selectionFromModel = .month
        , dropdownMsg = MonthDropdownMsg
        , onSelectMsg = EditMonth
        , itemToPrompt = monthToString >> Element.text
        , itemToElement = \selected highlighted -> monthToString >> itemToElement selected highlighted
        , itemToText = monthToString
        }
        |> Dropdown.withContainerAttributes containerAttrs
        |> Dropdown.withPromptElement (Element.el [] (Element.text "Select month"))
        |> Dropdown.withFilterPlaceholder "Type for month"
        |> Dropdown.withSelectAttributes selectAttrs
        |> Dropdown.withListAttributes listAttrs
        |> Dropdown.withSearchAttributes searchAttrs


yearDropdownConfig currentYear =
    Dropdown.filterable
        { itemsFromModel = \_ -> List.range 2000 (currentYear + 1) |> List.reverse
        , selectionFromModel = .year
        , dropdownMsg = YearDropdownMsg
        , onSelectMsg = EditYear
        , itemToPrompt = String.fromInt >> Element.text
        , itemToElement = \selected highlighted -> String.fromInt >> itemToElement selected highlighted
        , itemToText = String.fromInt
        }
        |> Dropdown.withContainerAttributes containerAttrs
        |> Dropdown.withPromptElement (Element.el [] (Element.text "Select year"))
        |> Dropdown.withFilterPlaceholder "Type for year"
        |> Dropdown.withSelectAttributes selectAttrs
        |> Dropdown.withListAttributes listAttrs
        |> Dropdown.withSearchAttributes searchAttrs


containerAttrs =
    [ Element.width Element.fill ]


selectAttrs =
    [ Border.width 1, Border.color (Element.rgb255 180 180 180), Border.rounded 3, Element.paddingXY 16 12, Element.spacing 10, Element.width Element.fill ]


searchAttrs =
    [ Border.width 0, Element.padding 0 ]


listAttrs =
    [ Border.width 1
    , Border.roundEach { topLeft = 0, topRight = 0, bottomLeft = 5, bottomRight = 5 }
    , Element.width Element.fill
    , Element.spacing 5
    ]


itemToElement selected _ i =
    let
        bgColor =
            if selected then
                Element.rgb255 137 207 240

            else
                Element.rgb255 255 255 255
    in
    Element.row
        [ Background.color bgColor
        , Element.paddingXY 16 12
        , Element.width Element.fill
        ]
        [ Element.el [] (Element.text i)
        ]
