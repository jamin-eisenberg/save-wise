module Pages.Bucket.BucketId_.Expense.ExpenseId_ exposing (Model, Msg, page)

import Dict
import Dropdown
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Gen.Params.Bucket.BucketId_.Expense.ExpenseId_ exposing (Params)
import Html.Attributes exposing (type_)
import Model.YearMonth exposing (monthToString)
import Page exposing (Page)
import Platform.Cmd as Cmd
import Request
import Shared
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
    }


type Msg
    = EditDescription String
    | EditCost String
    | YearDropdownMsg (Dropdown.Msg Year)
    | EditYear (Maybe Int)
    | MonthDropdownMsg (Dropdown.Msg Time.Month)
    | EditMonth (Maybe Time.Month)



-- | CreateExpense


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
    Page.element
        { init = init expenseId expense
        , update = update shared.currentYear
        , view = view shared.currentYear
        , subscriptions = \_ -> Sub.none
        }


init : String -> Maybe Shared.Expense -> ( Model, Cmd Msg )
init expenseId e =
    ( case e of
        Nothing ->
            { id = expenseId
            , description = ""
            , cost = Nothing
            , year = Nothing
            , yearDropdownState = Dropdown.init "year-dropdown"
            , month = Nothing
            , monthDropdownState = Dropdown.init "month-dropdown"
            }

        Just expense ->
            { id = expense.id
            , description = expense.description
            , cost = Just expense.cost
            , year = Just (Model.YearMonth.getYear expense.yearMonth)
            , yearDropdownState = Dropdown.init "year-dropdown"
            , month = Just (Model.YearMonth.getMonth expense.yearMonth)
            , monthDropdownState = Dropdown.init "month-dropdown"
            }
    , Cmd.none
    )


update : Int -> Msg -> Model -> ( Model, Cmd Msg )
update currentYear msg model =
    case msg of
        EditDescription newDescription ->
            ( { model | description = newDescription }, Cmd.none )

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
                        |> String.filter (\c -> not (List.member c [ '$', '.', ',' ]))
                        |> String.toInt
              }
            , Cmd.none
            )

        MonthDropdownMsg subMsg ->
            let
                ( state, cmd ) =
                    Dropdown.update monthDropdownConfig subMsg model model.monthDropdownState
            in
            ( { model | monthDropdownState = state }, cmd )

        EditMonth newMonth ->
            ( { model | month = newMonth }, Cmd.none )

        YearDropdownMsg subMsg ->
            let
                ( state, cmd ) =
                    Dropdown.update (yearDropdownConfig currentYear) subMsg model model.yearDropdownState
            in
            ( { model | yearDropdownState = state }, cmd )

        EditYear newYear ->
            ( { model | year = newYear }, Cmd.none )


view : Int -> Model -> View Msg
view currentYear model =
    { title = model.description
    , element =
        Element.column [ Element.width Element.fill, Element.padding 20, Element.spacing 20 ]
            [ Input.text []
                { onChange = EditDescription
                , text = model.description
                , placeholder = Nothing
                , label = Input.labelAbove [] (Element.text "Description")
                }
            , Input.text []
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
            ]
    }


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
