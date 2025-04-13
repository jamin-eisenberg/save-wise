module Views.Money exposing (..)

import Numeral


type alias MoneyFormat =
    { dollarSign : Bool
    , cents : Bool
    , alwaysSign : Bool
    }


format : MoneyFormat -> Int -> String
format { dollarSign, cents, alwaysSign } amountCents =
    let
        numeralFormat =
            (if dollarSign then
                "$"

             else
                ""
            )
                ++ "0,0"
                ++ (if cents then
                        ".00"

                    else
                        ""
                   )

        sign =
            case ( alwaysSign, amountCents < 0 ) of
                ( False, _ ) ->
                    ""

                ( True, True ) ->
                    ""

                ( True, False ) ->
                    "+"
    in
    sign ++ Numeral.format numeralFormat (toFloat amountCents / 100)
