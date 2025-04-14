module Model.YearMonth exposing (YearMonth, displayMMYY, epoch, fromMonthAndYear, fromPosix)

import Time


type YearMonth
    = YearMonth { month : Int, year : Int }


fromMonthAndYear month year =
    if 1 <= month && month <= 12 && year >= 0 then
        Just (YearMonth { month = month, year = year })

    else
        Nothing


displayMMYY (YearMonth { month, year }) =
    (String.fromInt month |> String.padLeft 2 '0')
        ++ "/"
        ++ (String.fromInt year
                |> String.padLeft 2 '0'
           )


epoch =
    YearMonth { month = 1, year = 1970 }


fromPosix posix =
    let
        zone =
            Time.utc
    in
    YearMonth { month = Time.toMonth zone posix |> timeMonthNumber, year = Time.toYear zone posix }


timeMonthNumber month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12
