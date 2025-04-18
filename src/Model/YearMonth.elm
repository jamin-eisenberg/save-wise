module Model.YearMonth exposing (YearMonth, displayMMYY, epoch, fromMonthAndYear, fromPosix, getMonth, getYear, monthToString)

import Time


type YearMonth
    = YearMonth { month : Time.Month, year : Int }


getYear (YearMonth { year }) =
    year


getMonth (YearMonth { month }) =
    month


fromMonthAndYear month year =
    if year >= 0 then
        Just (YearMonth { month = month, year = year })

    else
        Nothing


displayMMYY (YearMonth { month, year }) =
    (timeMonthNumber month |> String.fromInt |> String.padLeft 2 '0')
        ++ "/"
        ++ (String.fromInt year
                |> String.padLeft 2 '0'
           )


epoch =
    YearMonth { month = Time.Jan, year = 1970 }


fromPosix posix =
    let
        zone =
            Time.utc
    in
    YearMonth { month = Time.toMonth zone posix, year = Time.toYear zone posix }


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


monthToString : Time.Month -> String
monthToString month =
    case month of
        Time.Jan ->
            "January"

        Time.Feb ->
            "February"

        Time.Mar ->
            "March"

        Time.Apr ->
            "April"

        Time.May ->
            "May"

        Time.Jun ->
            "June"

        Time.Jul ->
            "July"

        Time.Aug ->
            "August"

        Time.Sep ->
            "September"

        Time.Oct ->
            "October"

        Time.Nov ->
            "November"

        Time.Dec ->
            "December"
