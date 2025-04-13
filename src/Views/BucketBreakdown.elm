module Views.BucketBreakdown exposing (view)

import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Gen.Route as Route
import Numeral
import Palette.Cubehelix
import Palette.X11 as X11
import Shared
import SolidColor
import Url
import Views.Money


view : List Shared.Bucket -> List Shared.Transfer -> Maybe String -> Element.Element msg
view buckets transfers selectedBucketId =
    let
        palette =
            (List.length buckets + 2)
                |> Palette.Cubehelix.generate
                |> List.drop 1
                |> List.take (List.length buckets)
    in
    Element.column [ Element.width Element.fill ]
        [ viewBar buckets transfers selectedBucketId palette
        , viewLegend buckets transfers selectedBucketId palette
        ]


viewBar : List Shared.Bucket -> List Shared.Transfer -> Maybe String -> List SolidColor.SolidColor -> Element.Element msg
viewBar buckets transfers selectedBucketId palette =
    Element.row
        [ Element.width Element.fill
        , Element.padding 5
        , Element.height (Element.px 75)
        ]
        (List.map2
            (\bucket color ->
                let
                    selected =
                        Just bucket.id == selectedBucketId

                    heightNarrowingFiller =
                        if selected then
                            0

                        else
                            1

                    linkRoute =
                        routeFromSelected selected bucket.id
                in
                Element.column [ Element.width (Element.fillPortion (Shared.totalNetCents bucket transfers)), Element.height Element.fill ]
                    [ Element.el [ Element.height (Element.fillPortion heightNarrowingFiller) ] Element.none
                    , Element.link
                        [ Element.width Element.fill
                        , Element.height (Element.fillPortion 5)
                        , Background.color (Shared.solidColorToColor color)
                        ]
                        { url = Route.toHref linkRoute, label = Element.none }
                    , Element.el [ Element.height (Element.fillPortion heightNarrowingFiller) ] Element.none
                    ]
            )
            buckets
            palette
        )


viewLegend : List Shared.Bucket -> List Shared.Transfer -> Maybe String -> List SolidColor.SolidColor -> Element.Element msg
viewLegend buckets transfers selectedBucketId palette =
    let
        total =
            buckets
                |> List.map (\bucket -> Shared.totalNetCents bucket transfers)
                |> List.map toFloat
                |> List.sum

        selected bucket =
            Just bucket.id == selectedBucketId

        wrapWithColoredBucketLink bucket label =
            Element.link
                ([ Element.width Element.fill, Element.height Element.fill, Element.padding 10 ]
                    ++ (if selected bucket then
                            [ Background.color (Shared.solidColorToColor X11.lightGreen) ]

                        else
                            []
                       )
                )
                { url = Route.toHref (routeFromSelected (selected bucket) bucket.id)
                , label = label
                }
    in
    Element.table [ Element.padding 20, Element.width Element.fill, Font.size 20, Font.alignLeft ]
        { data = List.map2 (\bucket color -> { bucket = bucket, color = color }) buckets palette
        , columns =
            [ { header = Element.none
              , width = Element.shrink
              , view =
                    \{ color, bucket } ->
                        wrapWithColoredBucketLink bucket
                            (Element.el
                                [ Element.width (Element.px 20)
                                , Element.height (Element.px 20)
                                , Border.color (Shared.solidColorToColor X11.black)
                                , Border.width 2
                                , Background.color (Shared.solidColorToColor color)
                                ]
                                Element.none
                            )
              }
            , { header = Element.none
              , width = Element.fill
              , view =
                    \{ bucket } ->
                        wrapWithColoredBucketLink bucket
                            (Element.text bucket.id)
              }
            , { header = Element.none
              , width = Element.shrink
              , view =
                    \{ bucket } ->
                        wrapWithColoredBucketLink bucket
                            (Element.text (Views.Money.format { dollarSign = True, cents = False, alwaysSign = False } (Shared.totalNetCents bucket transfers)))
              }
            , { header = Element.none
              , width = Element.shrink
              , view =
                    \{ bucket } ->
                        wrapWithColoredBucketLink bucket
                            (Element.text (Numeral.format "0.0%" (toFloat (Shared.totalNetCents bucket transfers) / total)))
              }
            ]
        }


routeFromSelected selected bucketId =
    if selected then
        Route.Home_

    else
        Route.Bucket__BucketId_ { bucketId = Url.percentEncode bucketId }
