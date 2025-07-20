module Views.Tabs exposing (..)

import Element
import Element.Border as Border
import Element.Font as Font
import Gen.Route as Route
import Palette.X11 as X11
import Shared


viewTabs : Shared.Tab -> Element.Element Never
viewTabs tab =
    let
        viewTab route text highlightIf =
            Element.link
                [ Element.width Element.fill
                , Element.height Element.fill
                , Border.widthEach { top = 5, bottom = 0, left = 0, right = 0 }
                , Border.color
                    (Shared.solidColorToColor
                        (if tab == highlightIf then
                            X11.steelBlue

                         else
                            X11.lightGray
                        )
                    )
                ]
                { url = Route.toHref route, label = Element.paragraph [ Element.centerX ] [ Element.text text ] }
    in
    Element.row
        [ Element.alignBottom
        , Element.centerX
        , Element.width Element.fill
        , Element.height (Element.px 60)
        , Font.size 20
        , Font.center
        ]
        [ viewTab Route.Home_ "Buckets" Shared.Buckets
        , viewTab Route.History "History" Shared.History
        ]
