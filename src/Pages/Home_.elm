module Pages.Home_ exposing (page)

import Dict exposing (Dict)
import Element
import Element.Font as Font
import Page exposing (Page)
import Request exposing (Request)
import Shared
import View exposing (View)
import Views.BucketBreakdown


page : Shared.Model -> Request -> Page
page shared _ =
    Page.static
        { view = view shared.buckets shared.transfers
        }


view : Dict String Shared.Bucket -> List Shared.Transfer -> View msg
view buckets transfers =
    { title = "Home"
    , element =
        Element.column
            [ Font.size 80, Font.center, Element.centerX, Element.width Element.fill, Element.paddingXY 0 20 ]
            [ Element.paragraph [] [ Element.text "$111,300" ]
            , Views.BucketBreakdown.view (Dict.values buckets) transfers Nothing
            ]
    }
