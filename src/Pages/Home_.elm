module Pages.Home_ exposing (page)

import Dict exposing (Dict)
import Element
import Element.Font as Font
import Html.Attributes exposing (style)
import Page exposing (Page)
import Request exposing (Request)
import Shared
import View exposing (View)
import Views.BucketBreakdown
import Views.Tabs exposing (viewTabs)


page : Shared.Model -> Request -> Page
page shared _ =
    Page.static
        { view = view shared.buckets shared.transfers
        }


view : Dict String Shared.Bucket -> List Shared.Transfer -> View Never
view buckets transfers =
    { title = "Home"
    , floatingElements = [ viewTabs Shared.Buckets ]
    , element =
        Element.column
            [ Element.centerX, Element.width Element.fill, Element.paddingXY 0 20 ]
            [ Element.paragraph [ Font.center, Font.size 80 ] [ Element.text "$111,300" ]
            , Views.BucketBreakdown.view (Dict.values buckets) transfers Nothing
            ]
    }
