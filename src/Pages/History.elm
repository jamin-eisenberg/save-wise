module Pages.History exposing (page)

import Browser.Dom exposing (Element)
import Element
import Gen.Params.History exposing (Params)
import Page exposing (Page)
import Request
import Shared
import View exposing (View)
import Views.Tabs exposing (viewTabs)


page : Shared.Model -> Request.With Params -> Page
page shared req =
    Page.static
        { view = view
        }


view : View Never
view =
    { title = "History"
    , floatingElements = [ viewTabs Shared.History ]
    , element = Element.none
    }
