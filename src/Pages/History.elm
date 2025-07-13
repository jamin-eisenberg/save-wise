module Pages.History exposing (page)

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


view : View msg
view =
    { title = "History"
    , element = viewTabs Shared.History
    }
