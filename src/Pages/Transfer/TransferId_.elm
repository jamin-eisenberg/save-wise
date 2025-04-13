module Pages.Transfer.TransferId_ exposing (page)

import Gen.Params.Transfer.TransferId_ exposing (Params)
import Page exposing (Page)
import Request
import Shared
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page
page shared req =
    Page.static
        { view = view
        }


view : View msg
view =
    View.placeholder "Transfer.TransferId_"
