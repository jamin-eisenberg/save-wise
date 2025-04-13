module Pages.Bucket.BucketId_.Expense.ExpenseId_ exposing (page)

import Gen.Params.Bucket.BucketId_.Expense.ExpenseId_ exposing (Params)
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
    View.placeholder "Bucket.BucketId_.Expense.ExpenseId_"
