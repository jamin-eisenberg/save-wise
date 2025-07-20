module View exposing (View, map, none, placeholder, toBrowserDocument)

import Browser
import Element exposing (Element)


type alias View msg =
    { title : String
    , floatingElements : List (Element msg)
    , element : Element msg
    }


placeholder : String -> View msg
placeholder str =
    { title = str
    , floatingElements = []
    , element = Element.text str
    }


none : View msg
none =
    placeholder ""


map : (a -> b) -> View a -> View b
map fn view =
    { title = view.title
    , floatingElements = List.map (Element.map fn) view.floatingElements
    , element = Element.map fn view.element
    }


toBrowserDocument : View msg -> Browser.Document msg
toBrowserDocument view =
    { title = "SaveWise - " ++ view.title
    , body =
        [ Element.layout (List.map Element.inFront view.floatingElements) view.element
        ]
    }
