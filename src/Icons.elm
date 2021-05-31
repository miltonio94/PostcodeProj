module Icons exposing (pinIcon, randomIcon)

import FeatherIcons exposing (mapPin, shuffle, toHtml, withClass, withSize)


pinIcon =
    mapPin
        |> withClass "pinIcon"
        |> toHtml []


randomIcon =
    shuffle
        |> withClass "randomIcon"
        |> toHtml []
