module ElmAbuse exposing (..)

-- where

import Native.ElmAbuse exposing (violentlyForceStyle)


forceStyle : List String -> Platform.Task String String
forceStyle hrefs =
    Native.ElmAbuse.violentlyForceStyle "__elm_abuse_" hrefs
