module Main where

import JavaScript
import Mouse
import WebSocket
import Json
import JavaScript.Experimental
import Dict

-- main = plainText . JavaScript.toString <~ input
main = plainText . display . toWorld <~ input

loopOut = JavaScript.fromString <~ WebSocket.connect "ws://localhost:3000" (show <~ sampleOn Mouse.clicks Mouse.position)

foreign import jsevent "input"
    (JavaScript.fromString "YARK")
    input : Signal JavaScript.JSString

foreign export jsevent "loopOut"
    loopOut : Signal JavaScript.JSString

display mworld = case mworld of
    Nothing -> "No world :/"
    Just world -> {-show <| Dict.fromList-} show world -- .Physics

toWorld mjson = case Json.fromJSString mjson of
    Nothing -> Nothing
    Just json -> Just <| JavaScript.Experimental.toRecord . Json.toJSObject <| json
