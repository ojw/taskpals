module Main where

import JavaScript
{-import Mouse-}
import WebSocket
{-import Json-}
{-import Window-}
{-import Automaton-}

import Graphics.Input as Input

import open JsonHelper
import open Display
import open TaskPals
import open UI

loopOut = JavaScript.fromString <~ WebSocket.connect "ws://localhost:3000" (constant "command")

foreign export jsevent "loopOut"
    loopOut : Signal JavaScript.JSString

foreign import jsevent "input"
    (JavaScript.fromString "")
    input : Signal JavaScript.JSString

world = toWorld <~ input

main : Signal Element
main = registerBox
{-main = render <~ world ~ viewState ~ selectState-}
