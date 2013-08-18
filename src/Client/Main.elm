module Main where

import JavaScript
import Mouse
import WebSocket
import Json
import JavaScript.Experimental
import Dict
import Char
import List
import Window
import Maybe
import Automaton

loopOut = JavaScript.fromString <~ WebSocket.connect "ws://localhost:3000" orderSig

foreign import jsevent "input"
    (JavaScript.fromString "YARK")
    input : Signal JavaScript.JSString

foreign export jsevent "loopOut"
    loopOut : Signal JavaScript.JSString

safeHead : [a] -> Maybe a
safeHead list = case list of { (a::_) -> Just a; _ -> Nothing }

mapMaybe : (a -> b) -> Maybe a -> Maybe b
mapMaybe f a = case a of { Just val -> Just <| f val; Nothing -> Nothing }

zoom = 8

mouseInGame : Signal (Int, Int)
mouseInGame = let mapMouse : (Int, Int) -> (Int, Int) -> (Int, Int)
                  mapMouse (w,h) (x,y) = (round ((toFloat x-(toFloat w/2))/zoom),round (((toFloat h/2)-toFloat y)/zoom))
              in mapMouse <~ Window.dimensions ~ Mouse.position

renderShapeAt shape (x,y) c = case shape of
    (Circle radius) -> circle (zoom * radius) |> outlined (solid c) |> move (zoom * x, zoom * y)
    (Rectangle w h) -> rect (zoom * w) (zoom * h) |> filled c |> move (zoom * x,zoom * y)

renderMenu phys = case phys.location of
    (OnMap x y) -> [rect 50 20 |> filled (rgba 100 0 0 0.5) |> move (zoom * x, zoom * y)]

renderPhys clientState (objId, phys) = case phys.location of
    (OnMap x y) -> Just <| renderShapeAt phys.shape (x,y) (if clientState.selected == Just objId then red else blue)
    (InObj _ _ _) -> Nothing


pointIsIn : Shape -> Location -> (Float, Float) -> Bool
pointIsIn shape location (x1,y1) = case (shape, location) of
    (Circle r, (OnMap x2 y2)) -> (x2-x1)^2 + (y2-y1)^2 <= r^2
    (Rectangle w h, (OnMap x2 y2)) -> 
        (x2-w/2 < x1 && x1 < x2+w/2) && (y2-h/2 < y1 && y1 < y2+h/2)
    _ -> False -- InObj is never moused over for now

pointIsInPhys (x,y) phys = pointIsIn phys.shape phys.location (toFloat x, toFloat y)

display (w,h) world clientState = 
    layers [ collage w h . Maybe.justs . map (renderPhys clientState) . Dict.toList <| world.physics
           , asText clientState
           ]

hovered = (\point -> mapMaybe fst . safeHead . filter (pointIsInPhys point . snd) . Dict.toList . .physics)
             <~ mouseInGame ~ world

selected = Automaton.run 
    (Automaton.state Nothing (\hover current -> maybe current Just hover)) Nothing  
    (sampleOn Mouse.clicks hovered)

-- type ClientState = {hovered: Maybe Int, player: String, selected: Maybe Int}

f hovered selected = {hovered = hovered, selected = selected, player = "James"}

-- clientState : Signal ClientState
clientState = f <~ hovered ~ selected


goTo : (Int, Int) -> Json.JsonValue
goTo (x,y) = Json.Object (Dict.fromList 
    [("GoTo", Json.Object (Dict.fromList [("ToMap", Json.Array [Json.Number <| toFloat x, Json.Number <| toFloat y])]))])

orderSig : Signal String
orderSig = sampleOn Mouse.clicks (Json.toString "" <~ (order <~ mouseInGame ~ clientState))

-- order : (Int, Int) -> Json.JsonValue
order pt clientState = case clientState.selected of
    Just objId -> if Maybe.isNothing clientState.hovered then
        Json.Object (Dict.fromList 
            [ ("Player", Json.String "James")
            , ("ObjId", Json.Number objId)
            , ("Goal", goTo pt)
            ])
                                                         else Json.Null
    _ -> Json.Null

world = worldDict . JavaScript.toString <~ input

main : Signal Element
main = display <~ Window.dimensions ~ world ~ clientState
{-main = plainText . show . Json.fromString . JavaScript.toString <~ input-}

type ObjId = Int
data Destination = ToMap (Float,Float) | ToObj ObjId
data Goal = GoTo Destination | WorkOn (ObjId, Int)
data Shape = Circle Float | Rectangle Float Float
data Location = OnMap Float Float | InObj ObjId Float Float
type Skill = {level: Int, speed: Int, skillType: String}
{-type Meta = {name: String, tags: [String]}-}
{-type Physics = {location: Location, shape: Shape, speed: Float, blocking: Bool}-}
{-type Task = {difficulty: Int, enabled: Bool, name: String, skill: Skill, workCompleted: Int, workRequired: Int, workType: String}-}
{-type Work = {goal: Maybe Goal, skills: [Skill], work: Maybe a}-}
{-type World = { meta: Dict.Dict Int Meta-}
             {-, physics: Dict.Dict Int Physics-}
             {-, work: Dict.Dict Int -- Work-}
             {-, tasks: Dict.Dict Int Task-}
             {-}-}

-------------------------------
-- Fixed old code to parse JSON
-------------------------------

toElmStr (Json.String str) = str

lookup = Dict.lookup

meta json = case json of
    Json.Array (Json.Number objId :: Json.Object dict :: []) -> case (Dict.lookup "Name" dict, Dict.lookup "Tags" dict, Dict.lookup "Owner" dict) of
            (Just (Json.String n), Just (Json.Array ts), Just (Json.Null)) -> (objId, {name=n, tags = map toElmStr ts, owner = Nothing})
            (Just (Json.String n), Just (Json.Array ts), Just (Json.String owner)) -> (objId, {name=n, tags = map toElmStr ts, owner = Just owner})
            _ -> (-1, {name="", tags = [], owner = Just <| show json})
    _ -> (-1, {name="", tags = [], owner = Just <| show json})

metaComp jsonDict = case Dict.lookup "Meta" jsonDict of
    Just (Json.Array metas) -> Dict.fromList <| map meta metas
    _ -> Dict.empty

toShape jsonDict = case (Dict.lookup "Circle" jsonDict, Dict.lookup "Rectangle" jsonDict) of
    (Just (Json.Number radius), _) -> Circle radius
    (_, Just (Json.Array (Json.Number w :: Json.Number h :: []))) -> Rectangle w h

-- NOTE: need to test InObj branch
toLocation jsonDict = case (Dict.lookup "OnMap" jsonDict, Dict.lookup "InObj" jsonDict) of
    (Just (Json.Array (Json.Number x :: Json.Number y :: [])), _) -> OnMap x y -- {x=x,y=y}
    (_, Just (Json.Array (Json.Number objId :: (Json.Array (Json.Number x :: Json.Number y :: [])) :: [] ))) -> InObj (round objId) x y -- {x=x,y=y}

-- Having a pattern matching issue.  This hack is grotesque.
boolHack boolStr = if boolStr == "Bool True" then True else False

phys json = case json of
    Json.Array (Json.Number objId :: Json.Object dict :: []) -> 
        case (Dict.lookup "Blocking" dict, Dict.lookup "Location" dict, Dict.lookup "Shape" dict, Dict.lookup "Speed" dict) of
            (Just foo, Just (Json.Object loc), Just (Json.Object shape), Just (Json.Number speed)) ->
                (objId, {location=toLocation loc, shape=toShape shape,speed=speed,blocking= (boolHack <| show foo)}) -- , shape=shape, speed=speed})
    _ -> (0, {location = OnMap 0 0, shape = Circle 1, speed = 1, blocking =False})

physComp jsonDict = 
    case Dict.lookup "Physics" jsonDict of
        Just (Json.Array physes) -> Dict.fromList <| map phys physes
        _ -> Dict.empty

--------------------------
-- DOES NOT WORK CORRECTLY
-- on GoTo branch
--------------------------
toGoal json =
    case json of
        Json.Null -> Nothing
        (Json.Object dict) -> case (lookup "GoTo" dict, lookup "WorkOn" dict) of
            (Just (Json.Object dict2), _) -> case (lookup "ToMap" dict2, lookup "ToObj" dict2) of
                (Just (Json.Array (Json.Number x :: Json.Number y :: [])), _) -> Just . GoTo <| ToMap (x, y)
                (_, (Just (Json.Number objId))) -> Just . GoTo . ToObj <| round objId
                _ -> Nothing
            (_, Just (Json.Object dict)) -> Nothing -- SHOULD FINISH WORKON
            _ -> Nothing
        _ -> Nothing

toSkill (Json.Object dict) = case (lookup "Level" dict, lookup "SkillType" dict, lookup "Speed" dict) of
    (Just (Json.Number level), Just (Json.Object skillType), Just (Json.Number speed)) ->
        {level = level, speed = speed, skillType = List.head (Dict.keys skillType)}
 
toWork json = case json of
    Json.Null -> Nothing
    (Json.Object dict) -> Just dict

work json = 
    case json of 
        (Json.Array (Json.Number objId :: Json.Object dict :: [])) -> 
            case (Dict.lookup "Goal" dict, Dict.lookup "GoalPref" dict, Dict.lookup "Skills" dict, Dict.lookup "Work" dict) of
                (Just goal, Just (Json.Object goalPref), Just (Json.Array skills), Just work) -> 
                    (1, {goal = toGoal goal, skills = map toSkill skills, work = toWork work})
                _ -> (0, {goal = Nothing, skills = [], work = Nothing})
        _ -> (0, {goal = Nothing, skills = [], work = Nothing})

workComp jsonDict = case Dict.lookup "Work" jsonDict of
    Just (Json.Array works) -> Dict.fromList <| map work works
    _ -> Dict.empty

extract obj = List.head <| Dict.keys obj

toTask dict = case (lookup "Difficulty" dict, lookup "Enabled" dict, lookup "Name" dict, lookup "Skill" dict, lookup "WorkCompleted" dict, lookup "WorkRequired" dict, lookup "WorkType" dict) of
    (Just (Json.Number difficulty), Just enabled, Just (Json.String name), Just (Json.Object skill), Just (Json.Number workCompleted), Just (Json.Number workRequired), Just (Json.Object workType)) ->
        {difficulty = difficulty, enabled = boolHack <| show enabled, name = name, skill = extract skill, workCompleted = workCompleted, workRequired = workRequired, workType = extract workType}

task (Json.Array (Json.Number taskId :: Json.Object json :: [])) = (taskId, toTask json)

objTask (Json.Array (Json.Number objId :: Json.Array rest :: [])) = (objId, map task rest)

-- taskComp : Dict.Dict String String -> Dict.Dict Int 
taskComp jsonDict = case Dict.lookup "Tasks" jsonDict of
    Just (Json.Array objTasks) -> Dict.fromList <| map objTask objTasks
    _ -> Dict.empty

worldDict string = case Json.fromString string of
    Just (Json.Object dict) -> { tasks = taskComp dict
                               , work = workComp dict
                               , physics= physComp dict
                               , meta = metaComp dict
                               }
    _ -> {tasks = Dict.empty, work = Dict.empty, physics= Dict.empty, meta = Dict.empty}

