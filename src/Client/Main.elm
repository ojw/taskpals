module Client.Main where

import Window
import WebSocket
import Json
import Maybe
import Dict
import List
import Mouse
import Automaton

safeHead : [a] -> Maybe a
safeHead list = case list of { (a::_) -> Just a; _ -> Nothing }

mapMaybe : (a -> b) -> Maybe a -> Maybe b
mapMaybe f a = case a of { Just val -> Just <| f val; Nothing -> Nothing }

-- import Client.Render

zoom = 8

mapMouse : (Int, Int) -> (Float, Float) -> (Float, Float)
mapMouse (w,h) (x,y) = ((x-(toFloat w/2))/zoom,((toFloat h/2)-y)/zoom)

mouseInGame : Signal (Float, Float)
mouseInGame = mapMouse <~ Window.dimensions ~ Mouse.position

renderShapeAt shape (x,y) c = case shape of
    (Circle radius) -> circle (zoom * radius) |> outlined (solid c) |> move (zoom * x, zoom * y)
    (Rectangle w h) -> rect (zoom * w) (zoom * h) |> filled c |> move (zoom * x,zoom * y)

renderMenu phys = case phys.location of
    (OnMap x y) -> [rectangle 50 20 |> filled (rgba 100 0 0 0.5) |> move (zoom * x, zoom * y)]

renderPhys clientState (objId, phys) = case phys.location of
    (OnMap x y) -> Just <| renderShapeAt phys.shape (x,y) (if clientState.selected == Just objId then red else blue)
    (InObj _ _ _) -> Nothing

pointIsIn : Shape -> Location -> (Float, Float) -> Bool
pointIsIn shape location (x1,y1) = case (shape, location) of
    (Circle r, (OnMap x2 y2)) -> (x2-x1)^2 + (y2-y1)^2 <= r^2
    (Rectangle w h, (OnMap x2 y2)) -> (x2-w/2 < x1 && x1 < x2+w/2) && (y2-h/2 < y1 && y1 < y2+h/2)
    _ -> False -- InObj is never moused over for now

pointIsInPhys (x,y) phys = pointIsIn phys.shape phys.location (x,y)

-- display : (Int, Int) -> World -> CientState -> Element
display (w,h) world clientState = 
    layers [ collage w h . Maybe.justs . map (renderPhys clientState) . Dict.toList <| world.physics
           , asText clientState
           ]

goTo : (Float, Float) -> JsonValue
goTo (x,y) = Json.Object (Dict.fromList 
    [("GoTo", Json.Object (Dict.fromList [("ToMap", Json.Array [Json.Number x, Json.Number y])]))])

order : Signal JsonValue
order = sampleOn Mouse.clicks (
    (\(x,y) -> 
        Json.Object (Dict.fromList 
            [ ("Player", Json.String "James")
            , ("ObjId", Json.Number 1)
            , ("Goal", goTo (x,y))
            ])) <~ mouseInGame)
        
output = sampleOn Mouse.clicks (Json.toString " " <~ order)

world : Signal World
world = worldDict <~ WebSocket.connect "ws://0.0.0.0:3000" output

main : Signal Element
main = display <~ Window.dimensions ~ world ~ clientState

hovered = (\point -> mapMaybe fst . safeHead . filter (pointIsInPhys point . snd) . Dict.toList . .physics)
             <~ mouseInGame ~ world
                                 
selected = Automaton.run 
    (Automaton.state Nothing (\hover current -> maybe current Just hover)) Nothing  
    (sampleOn Mouse.clicks hovered)

foo = Automaton.run (Automaton.state Nothing (\bar _ -> bar)) Nothing

type ClientState = {hovered: Maybe Number, player: String, selected: Maybe Number}

clientState : Signal ClientState
clientState = let f hovered selected = {hovered = hovered, selected = selected, player = "James"} in f <~ hovered ~ selected

type ObjId = Number
data Destination = ToMap (Float,Float) | ToObj ObjId
data Goal = GoTo Destination | WorkOn (ObjId, Number)
data Shape = Circle Float | Rectangle Float Float
data Location = OnMap Float Float | InObj ObjId Float Float
type Skill = {level: Int, speed: Int, skillType: String}
type Meta = {name: String, tags: [String]}
type Physics = {location: Location, shape: Shape, speed: Float, blocking: Bool}
type Task = {difficulty: Int, enabled: Bool, name: String, skill: Skill, workCompleted: Int, workRequired: Int, workType: String}
type Work = {goal: Maybe Goal, skills: [Skill], work: Maybe Work}
type World = { meta: Dict Int Meta
             , physics: Dict Int Physics
             , work: Dict Int -- Work
             , tasks: Dict Int Task
             }

toElmStr (String str) = str

lookup = Dict.lookup

meta json = case json of
    Array (Number objId :: Object dict :: []) -> case (Dict.lookup "Name" dict, Dict.lookup "Tags" dict) of
            (Just (String n), Just (Array ts)) -> (objId, {name=n, tags = map toElmStr ts})
            _ -> error
    _ -> error -- "BAR"

metaComp jsonDict = case Dict.lookup "Meta" jsonDict of
    Just (Array metas) -> Dict.fromList <| map meta metas
    _ -> Dict.empty

toShape jsonDict = case (Dict.lookup "Circle" jsonDict, Dict.lookup "Rectangle" jsonDict) of
    (Just (Number radius), _) -> Circle radius
    (_, Just (Array (Number w :: Number h :: []))) -> Rectangle w h

-- NOTE: need to test InObj branch
toLocation jsonDict = case (Dict.lookup "OnMap" jsonDict, Dict.lookup "InObj" jsonDict) of
    (Just (Array (Number x :: Number y :: [])), _) -> OnMap x y -- {x=x,y=y}
    (_, Just (Array (Numer objId :: (Array (Number x :: Number y :: [])) :: [] ))) -> InObj objId x y -- {x=x,y=y}

phys json = case json of
    Array (Number objId :: Object dict :: []) -> 
        case (Dict.lookup "Blocking" dict, Dict.lookup "Location" dict, Dict.lookup "Shape" dict, Dict.lookup "Speed" dict) of
            (Just (Bool blocking), Just (Object loc), Just (Object shape), Just (Number speed)) ->
                (objId, {location=toLocation loc, shape=toShape shape,speed=speed,blocking=blocking}) -- , shape=shape, speed=speed})

physComp jsonDict = case Dict.lookup "Physics" jsonDict of
    Just (Array physes) -> Dict.fromList <| map phys physes
    _ -> Dict.empty

toGoal json = case json of
    Null -> Nothing
    (Object dict) -> case (lookup "GoTo" dict, lookup "WorkOn" dict) of
        (Just (Object dict), _) -> case (lookup "ToMap" dict, lookup "ToObj" dict) of
            (Just (Array (Number x :: Number y :: [])), _) -> Just <| GoTo <| ToMap (x,y)
            (_, (Just (Number objId))) -> Just <| GoTo <| ToObj objId
        (_, Just (Object dict)) -> Nothing -- dict
        _ -> Nothing
    _ -> Nothing

-- Not a serious implementation :/
toGoalPref json = Nothing

toSkill (Object dict) = case (lookup "Level" dict, lookup "SkillType" dict, lookup "Speed" dict) of
    (Just (Number level), Just (Object skillType), Just (Number speed)) ->
        {level = level, speed = speed, skillType = List.head (Dict.keys skillType)}
 
toWork json = case json of
    Null -> Nothing
    (Object dict) -> Just dict

work (Array (Number objId :: Object dict :: [])) = case (Dict.lookup "Goal" dict, Dict.lookup "GoalPref" dict, Dict.lookup "Skills" dict, Dict.lookup "Work" dict) of
    (Just goal, Just (Object goalPref), Just (Array skills), Just work) -> 
        (1, {goal = toGoal goal, {-goalPref = toGoalPref goalPref,-} skills = map toSkill skills, work = toWork work})

-- (objId, dict)

workComp jsonDict = case Dict.lookup "Work" jsonDict of
    Just (Array works) -> Dict.fromList <| map work works
    _ -> Dict.empty

extract obj = List.head <| Dict.keys obj

toTask dict = case (lookup "Difficulty" dict, lookup "Enabled" dict, lookup "Name" dict, lookup "Skill" dict, lookup "WorkCompleted" dict, lookup "WorkRequired" dict, lookup "WorkType" dict) of
    (Just (Number difficulty), Just (Bool enabled), Just (String name), Just (Object skill), Just (Number workCompleted), Just (Number workRequired), Just (Object workType)) ->
        {difficulty = difficulty, enabled = enabled, name = name, skill = extract skill, workCompleted = workCompleted, workRequired = workRequired, workType = extract workType}

task (Array (Number taskId :: Object json :: [])) = (taskId, toTask json)

objTask (Array (Number objId :: Array rest :: [])) = (objId, map task rest)

taskComp : Dict String -> Dict Int
taskComp jsonDict = case Dict.lookup "Tasks" jsonDict of
    Just (Array objTasks) -> Dict.fromList <| map objTask objTasks
    _ -> Dict.empty

worldDict string = case Json.fromString string of
    Just (Object dict) -> {tasks = taskComp dict, work=workComp dict, physics=physComp dict, meta=metaComp dict}
    _ -> {tasks = Dict.empty, work = Dict.empty, physics= Dict.empty, meta = Dict.empty}

