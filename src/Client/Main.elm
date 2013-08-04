module Main where

import Window
import WebSocket
import Json
import Maybe
import Dict
import List
import JavaScript.Experimental

-- import Serialize

renderShapeAt shape (x,y) c = case shape of
    (Circle radius) -> circle radius |> filled c |> move (x,y)
    (Rectangle w h) -> rect w h |> filled c |> move (x,y)

renderPhys phys = case phys.location of
    (OnMap x y) -> Just <| renderShapeAt phys.shape (x,y) (if phys.blocking then red else blue)
    (InObj _ _ _) -> Nothing

display : (Int, Int) -> a -> Element
display (w,h) world = 
    layers [ collage w h (Maybe.justs (map renderPhys (Dict.values world.physics)))
           , asText world
           ]

main : Signal Element
main = display <~ Window.dimensions ~ worldSig

input = WebSocket.connect "ws://0.0.0.0:3000" (constant "") 

-- worldSig : {physics: a, meta: b, tasks: c, work: d}
worldSig : Signal World
worldSig = worldDict <~ (Json.fromString <~ input)

type ObjId = Number
data Destination = ToMap (Float,Float) | ToObj ObjId
data Goal = GoTo Destination | WorkOn (ObjId, Number)
data Shape = Circle Float | Rectangle Float Float
data Location = OnMap Float Float | InObj ObjId Float Float
type World = { meta: Dict Int -- Dict Int -- {name: String, tags: [String]}
             , physics: Dict Int -- Dict Int --{shape: Shape, location: Location, speed: Int, blocking: Bool}
             , work: Dict Int -- Dict Int --{}
             , tasks: Dict Int
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
        (_, Just (Object dict)) -> dict
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
        (1, {goal = toGoal goal, goalPref = toGoalPref goalPref, skills = map toSkill skills, work = toWork work})

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

worldDict json = case json of
    Just (Object dict) -> {tasks = taskComp dict, work=workComp dict, physics=physComp dict, meta=metaComp dict}
    _ -> {tasks = Dict.empty, work = Dict.empty, physics= Dict.empty, meta = Dict.empty}

