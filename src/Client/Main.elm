module Main where

import JavaScript
import Mouse
import WebSocket
import Json
import JavaScript.Experimental
import Dict
import Char
import List

main = plainText . show . worldDict . JavaScript.toString <~ input

loopOut = JavaScript.fromString <~ WebSocket.connect "ws://localhost:3000" (show <~ sampleOn Mouse.clicks Mouse.position)

foreign import jsevent "input"
    (JavaScript.fromString "YARK")
    input : Signal JavaScript.JSString

foreign export jsevent "loopOut"
    loopOut : Signal JavaScript.JSString

{-display mworld = case mworld of-}
    {-Nothing -> "No world :/"-}
    {-Just world -> show <| fixWorld world-}

renderPhys phys = case (phys.location, phys.shape) of
    ({onMap}, {circle}) -> "COICLE"
    ({onMap}, {rectangle}) -> "RECTONGLE"
    _ -> "DERP"

unsafeListToPair : [a] -> (a,a)
unsafeListToPair list = (head list, head . drop 1 <| list)

{-toWorld mjson = case Json.fromJSString mjson of-}
    {-Nothing -> {meta = Dict.empty, physics = Dict.empty, nextObj = 0, work = Dict.empty, tasks = Dict.empty}-}
    {-Just json -> fixWorld . JavaScript.Experimental.toRecord . Json.toJSObject . fixCases <| json-}

{-fixWorld world = let fixHeteroList list = case list of-}
                        {-([first, second]::rest) -> (first, second) :: fixHeteroList rest-}
                        {-_ -> []-}
                 {-in { world | physics <- Dict.fromList . fixHeteroList <| world.physics-}
                            {-, work <- Dict.fromList . fixHeteroList <| world.work -}
                            {-, tasks <- Dict.map (Dict.fromList . fixHeteroList) <| Dict.fromList . fixHeteroList <| world.tasks-}
                            {-, meta <- Dict.fromList . fixHeteroList <| world.meta }-}


{-fixCases : Json.JsonValue -> Json.JsonValue-}
{-fixCases value = let lowerHead : String -> String-}
                     {-lowerHead string = case string of-}
                        {-(x::xs) -> Char.toLower x :: xs-}
                        {-[] -> []-}
                 {-in case value of-}
                    {-Json.String string -> Json.String <| lowerHead string-}
                    {-Json.Array a -> Json.Array <| map fixCases a-}
                    {-Json.Object d -> Json.Object <| Dict.fromList . map (\(k,v) -> (lowerHead k, fixCases v))  . Dict.toList <| d -}
                    {-jsonValue -> jsonValue -}


type ObjId = Int
data Destination = ToMap (Float,Float) | ToObj ObjId
data Goal = GoTo Destination | WorkOn (ObjId, Int)
data Shape = Circle Float | Rectangle Float Float
data Location = OnMap Float Float | InObj ObjId Float Float
type Skill = {level: Int, speed: Int, skillType: String}
type Meta = {name: String, tags: [String]}
type Physics = {location: Location, shape: Shape, speed: Float, blocking: Bool}
type Task = {difficulty: Int, enabled: Bool, name: String, skill: Skill, workCompleted: Int, workRequired: Int, workType: String}
type Work = {goal: Maybe Goal, skills: [Skill], work: Maybe a}
type World = { meta: Dict.Dict Int Meta
             , physics: Dict.Dict Int Physics
             , work: Dict.Dict Int -- Work
             , tasks: Dict.Dict Int Task
             }

-------------------------------
-- Fixed old code to parse JSON
-------------------------------

toElmStr (Json.String str) = str

lookup = Dict.lookup

meta json = case json of
    Json.Array (Json.Number objId :: Json.Object dict :: []) -> case (Dict.lookup "Name" dict, Dict.lookup "Tags" dict) of
            (Just (Json.String n), Just (Json.Array ts)) -> (objId, {name=n, tags = map toElmStr ts})
            _ -> (-1, {name="", tags = []})
    _ -> (-1, {name="", tags = []})

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

toGoal json = case json of
    Json.Null -> Nothing
    (Json.Object dict) -> case (lookup "GoTo" dict, lookup "WorkOn" dict) of
        (Just (Json.Object dict), _) -> case (lookup "ToMap" dict, lookup "ToObj" dict) of
            (Just (Json.Array (Json.Number x :: Json.Number y :: [])), _) -> Just <| GoTo <| ToMap (x,y)
            (_, (Just (Json.Number objId))) -> Just . GoTo . ToObj <| round objId
        (_, Just (Json.Object dict)) -> Nothing -- dict
        _ -> Nothing
    _ -> Nothing

-- Not a serious implementation :/
toGoalPref json = Nothing

toSkill (Json.Object dict) = case (lookup "Level" dict, lookup "SkillType" dict, lookup "Speed" dict) of
    (Just (Json.Number level), Just (Json.Object skillType), Just (Json.Number speed)) ->
        {level = level, speed = speed, skillType = List.head (Dict.keys skillType)}
 
toWork json = case json of
    Json.Null -> Nothing
    (Json.Object dict) -> Just dict

work (Json.Array (Json.Number objId :: Json.Object dict :: [])) = case (Dict.lookup "Goal" dict, Dict.lookup "GoalPref" dict, Dict.lookup "Skills" dict, Dict.lookup "Work" dict) of
    (Just goal, Just (Json.Object goalPref), Just (Json.Array skills), Just work) -> 
        (1, {goal = toGoal goal, {-goalPref = toGoalPref goalPref,-} skills = map toSkill skills, work = toWork work})

-- (objId, dict)

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
    {-Just (Json.Object dict) -> {tasks = taskComp dict, work=workComp dict, physics=physComp dict, meta=metaComp dict}-}
    Just (Json.Object dict) -> -- {tasks = taskComp dict, work=workComp dict, physics=physComp dict, meta=metaComp dict}
        {tasks = taskComp dict, work = workComp dict, physics= physComp dict, meta = metaComp dict}
    _ -> {tasks = Dict.empty, work = Dict.empty, physics= Dict.empty, meta = Dict.empty}

