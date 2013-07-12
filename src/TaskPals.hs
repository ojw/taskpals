{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances,
             DeriveFunctor, OverloadedStrings #-}

module TaskPals where

import Control.Lens
import Control.Applicative
import Data.IntMap (IntMap)
import qualified Data.IntMap as I
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe

type ObjId = Int
type TaskId = Int
type Time = Double
type X = Double
type Y = Double
type Point = (X,Y)
type Radius = Double
type Width = Double
type Height = Double
type Speed = Double

data SkillType = Labor | Combat | Medical | Mechanical | Chemical | Hacking | Observation
    deriving (Eq, Ord, Show)

data WorkType = Open | Close | Break | Unlock | Hack | Fix | Heal | Barricade | Use

data World = World
    { _worldObjs :: IntMap Obj
    , _worldTasks :: IntMap Task
    }

data Obj = Obj
    { _objObjId :: ObjId
    , _objTask :: TaskSystem
    , _objSpace :: SpatialSystem
    }

data SpatialSystem = SpatialSystem
    { _spatialsystemLocation :: Location
    , _spatialsystemShape :: Shape
    , _spatialsystemDestination :: Maybe Destination
    , _spatialsystemSpeed :: Speed
    }

data Location = OnMap (X,Y) | InObj ObjId (X,Y)
data Shape = Circle Radius | Rectangle Width Height
data Destination = ToMap (X,Y) | ToObj ObjId | ToTask TaskId

data TaskSystem = TaskSystem
    { _tasksystemTasks :: [TaskId]
    , _tasksystemSkills :: Map SkillType Skill
    , _tasksystemWork :: Maybe Work
    }

data Work = Work
    { _workTask :: TaskId
    , _workComplete :: Double
    }

data Skill = Skill
    { _skillSkillType :: SkillType
    , _skillLevel :: Int
    , _skillSpeed :: Int
    }

{-data Visibility a = Visible a | Invisible Int a deriving (Ord, Eq, Functor)-}

{-see :: Visibility a -> a-}
{-see (Visible a) = a-}
{-see (Invisible _ a) = a-}

data Task = Task
    { _taskName :: Text
    , _taskWorkType :: WorkType
    , _taskSkill :: SkillType
    , _taskDifficulty :: Int
    , _taskWorkRequired :: Int
    , _taskWorkCompleted :: Int
    , _taskObject :: ObjId
    , _taskOutcome :: [TaskEvent] -- World -> World -- ? or Task -> World -> World or something else
    , _taskVisibility :: Int -- Complete Examine task will reveal tasks w/ visibility <= Observation skill
    } 

data Target = Self | AtObj ObjId | InRadiusOf (X,Y) Radius | InRectangle (X,Y) (Width, Height)

data TaskEvent = None | RemoveThisTask | AddTask Task | SetBlocking Bool | CreateWork WorkType Int Target | RemoveThisObj

makeFields ''TaskSystem
makeFields ''SpatialSystem
makeFields ''Obj
makeFields ''Work
makeFields ''Skill
makeFields ''Task 
makeFields ''World

_x :: Lens Location Location X X
_x = lens getX setX where
    getX loc = case loc of
        OnMap (x,_) -> x
        InObj _ (x,_) -> x
    setX loc x' = case loc of
        OnMap (x,y) -> OnMap (x',y)
        InObj objId (x,y) -> InObj objId (x',y)

_y :: Lens Location Location Y Y
_y = lens getY setY where
    getY loc = case loc of
        OnMap (_,y) -> y
        InObj _ (_,y) -> y
    setY loc y' = case loc of
        OnMap (x,y) -> OnMap (x,y')
        InObj objId (x,y) -> InObj objId (x,y')

obj :: Obj
obj = Obj 0 (TaskSystem [] M.empty Nothing) (SpatialSystem (OnMap (0,0)) (Circle 10) Nothing 10)

open :: Task
open = Task "Open" Open Labor 1 1 0 0 [RemoveThisTask, AddTask close, SetBlocking False] 0

close :: Task
close = Task "Close" Close Labor 1 1 0 0 [RemoveThisTask, AddTask open, SetBlocking True] 0

break :: Task
break = Task "Break" Break Labor 2 10 0 0 [RemoveThisObj] 0

hardBreak :: Task
hardBreak = TaskPals.break & difficulty .~ 3 & workRequired .~ 20

modObj :: ObjId -> (Obj -> Obj) -> World -> World
modObj objId f world = objs %~ (I.adjust f objId) $ world

modTask :: TaskId -> (Task -> Task) -> World -> World
modTask taskId f world = tasks %~ (I.adjust f taskId) $ world

modTaskObj :: TaskId -> (Obj -> Obj) -> World -> World
modTaskObj taskId f world = case I.lookup taskId (world^.tasks) of
    Nothing -> world
    Just task -> modObj (task^.object) f world

removeTask :: TaskId -> World -> World
removeTask taskId world = tasks %~ I.delete taskId $ modTaskObj taskId (over (task.tasks) (filter (/= taskId))) world

{-runEvent :: TaskId -> ObjId -> Event -> World -> World-}
{-runEvent _ _ None world = world-}
{-runEvent taskId objId RemoveThisTask =-}

-- LocationSystem

moveBy :: (X,Y) -> Obj -> Obj
moveBy (x,y) obj = case obj^.space.location of
    OnMap (x1,y1) -> obj & space.location .~ OnMap (x+x1, y+y1)
    InObj objId (x1,y1) -> obj & space.location .~ InObj objId (x+x1, y+y1)

inSameSpace :: Obj -> Obj -> Bool
inSameSpace obj1 obj2 = case (obj1^.space.location, obj2^.space.location) of
    (OnMap (x1,y1), OnMap (x2,y2)) -> True
    (InObj obj1 (x1,y1), InObj obj2 (x2,y2)) -> obj1 == obj2
    _ -> False

coordinates :: Obj -> (X,Y)
coordinates obj = case obj^.space.location of
    OnMap (x,y) -> (x,y)
    InObj _ (x,y) -> (x,y)

surrounds :: (Shape, Point) -> Point -> Bool
surrounds ((Rectangle w h), (x,y)) (x',y') = x <= x' && x' <= x+w && y <= y' && y' <= y+h
surrounds ((Circle r), (x,y)) (x',y') = (x-x')^^2 + (y-y')^^2 <= r^^2

between :: (X,Y) -> (X,Y) -> (X,Y) -> Bool
between (x1,y1) (x2,y2) (testX, testY) = x1 <= testX && testX <= x2 && y1 <= testY && testY <= y2

rectanglesOverlap :: (X,Y) -> (Width, Height) -> (X, Y) -> (Width, Height) -> Bool
rectanglesOverlap (x1,y1) (w1,h1) (x2,y2) (w2,h2) =
    any (between (x2,y2) (x2+w2, y2+h2)) [(x1,y1),(x1+w1,y1),(x1,y1+h1),(x1+w1,y1+h1)] ||
    any (between (x1,y1) (x1+w1, y1+h1)) [(x2,y2),(x2+w2,y2),(x2,y2+h2),(x2+w2,y2+h2)]

-- I am bad at geometry.
circleAndRectangleOverlap :: (X, Y, Radius) -> (X, Y, Width, Height) -> Bool
circleAndRectangleOverlap (x1,y1,r) (x2,y2,w,h) =
    any (flip surrounds (x1,y1)) (map ((,)(Circle r)) [(x2,y2),(x2+w,y2),(x2,y2+h),(x2+w,y2+h)])
    ||
    any (flip surrounds (x1,y1)) [(Rectangle (w+2*r) h, (x2-r,y2)), (Rectangle w (h+2*r), (x2, y2-r))]

collide :: Obj -> Obj -> Bool
collide obj1 obj2
    | inSameSpace obj1 obj2 = 
        let loc1 = obj1^.space.location
            loc2 = obj2^.space.location
            shape1 = obj1^.space.shape
            shape2 = obj2^.space.shape in
        case (shape1, shape2) of
            (Circle r1, Circle r2) -> 
                (loc1^._x - loc2^._x)^^2 + (loc1^._y - loc2^._y)^^2 <= (r1 + r2)^^2
            (Rectangle w1 h1, Rectangle w2 h2) ->
                rectanglesOverlap (loc1^._x,loc1^._y) (w1,h1) (loc2^._x,loc2^._y) (w2,h2)
            (Circle r, Rectangle w h) -> circleAndRectangleOverlap (loc1^._x, loc1^._y, r) (loc2^._x, loc2^._y, w, h)
            (Rectangle w h, Circle r) -> circleAndRectangleOverlap (loc2^._x, loc2^._y, r) (loc1^._x, loc1^._y, w, h)
    | otherwise = False

collidesWithAny :: Obj -> World -> Bool
collidesWithAny obj world = I.null $ I.filter (collide obj) (world ^. objs)

destinationCoordinates :: Obj -> World -> Maybe (X,Y)
destinationCoordinates obj world = case obj^.space.destination of
    Just (ToMap (x,y)) -> Just (x,y)
    Just (ToObj objId) -> fmap coordinates $ I.lookup objId (world^.objs)
    Just (ToTask taskId) -> coordinates <$> do
        task <- I.lookup taskId (world^.tasks)
        I.lookup (task^.object) (world^.objs)
    Nothing -> Nothing

nextStep :: Time -> Obj -> World -> Maybe (X,Y)
nextStep time obj world = case destinationCoordinates obj world of
    Nothing -> Nothing
    Just (x2,y2) -> let (x1,y1) = coordinates obj
                        hyp = (x2-x1)^^2 + (y2-y1)^^2
                        s = obj^.space.speed in
                    Just (time/1000 * s * (x2-x1)/hyp, time/1000 * s * (y2-y1)/hyp)

moveObj :: Time -> Obj -> World -> Obj
moveObj time obj world = case nextStep time obj world of
    Nothing -> space.destination .~ Nothing $ obj
    Just (x,y) -> if collidesWithAny (moveBy (x,y) obj) world then obj else moveBy (x,y) obj

moveInWorld :: Time -> Obj -> World -> World
moveInWorld time obj world = objs %~ (I.insert (obj^.objId) (moveObj time obj world)) $ world

moveWorld :: Time -> World -> World
moveWorld time world = I.foldr (moveInWorld time) world (world^.objs)

-- TaskSystem

getSkill :: SkillType -> Obj -> Skill
getSkill skillType obj = fromMaybe (Skill skillType 0  0) (M.lookup skillType (view (task.skills) obj))

canWorkOn :: Obj -> Task -> Bool
canWorkOn obj task = getSkill (task^.skill) obj ^. level >= task ^. difficulty 

workIsComplete :: Work -> Bool
workIsComplete work = view complete work >= 100

applyWork :: Work -> World -> World
applyWork work world
    | workIsComplete work = world & tasks.at (work^.task).traverse.workCompleted +~ 1 
    | otherwise           = world

logWork :: Int -> Task -> Task
logWork int = workCompleted +~ int

tickWork' :: Time -> ObjId -> World -> Maybe Work
tickWork' time objId world = do
    obj <- world^.objs.at objId
    work <- obj^.task.work
    task <- world^.tasks.at (work^.task)
    if canWorkOn obj task 
        then Just (complete +~ (time * fromIntegral (view level (getSkill (view skill task) obj))) $ work) 
        else Nothing

tickWork'' :: Time -> ObjId -> World -> World
tickWork'' time objId world = case tickWork' time objId world of
    Nothing -> over objs (I.adjust (set (task.work) Nothing) objId) world
    Just work' -> if workIsComplete work' 
        then applyWork work' $
             over objs (I.adjust (set (task.work) Nothing) objId) world 
        else over objs (I.adjust (set (task.work) (Just work')) objId) world

tickWorks :: Time -> World -> World
tickWorks time world = foldr (tickWork'' time) world (I.keys $ view objs world)

taskIsComplete :: Task -> Bool
taskIsComplete task = view workCompleted task >= view workRequired task

tickTask :: Task -> World -> World
tickTask task world
    | taskIsComplete task = world -- view outcome task world 
    | otherwise           =  world

tickTasks :: World -> World
tickTasks world = I.foldr tickTask world (view tasks world)

tickWorld :: Time -> World -> World
tickWorld time world = moveWorld time $ tickTasks $ tickWorks time world
