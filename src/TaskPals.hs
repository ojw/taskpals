{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies, 
    TypeSynonymInstances, FlexibleInstances, DeriveFunctor, OverloadedStrings,
    FlexibleContexts, DeriveDataTypeable #-}

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
import Control.Monad.State
import Control.Monad.Reader
import Data.Data
import Data.Aeson.TH
import qualified Data.Char as Char
import qualified Data.HashMap.Strict as H

type ObjId = Int
type TaskId = (ObjId, Int)
type Time = Double
type X = Double
type Y = Double
type Point = (X,Y)
type Radius = Double
type Width = Double
type Height = Double
type Speed = Double
type Player = Text
type Tag = Text

data SkillType = Labor | Combat | Medical | Mechanical | Chemical | Hacking | Observation
    deriving (Eq, Ord, Read, Show, Data, Typeable)

data WorkType = Generic | Open | Close | Create | Break | Unlock | Hack | Fix | Heal | Barricade | Use
    deriving (Eq, Ord, Read, Show, Data, Typeable)

data Shape = Circle Radius | Rectangle Width Height deriving (Read, Show, Data, Typeable, Eq)

data Destination = ToMap (X,Y) | ToObj ObjId  deriving (Read, Show, Data, Typeable, Eq)-- -- | ToTask TaskId

data Target = None | Self | AtObj ObjId | WithinRadius Radius | InCircle Location Radius | InRectangle Location (Width, Height) {- | WithTag -}
 deriving (Read, Show, Data, Typeable, Eq)

data TaskEvent = ResetThisTask | DisableThisTask | ActivateTask TaskId | DisableTask TaskId | SetBlocking Bool | CreateWork WorkType Int Target | RemoveThisObj | ReplaceThisObjWith ObjId
 deriving (Read, Show, Data, Typeable, Eq)

-------------------------------------------------------------------------------

data Skill = Skill
    { _skillSkillType :: SkillType
    , _skillLevel :: Int
    , _skillSpeed :: Int
    } deriving (Read, Show, Data, Typeable, Eq)

-------------------------------------------------------------------------------

data Command = Command
    { _commandPlayer :: Player
    , _commandObjId :: ObjId
    , _commandGoal :: Goal
    }

data GoalPref = NeverAct | UseSkill SkillType | WorkOnType WorkType | GoalPrefs [GoalPref]
    | Target GoalPref
    deriving (Read, Show, Data, Typeable, Eq)

data Goal = GoTo Destination | WorkOn TaskId deriving (Read, Show, Data, Typeable, Eq)

data WorkComponent = WorkComponent
    { _worksystemGoalPref :: GoalPref
    , _worksystemGoal :: Maybe Goal
    , _worksystemWork :: Maybe Work
    , _worksystemSkills :: [Skill]
    } deriving (Read, Show, Eq)

data Work = Work
    { _workTask :: TaskId
    , _workComplete :: Double
    , _workTarget :: Maybe Target
    } deriving (Read, Show, Data, Typeable, Eq)

data Task = Task
    { _taskTaskId :: TaskId
    , _taskOwner :: ObjId
    , _taskName :: Text
    , _taskWorkType :: WorkType
    , _taskSkill :: SkillType
    , _taskDifficulty :: Int
    , _taskWorkRequired :: Int
    , _taskWorkCompleted :: Int
    , _taskOutcome :: [TaskEvent] -- World -> World -- ? or Task -> World -> World or something else
    , _taskEnabled :: Bool
    }  deriving (Read, Show, Eq)

data Location = OnMap (X,Y) | InObj ObjId (X,Y) deriving (Read, Show, Data, Typeable, Eq)

data ObjMeta = ObjMeta
    { _objidentityName :: Text
    , _objidentityTags :: [Tag]
    } deriving (Read, Show, Data, Typeable, Eq)

data Physics = Physics
    { _physicsLocation :: Location
    , _physicsShape :: Shape
    , _physicsSpeed :: Speed
    , _physicsBlocking :: Bool
    } deriving (Read, Show, Data, Typeable, Eq)

data World = World
    { _worldNextObj :: ObjId
    , _worldTime :: Time
    , _worldWork :: IntMap WorkComponent
    , _worldTasks :: IntMap (IntMap Task)
    , _worldPhysics :: IntMap Physics
    , _worldObjMetas :: IntMap ObjMeta
    } deriving (Read, Show, Eq)

makeFields ''ObjMeta
makeFields ''Command
makeFields ''Physics
makeFields ''Work
makeFields ''Skill
makeFields ''Task 
makeFields ''World
makeFields ''WorkComponent

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

deriveJSON (dropWhile (not . Char.isUpper)) ''Skill
deriveJSON (dropWhile (not . Char.isUpper)) ''SkillType
deriveJSON (dropWhile (not . Char.isUpper)) ''WorkType
deriveJSON (dropWhile (not . Char.isUpper)) ''Command
deriveJSON (dropWhile (not . Char.isUpper)) ''Location
deriveJSON (dropWhile (not . Char.isUpper)) ''Destination
deriveJSON (dropWhile (not . Char.isUpper)) ''Goal
deriveJSON (dropWhile (not . Char.isUpper)) ''World
deriveJSON (dropWhile (not . Char.isUpper)) ''Task
deriveJSON (dropWhile (not . Char.isUpper)) ''Physics
deriveJSON (dropWhile (not . Char.isUpper)) ''ObjMeta
deriveJSON (dropWhile (not . Char.isUpper)) ''TaskEvent
deriveJSON (dropWhile (not . Char.isUpper)) ''Shape
deriveJSON (dropWhile (not . Char.isUpper)) ''GoalPref
deriveJSON (dropWhile (not . Char.isUpper)) ''Target
deriveJSON (dropWhile (not . Char.isUpper)) ''Work
deriveJSON (dropWhile (not . Char.isUpper)) ''WorkComponent

-- Ignoring player / obj validation for now
addCommand :: Command -> IntMap Goal -> IntMap Goal
addCommand (Command _ objId goal) goals = I.insert objId goal goals

addCommands :: [Command] -> IntMap Goal -> IntMap Goal
addCommands commands goals = foldr addCommand goals commands

-- Skipping GoalPrefs for now

workOn :: Task -> WorkComponent -> (WorkComponent, Maybe (Int, TaskId))
workOn task system = if task^.enabled
    then if canWorkOn task system
            then if isComplete work' 
                    then (system & work .~ Nothing, Just ((releventSkillFor task system)^.level, task^.taskId))
                    else (system & work .~ Just work', Nothing)
            else (system & work .~ Nothing, Nothing)
    else (system & work .~ Nothing & goal .~ Nothing, Nothing)
      where
        work' = maybe (Work (task^.taskId) 0 Nothing) (complete+~1) (system^.work)
        -- Stupidly uses first relevent skill it encounters.
        canWorkOn :: Task -> WorkComponent -> Bool
        canWorkOn task system = task^.difficulty < (releventSkillFor task system)^.level
        releventSkillFor :: Task -> WorkComponent -> Skill
        releventSkillFor task system = case filter ((== task^.skill) . (view skillType)) (system^.skills) of
            [] -> Skill (task^.skill) 0 0
            (sk:sks) -> sk
        isComplete :: Work -> Bool
        isComplete work = work^.complete >= 100

tickWork :: World -> ObjId -> WorkComponent -> (WorkComponent, (Maybe (Int, TaskId), Maybe Destination))
tickWork world objId ws = case ws^.goal of
    Nothing                         -> (ws & work.~ Nothing, (Nothing, Nothing))
    Just (GoTo destination)         -> (ws,                  (Nothing, Just destination))
    Just (WorkOn (taskObj, taskId)) -> (ws',                 (done,    dest))
      where
        (ws', done) = case world^?tasks.at taskObj.traversed.at taskId.traversed of
            Just task -> workOn task ws
            Nothing -> (ws, Nothing)
        dest = Just $ ToObj taskObj

addWorkToTask :: Task -> Int -> (Task, Maybe [TaskEvent])
addWorkToTask task work = if work > task^.difficulty 
    then if task'^.workCompleted >= task'^.workRequired
            then (task', Just $ task'^.outcome)
            else (task', Nothing)
    else (task, Nothing)
  where
    task' = task & workCompleted +~ (work - task^.difficulty)
        
-- the Int is completed work to add to task completion
tickTask :: World -> ObjId -> Task -> Int -> (Task, Maybe [TaskEvent])
tickTask world objId task newWork = addWorkToTask task newWork

-- Physics

overlap :: Physics -> Physics -> Bool
overlap phys1 phys2 = inSameSpace loc1 loc2 && 
        case (shape1, shape2) of
            (Circle r1, Circle r2) -> 
                (loc1^._x - loc2^._x)^^2 + (loc1^._y - loc2^._y)^^2 <= (r1 + r2)^^2
            (Rectangle w1 h1, Rectangle w2 h2) ->
                rectanglesOverlap (loc1^._x,loc1^._y) (w1,h1) (loc2^._x,loc2^._y) (w2,h2)
            (Circle r, Rectangle w h) -> 
                circleAndRectangleOverlap (loc1^._x, loc1^._y, r) (loc2^._x, loc2^._y, w, h)
            (Rectangle w h, Circle r) -> 
                circleAndRectangleOverlap (loc2^._x, loc2^._y, r) (loc1^._x, loc1^._y, w, h)
  where
    loc1 = phys1^.location
    shape1 = phys1^.shape
    loc2 = phys2^.location
    shape2 = phys2^.shape
    rectanglesOverlap :: (X,Y) -> (Width, Height) -> (X, Y) -> (Width, Height) -> Bool
    rectanglesOverlap (x1,y1) (w1,h1) (x2,y2) (w2,h2) =
        any ((Rectangle w2 h2, (x2,y2)) `surrounds`) [(x1,y1),(x1+w1,y1),(x1,y1+h1),(x1+w1,y1+h1)] ||
        any ((Rectangle w1 h1, (x1,y1)) `surrounds`) [(x2,y2),(x2+w2,y2),(x2,y2+h2),(x2+w2,y2+h2)]
    circleAndRectangleOverlap :: (X, Y, Radius) -> (X, Y, Width, Height) -> Bool
    circleAndRectangleOverlap (x1,y1,r) (x2,y2,w,h) =
        any (`surrounds` (x1,y1)) (map ((,)(Circle r)) [(x2,y2),(x2+w,y2),(x2,y2+h),(x2+w,y2+h)]) ||
        any (`surrounds` (x1,y1)) [(Rectangle (w+2*r) h, (x2-r,y2)), (Rectangle w (h+2*r), (x2, y2-r))]

collide :: Physics -> Physics -> Bool
collide phys1 phys2 = overlap phys1 phys2 && phys1^.blocking && phys2^.blocking

speedConstant :: Double
speedConstant = 1

moveBy :: (X,Y) -> Physics -> Physics
moveBy (x,y) phys = case phys^.location of
    OnMap (x1,y1) -> phys & location .~ OnMap (x+x1, y+y1)
    InObj objId (x1,y1) -> phys & location .~ InObj objId (x+x1, y+y1)

inSameSpace :: Location -> Location -> Bool
inSameSpace (OnMap (x1,y1)) (OnMap (x2,y2)) = True
inSameSpace (InObj obj1 (x1,y1)) (InObj obj2 (x2,y2)) = obj1 == obj2
inSameSpace _  _ = False

coordinates :: Physics -> (X,Y)
coordinates phys = case phys^.location of
    OnMap (x,y) -> (x,y)
    InObj _ (x,y) -> (x,y)

surrounds :: (Shape, Point) -> Point -> Bool
surrounds (Rectangle w h, (x,y)) (x',y') = x <= x' && x' <= x+w && y <= y' && y' <= y+h
surrounds (Circle r, (x,y)) (x',y') = (x-x')^^2 + (y-y')^^2 <= r^^2

collidesWithAny :: Physics -> World -> Bool
collidesWithAny obj world = I.null $ I.filter (collide obj) (world ^. physics)

nextStep :: Time -> World -> Physics -> Destination -> Maybe (X,Y)
nextStep time world phys destination = case destinationCoordinates world destination of
    Nothing -> Nothing
    Just (x2,y2) -> let x1 = phys^.location._x
                        y1 = phys^.location._y
                        hyp = (x2-x1)^^2 + (y2-y1)^^2
                        s = phys^.speed in
                    Just (time/1000 * s * (x2-x1)/hyp, speedConstant * time * s * (y2-y1)/hyp)
  where destinationCoordinates world (ToMap (x,y)) = Just (x, y)
        destinationCoordinates world (ToObj objId) = case world^.physics.at objId of
            Nothing -> Nothing
            Just phys -> Just (phys^.location._x, phys^.location._y)

moveObj :: Time -> World -> Destination -> Physics -> Physics
moveObj time world destination phys = case nextStep time world phys destination of
    Nothing -> phys -- blocking shouldn't cause obj to give up on movement -- space.destination .~ Nothing $ obj
    Just (x,y) -> if collidesWithAny phys' world then phys else phys'
      where
        phys' = moveBy (x,y) phys

{--- Event system-}

inTarget :: Target -> ObjId -> ObjId -> World -> Bool
inTarget None _ _ _ = False
inTarget Self targetter target world = targetter == target
inTarget (AtObj targetId) targetter target world = targetId == target
inTarget (WithinRadius radius) targetter target world = maybe False (uncurry overlap) $ do
    targetterLocation <- world^?physics.at targetter.traversed.location
    targetPhysics <- world^.physics.at target
    return (Physics targetterLocation (Circle radius) 0 False, targetPhysics)
inTarget (InCircle locatn radius) targetter target world = maybe False (uncurry overlap) $ do
    targetPhysics <- world^.physics.at target
    return (Physics locatn (Circle radius) 0 False, targetPhysics)
inTarget (InRectangle locatn (width, height)) targetter target world = maybe False (uncurry overlap) $ do
    targetPhysics <- world^.physics.at target
    return (Physics locatn (Rectangle width height) 0 False, targetPhysics)
    

runEvent :: TaskId -> ObjId -> TaskEvent -> World -> World
runEvent taskId objId ResetThisTask = tasks.at (fst taskId).traversed.at (snd taskId).traversed.workCompleted .~ 0
runEvent taskId objId DisableThisTask = tasks.at (fst taskId).traversed.at (snd taskId).traversed.enabled .~ False
runEvent taskId objId (ActivateTask taskId') = tasks.at (fst taskId').traversed.at (snd taskId').traversed.enabled .~ True
runEvent taskId objId (DisableTask taskId') = tasks.at (fst taskId').traversed.at (snd taskId').traversed.enabled .~ False
runEvent taskId objId (SetBlocking blocking') = physics.at objId.traversed.blocking .~ blocking'
runEvent taskId objId (CreateWork workType amount target) = id 
-- runEvent taskId objId RemoveThisObj = removeObj objId
-- runEvent taskId objId (ReplaceThisObjWith obj') = objs %~ I.insert objId obj' 
