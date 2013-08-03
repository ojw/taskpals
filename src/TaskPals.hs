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

data MetaComponent = MetaComponent
    { _objidentityName :: Text
    , _objidentityTags :: [Tag]
    } deriving (Read, Show, Data, Typeable, Eq)

data PhysicsComponent = PhysicsComponent
    { _physicsLocation :: Location
    , _physicsShape :: Shape
    , _physicsSpeed :: Speed
    , _physicsBlocking :: Bool
    } deriving (Read, Show, Data, Typeable, Eq)

type TaskComponent = IntMap Task

data World = World
    { _worldNextObj :: ObjId
    , _worldTime :: Time
    , _worldWork :: IntMap WorkComponent
    , _worldTasks :: IntMap TaskComponent
    , _worldPhysics :: IntMap PhysicsComponent
    , _worldMeta :: IntMap MetaComponent
    } deriving (Read, Show, Eq)

makeFields ''MetaComponent
makeFields ''Command
makeFields ''PhysicsComponent
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
deriveJSON (dropWhile (not . Char.isUpper)) ''PhysicsComponent
deriveJSON (dropWhile (not . Char.isUpper)) ''MetaComponent
deriveJSON (dropWhile (not . Char.isUpper)) ''TaskEvent
deriveJSON (dropWhile (not . Char.isUpper)) ''Shape
deriveJSON (dropWhile (not . Char.isUpper)) ''GoalPref
deriveJSON (dropWhile (not . Char.isUpper)) ''Target
deriveJSON (dropWhile (not . Char.isUpper)) ''Work
deriveJSON (dropWhile (not . Char.isUpper)) ''WorkComponent

-- Engine

tick :: Time -> [Command] -> State World ()
tick time commands = do
    world <- get
    zoom work $ addCommandsM commands
    (fin, dest) <- zoom work $ tickWorksM world
    events <- zoom tasks $ tickTasksM world fin
    zoom physics $ tickPhysicsM time world dest
    runEventsM events

-- Ignoring player / obj validation for now

addCommands :: [Command] -> IntMap WorkComponent -> IntMap WorkComponent
addCommands commands wcs = foldr addCommand wcs commands
  where
    addCommand (Command _ objId goal') = at objId .traversed.goal .~ Just goal'

addCommandsM :: MonadState (IntMap WorkComponent) m => [Command] -> m ()
addCommandsM commands = modify $ addCommands commands

-- Skipping GoalPrefs for now

workOn :: ObjId -> Task -> WorkComponent -> (WorkComponent, Maybe (Int, ObjId, TaskId))
workOn objId task system = if task^.enabled
    then if canWorkOn task system
            then if isComplete work' 
                    then (system & work .~ Nothing, Just ((releventSkillFor task system)^.level, objId, task^.taskId))
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

tickWork :: World -> ObjId -> WorkComponent -> (WorkComponent, (Maybe (Int, ObjId, TaskId), Maybe Destination))
tickWork world objId wc = case wc^.goal of
    Nothing                         -> (wc & work.~ Nothing, (Nothing, Nothing))
    Just (GoTo destination)         -> (wc & work.~ Nothing, (Nothing, Just destination))
    Just (WorkOn (taskObj, taskId)) -> (wc',                 (done,    dest))
      where
        (wc', done) = case world^?tasks.at taskObj.traversed.at taskId.traversed of
            Just task -> workOn objId task wc
            Nothing -> (wc & goal .~ Nothing & work .~ Nothing, Nothing)
        dest = Just $ ToObj taskObj

tickWorks :: World -> IntMap WorkComponent -> (IntMap WorkComponent, [(Int, ObjId, TaskId)], [(ObjId, Destination)])
tickWorks world wcs = I.foldrWithKey f (wcs, [], []) wcs
  where
    f objId wc (workComponents, finishedWork, destinations) =
        let (wc', (fin, dest)) = tickWork world objId wc in
            (I.insert objId wc' workComponents, maybeToList fin ++ finishedWork, maybeToList (fmap ((,) objId) dest) ++ destinations)

tickWorksM :: MonadState (IntMap WorkComponent) m => World -> m ([(Int, ObjId, TaskId)], [(ObjId, Destination)])
tickWorksM world = do
    wcs <- get
    let (wc', fin, dest) = tickWorks world wcs
    put wc'
    return (fin, dest)

tickTasksM :: MonadState (IntMap TaskComponent) m => World -> [(Int, ObjId, TaskId)] -> m [(TaskId, ObjId, TaskEvent)]
tickTasksM world newWork = do
    tcs <- get
    let (tcs', events) = tickTasks world newWork tcs
    put tcs'
    return events
            
tickTasks :: World -> [(Int, ObjId, TaskId)] -> IntMap TaskComponent -> (IntMap TaskComponent, [(TaskId, ObjId, TaskEvent)])
tickTasks world newWork tcs = foldr f (tcs, []) newWork
  where
    f work (tcs, events) = let (tcs', events') = tickTask' world work tcs in (tcs', events' ++ events)

tickTask' :: World -> (Int, ObjId, TaskId) -> IntMap TaskComponent -> (IntMap TaskComponent, [(TaskId, ObjId, TaskEvent)])
tickTask' world (newWork, worker, taskId) tcs = case tcs^.at (fst taskId) of -- .traversed.at (snd taskId) of
    Nothing -> (tcs, [])
    Just tasks -> case tasks^.at (snd taskId) of
        Nothing -> (tcs, [])
        Just task -> case tickTask world worker taskId task newWork of
            (task', mEvents) -> (tcs & I.adjust (I.insert (snd taskId) task') (fst taskId), join $ maybeToList mEvents)

-- the Int is completed work to add to task completion
tickTask :: World -> ObjId -> TaskId -> Task -> Int -> (Task, Maybe [(TaskId, ObjId, TaskEvent)])
tickTask world objId taskId task newWork = addWorkToTask task newWork
  where
    addWorkToTask :: Task -> Int -> (Task, Maybe [(TaskId, ObjId, TaskEvent)])
    addWorkToTask task work = if work > task^.difficulty 
        then if task'^.workCompleted >= task'^.workRequired
                then (task', Just $ map ((,,) taskId objId) $ task'^.outcome)
                else (task', Nothing)
        else (task, Nothing)
      where
        task' = task & workCompleted +~ (work - task^.difficulty)
        

-- PhysicsComponent

overlap :: PhysicsComponent -> PhysicsComponent -> Bool
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

collide :: PhysicsComponent -> PhysicsComponent -> Bool
collide phys1 phys2 = overlap phys1 phys2 && phys1^.blocking && phys2^.blocking

speedConstant :: Double
speedConstant = 1

moveBy :: (X,Y) -> PhysicsComponent -> PhysicsComponent
moveBy (x,y) phys = case phys^.location of
    OnMap (x1,y1) -> phys & location .~ OnMap (x+x1, y+y1)
    InObj objId (x1,y1) -> phys & location .~ InObj objId (x+x1, y+y1)

inSameSpace :: Location -> Location -> Bool
inSameSpace (OnMap (x1,y1)) (OnMap (x2,y2)) = True
inSameSpace (InObj obj1 (x1,y1)) (InObj obj2 (x2,y2)) = obj1 == obj2
inSameSpace _  _ = False

coordinates :: PhysicsComponent -> (X,Y)
coordinates phys = case phys^.location of
    OnMap (x,y) -> (x,y)
    InObj _ (x,y) -> (x,y)

surrounds :: (Shape, Point) -> Point -> Bool
surrounds (Rectangle w h, (x,y)) (x',y') = x <= x' && x' <= x+w && y <= y' && y' <= y+h
surrounds (Circle r, (x,y)) (x',y') = (x-x')^^2 + (y-y')^^2 <= r^^2

collidesWithAny :: PhysicsComponent -> World -> Bool
collidesWithAny obj world = I.null $ I.filter (collide obj) (world ^. physics)

nextStep :: Time -> World -> PhysicsComponent -> Destination -> Maybe (X,Y)
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

moveObj :: Time -> World -> Destination -> PhysicsComponent -> PhysicsComponent
moveObj time world destination phys = case nextStep time world phys destination of
    Nothing -> phys -- blocking shouldn't cause obj to give up on movement -- space.destination .~ Nothing $ obj
    Just (x,y) -> if collidesWithAny phys' world then phys else phys'
      where
        phys' = moveBy (x,y) phys

tickPhysics :: Time -> World -> [(ObjId, Destination)] -> IntMap PhysicsComponent -> IntMap PhysicsComponent
tickPhysics time world destinations pcs = foldr f pcs destinations
  where
    f (objId, destination) = at objId.traversed %~ moveObj time world destination

tickPhysicsM :: MonadState (IntMap PhysicsComponent) m => Time -> World -> [(ObjId, Destination)] -> m ()
tickPhysicsM time world destinations = modify $ tickPhysics time world destinations

{--- Event system-}

inTarget :: Target -> ObjId -> ObjId -> World -> Bool
inTarget None _ _ _ = False
inTarget Self targetter target world = targetter == target
inTarget (AtObj targetId) targetter target world = targetId == target
inTarget (WithinRadius radius) targetter target world = maybe False (uncurry overlap) $ do
    targetterLocation <- world^?physics.at targetter.traversed.location
    targetPhysics <- world^.physics.at target
    return (PhysicsComponent targetterLocation (Circle radius) 0 False, targetPhysics)
inTarget (InCircle locatn radius) targetter target world = maybe False (uncurry overlap) $ do
    targetPhysics <- world^.physics.at target
    return (PhysicsComponent locatn (Circle radius) 0 False, targetPhysics)
inTarget (InRectangle locatn (width, height)) targetter target world = maybe False (uncurry overlap) $ do
    targetPhysics <- world^.physics.at target
    return (PhysicsComponent locatn (Rectangle width height) 0 False, targetPhysics)
    
runEvents :: [(TaskId, ObjId, TaskEvent)] -> World -> World
runEvents events world = foldr f world events
  where
    f (taskId, objId, taskEvent) = runEvent taskId objId taskEvent

runEventsM :: MonadState World m => [(TaskId, ObjId, TaskEvent)] -> m ()
runEventsM events = modify $ runEvents events

runEvent :: TaskId -> ObjId -> TaskEvent -> World -> World
runEvent taskId objId ResetThisTask = tasks.at (fst taskId).traversed.at (snd taskId).traversed.workCompleted .~ 0
runEvent taskId objId DisableThisTask = tasks.at (fst taskId).traversed.at (snd taskId).traversed.enabled .~ False
runEvent taskId objId (ActivateTask taskId') = tasks.at (fst taskId').traversed.at (snd taskId').traversed.enabled .~ True
runEvent taskId objId (DisableTask taskId') = tasks.at (fst taskId').traversed.at (snd taskId').traversed.enabled .~ False
runEvent taskId objId (SetBlocking blocking') = physics.at objId.traversed.blocking .~ blocking'
runEvent taskId objId (CreateWork workType amount target) = id 
-- runEvent taskId objId RemoveThisObj = removeObj objId
-- runEvent taskId objId (ReplaceThisObjWith obj') = objs %~ I.insert objId obj' 
