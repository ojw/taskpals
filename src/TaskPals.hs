{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies, 
    TypeSynonymInstances, FlexibleInstances, DeriveFunctor, OverloadedStrings,
    FlexibleContexts #-}

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
type Player = Text

data SkillType = Labor | Combat | Medical | Mechanical | Chemical | Hacking | Observation
    deriving (Eq, Ord, Show)

data WorkType = Open | Close | Create | Break | Unlock | Hack | Fix | Heal | Barricade | Use

data Game = Game
    { _gamePlayers :: Map Player [ObjId]
    , _gameState :: World
    }

data World = World
    { _worldObjs :: IntMap Obj
    , _worldTasks :: IntMap Task
    , _worldNextObj :: Int
    , _worldNextTask :: Int
    }

data Obj = Obj
    { _objObjId :: ObjId
    , _objTask :: TaskSystem
    , _objSpace :: SpatialSystem
    }

data SpatialSystem = SpatialSystem
    { _spatialsystemLocation :: Location
    , _spatialsystemShape :: Shape
    , _spatialsystemSpeed :: Speed
    , _spatialsystemDestination :: Maybe Destination
    , _spatialsystemBlocking :: Bool
    }

data Location = OnMap (X,Y) | InObj ObjId (X,Y)
data Shape = Circle Radius | Rectangle Width Height
data Destination = ToMap (X,Y) | ToObj ObjId -- -- | ToTask TaskId
data Goal = NoGoal | GoTo Destination | WorkOn TaskId

data TaskSystem = TaskSystem
    { _tasksystemTasks :: [TaskId]
    , _tasksystemSkills :: Map SkillType Skill
    , _tasksystemWork :: Maybe Work
    , _tasksystemGoal :: Goal
    }

data Work = Work
    { _workTask :: TaskId
    , _workComplete :: Double
    , _workTarget :: Maybe Target -- I think this is the safest place for target (obj whose work completes task picks target)
    , _workLevel :: Int -- is same as objs' skill level... duplication bad and hopefully temporary
    }

data Skill = Skill
    { _skillSkillType :: SkillType
    , _skillLevel :: Int
    , _skillSpeed :: Int
    }

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

data Target = None | Self | AtObj ObjId | WithinRadius Radius | InCircle (X,Y) Radius | InRectangle (X,Y) (Width, Height)

data TaskEvent = ResetThisTask | RemoveThisTask | AddTask Task | SetBlocking Bool | CreateWork WorkType Int Target | RemoveThisObj | ReplaceThisObjWith Obj

makeFields ''TaskSystem
makeFields ''SpatialSystem
makeFields ''Obj
makeFields ''Work
makeFields ''Skill
makeFields ''Task 
makeFields ''World
makeFields ''Game

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

-- Event system

inTarget :: Target -> ObjId -> Obj -> Bool
inTarget target objId obj = False

runEvent :: TaskId -> ObjId -> TaskEvent -> World -> World
runEvent taskId objId ResetThisTask world = world & tasks.at taskId.traversed.workCompleted .~ 0
runEvent taskId objId RemoveThisTask world = removeTask taskId world
runEvent taskId objId (AddTask task') world = world & nextTask %~ succ 
                                                    & tasks %~ I.insert (world^.nextTask) task' 
                                                    & objs.at objId.traversed.task.tasks <>~ pure (world^.nextTask)
runEvent taskId objId (SetBlocking blocking') world = world & objs.at objId.traversed.space.blocking .~ blocking'
runEvent taskId objId (CreateWork workType amount target) world = world 
runEvent taskId objId RemoveThisObj world = world & objs %~ sans objId
runEvent taskId objId (ReplaceThisObjWith obj') world = world & objs %~ I.insert objId obj'

obj :: Obj
obj = Obj 0 (TaskSystem [] M.empty Nothing NoGoal) (SpatialSystem (OnMap (0,0)) (Circle 10) 10 Nothing True)

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
removeTask taskId = tasks %~ I.delete taskId -- objs working on taskId will give up next tick

-- LocationSystem 

speedConstant :: Double
speedConstant = 1

moveBy :: (X,Y) -> Obj -> Obj
moveBy (x,y) obj = case obj^.space.location of
    OnMap (x1,y1) -> obj & space.location .~ OnMap (x+x1, y+y1)
    InObj objId (x1,y1) -> obj & space.location .~ InObj objId (x+x1, y+y1)

inSameSpace' :: Location -> Location -> Bool
inSameSpace' (OnMap (x1,y1)) (OnMap (x2,y2)) = True
inSameSpace' (InObj obj1 (x1,y1)) (InObj obj2 (x2,y2)) = obj1 == obj2
inSameSpace' _  _ = False

inSameSpace :: Obj -> Obj -> Bool
inSameSpace obj1 obj2 = inSameSpace' (obj1^.space.location) (obj2^.space.location)

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

overlap' :: (Location, Shape) -> (Location, Shape) -> Bool
overlap' (loc1, shape1) (loc2, shape2)
    | inSameSpace' loc1 loc2 = 
        case (shape1, shape2) of
            (Circle r1, Circle r2) -> 
                (loc1^._x - loc2^._x)^^2 + (loc1^._y - loc2^._y)^^2 <= (r1 + r2)^^2
            (Rectangle w1 h1, Rectangle w2 h2) ->
                rectanglesOverlap (loc1^._x,loc1^._y) (w1,h1) (loc2^._x,loc2^._y) (w2,h2)
            (Circle r, Rectangle w h) -> 
                circleAndRectangleOverlap (loc1^._x, loc1^._y, r) (loc2^._x, loc2^._y, w, h)
            (Rectangle w h, Circle r) -> 
                circleAndRectangleOverlap (loc2^._x, loc2^._y, r) (loc1^._x, loc1^._y, w, h)
    | otherwise = False

overlap :: Obj -> Obj -> Bool
overlap obj1 obj2 = overlap' (obj1^.space.location, obj1^.space.shape) (obj2^.space.location, obj2^.space.shape)

collide :: Obj -> Obj -> Bool
collide obj1 obj2 = overlap obj1 obj2 && (obj1^.space.blocking && obj2^.space.blocking)

collidesWithAny :: Obj -> World -> Bool
collidesWithAny obj world = I.null $ I.filter (collide obj) (world ^. objs)

destinationCoordinates :: Obj -> World -> Maybe (X,Y)
destinationCoordinates obj world = case obj^.task.goal of
    GoTo (ToMap (x,y))-> Just (x,y)
    GoTo (ToObj objId)-> fmap coordinates $ I.lookup objId (world^.objs)
    {-GoTo (ToTask taskId)-> coordinates <$> do-}
        {-task <- I.lookup taskId (world^.tasks)-}
        {-I.lookup (task^.object) (world^.objs)-}
    WorkOn taskId -> coordinates <$> do
        task <- I.lookup taskId (world^.tasks)
        I.lookup (task^.object) (world^.objs)
    NoGoal -> Nothing

nextStep :: Time -> Obj -> World -> Maybe (X,Y)
nextStep time obj world = case destinationCoordinates obj world of
    Nothing -> Nothing
    Just (x2,y2) -> let (x1,y1) = coordinates obj
                        hyp = (x2-x1)^^2 + (y2-y1)^^2
                        s = obj^.space.speed in
                    Just (time/1000 * s * (x2-x1)/hyp, speedConstant * time * s * (y2-y1)/hyp)

moveObj :: Time -> World -> Obj -> Obj
moveObj time world obj = case nextStep time obj world of
    Nothing -> obj -- blocking shouldn't cause obj to give up on movement -- space.destination .~ Nothing $ obj
    Just (x,y) -> if collidesWithAny (moveBy (x,y) obj) world then obj else moveBy (x,y) obj

moveInWorld :: Time -> Obj -> World -> World
moveInWorld time obj world = world & objs %~ (I.adjust (moveObj time world) (obj^.objId))

tickLocations :: Time -> World -> World
tickLocations time world = I.foldr (moveInWorld time) world (world^.objs)

-- TaskSystem

workConstant :: Double
workConstant = 1

setGoal :: ObjId -> Goal -> World -> World
setGoal objId goal' = objs.at objId.traversed.task.goal .~ goal'

isWorkingOnTask :: Obj -> TaskId -> Bool
isWorkingOnTask obj taskId = case (obj^.task.goal, obj^.task.work) of
    (WorkOn taskId', Just work) -> taskId' == work^.task
    _ -> False

getSkill :: SkillType -> Obj -> Skill
getSkill skillType obj = fromMaybe (Skill skillType 0  0) (M.lookup skillType (view (task.skills) obj))

-- missing location check
canWorkOn :: Obj -> Task -> Bool
canWorkOn obj task = getSkill (task^.skill) obj ^. level >= task ^. difficulty 

workIsComplete :: Work -> Bool
workIsComplete work = view complete work >= 100

applyWork :: Work -> World -> World
applyWork work world
    | workIsComplete work = world & tasks.at (work^.task).traverse %~ \tsk -> tsk & workCompleted +~ (work^.level - tsk^.difficulty)
    | otherwise           = world

tickWork :: Time -> ObjId -> World -> World
tickWork time objId world = case mWork of
    Nothing -> world & objs.at objId.traversed.task.work .~ Nothing
                     & objs.at objId.traversed.task.goal %~ \g -> case g of
                        WorkOn _ -> NoGoal
                        goal -> goal
    Just work' -> if workIsComplete work' 
        then applyWork work' $
             world & objs.at objId.traversed.task.work .~ Nothing
                   & objs.at objId.traversed.task.goal .~ NoGoal
        else world & objs.at objId.traversed.task.work .~ Just work'
  where
    mWork = do
        obj <- world^.objs.at objId
        objWork <- obj^.task.work
        objTask <- world^.tasks.at (objWork^.task)
        if canWorkOn obj objTask && isWorkingOnTask obj (objWork^.task)
            then Just (complete +~ (workConstant * time * fromIntegral 
                        (view level (getSkill (view skill objTask) obj))) $ objWork)
            else Nothing

tickWorks :: Time -> World -> World
tickWorks time world = foldr (tickWork time) world (I.keys $ view objs world)

taskIsComplete :: Task -> Bool
taskIsComplete task = view workCompleted task >= view workRequired task

tickTask :: TaskId -> Task -> World -> World
tickTask taskId task world  
    | taskIsComplete task = foldr (runEvent taskId (task^.object)) world (task^.outcome)
    | otherwise = world

tickTasks :: World -> World
tickTasks world = I.foldWithKey tickTask world (world^.tasks)

tickWorld :: Time -> World -> World
tickWorld time world = tickLocations time $ tickTasks $ tickWorks time world

addTask :: MonadState World m => Task -> m TaskId
addTask task = do
    taskId <- nextTask <%= succ
    tasks %= I.insert taskId task
    return taskId

addObj :: MonadState World m => Obj -> m ObjId
addObj obj = do
    objId <- nextObj <%= succ
    objs %= I.insert objId obj
    return objId

authenticateGoals :: Game -> [(Player, (ObjId, Goal))] -> [(ObjId, Goal)]
authenticateGoals game = map snd . (filter $ \(player, (objId, goal)) -> 
    objId `elem` (M.findWithDefault [] player (game^.players)))

-- rewrite with monads, cleaner separation

tick :: Time -> [(Player, (ObjId, Goal))] -> ReaderT Time (State Game) ()
tick time goals = do
    game <- get
    let goals' = authenticateGoals game goals
    zoom TaskPals.state $ do
        updateGoals goals'
        tickMovement
        tickWorkNew
        events <- tickTasksNew
        runEvents events

updateGoals :: (MonadReader Time m, MonadState World m) => [(ObjId, Goal)] -> m ()
updateGoals goals = do
    world <- get
    put $ foldr (uncurry setGoal) world goals

tickMovement :: (MonadReader Time m, MonadState World m) => m ()
tickMovement = do
    world <- get
    time <- ask
    put $ I.foldr (moveInWorld time) world (world^.objs)

tickWorkNew :: (MonadReader Time m, MonadState World m) => m ()
tickWorkNew = undefined

tickTasksNew :: (MonadReader Time m, MonadState World m) => m [(TaskId, ObjId, TaskEvent)]
tickTasksNew = do
    world <- get
    return $ I.foldWithKey tickTaskNew [] (world^.tasks)

tickTaskNew :: TaskId -> Task -> [(TaskId, ObjId, TaskEvent)] -> [(TaskId, ObjId, TaskEvent)]
tickTaskNew taskId task events
    | taskIsComplete task = events ++ map ((,,) taskId (task^.object)) (task^.outcome)
    | otherwise = events

runEvents :: (MonadReader Time m, MonadState World m) => [(TaskId, ObjId, TaskEvent)] -> m ()
runEvents events = do
    world <- get
    put $ foldr (\(taskId, objId, event) -> runEvent taskId objId event) world events
