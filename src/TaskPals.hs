{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies, 
    TypeSynonymInstances, FlexibleInstances, DeriveFunctor, OverloadedStrings,
    FlexibleContexts, DeriveDataTypeable #-}

module TaskPals where

import Control.Lens hiding ( (.=) )
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
import Data.Aeson
import Data.Aeson.TH
import qualified Data.Char as Char
import qualified Data.HashMap.Strict as H

type ObjId = Int
type TaskId = (ObjId, Int)
type TimeDelta = Double
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

data TaskEvent = ResetThisTask | DisableThisTask | ActivateTask TaskId | RemoveThisTask | AddTask TaskId | SetBlocking Bool | CreateWork WorkType Int Target | RemoveThisObj | ReplaceThisObjWith ObjId
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

data WorkSystem = WorkSystem
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

-------------------------------------------------------------------------------

data World = World
    { _worldNextObj :: ObjId
    , _worldTimeDelta :: TimeDelta
    , _worldWork :: IntMap WorkSystem
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
makeFields ''WorkSystem

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
deriveJSON (dropWhile (not . Char.isUpper)) ''WorkSystem

-- Ignoring player / obj validation for now
addCommand :: Command -> IntMap Goal -> IntMap Goal
addCommand (Command _ objId goal) goals = I.insert objId goal goals

addCommands :: [Command] -> IntMap Goal -> IntMap Goal
addCommands commands goals = foldr addCommand goals commands

-- Skipping GoalPrefs for now

releventSkillFor :: Task -> WorkSystem -> Skill
releventSkillFor task system = case filter ((== task^.skill) . (view skillType)) (system^.skills) of
    [] -> Skill (task^.skill) 0 0
    (sk:sks) -> sk

-- Stupidly uses first relevent skill it encounters.
canWorkOn :: Task -> WorkSystem -> Bool
canWorkOn task system = task^.difficulty < (releventSkillFor task system)^.level

isComplete :: Work -> Bool
isComplete work = work^.complete >= 100

workOn :: Task -> WorkSystem -> (WorkSystem, Maybe (Int, TaskId))
workOn task system = if canWorkOn task system
    then if isComplete work' 
            then (system & work .~ Nothing, Just ((releventSkillFor task system)^.level, task^.taskId))
            else (system & work .~ Just work', Nothing)
    else (system & work .~ Nothing, Nothing)
  where
    work' = maybe (Work (task^.taskId) 0 Nothing) (complete+~1) (system^.work)

tickWork :: World -> ObjId -> WorkSystem -> (WorkSystem, (Maybe (Int, TaskId), Maybe Destination))
tickWork world objId ws@(WorkSystem _ Nothing _ _) = (ws & goal.~ Nothing, (Nothing, Nothing))
tickWork world objId ws@(WorkSystem _ (Just (GoTo destination)) _ _) = (ws, (Nothing, Just destination))
tickWork world objId ws@(WorkSystem _ (Just (WorkOn (taskObj, taskId))) oldWork skills) = (ws', (done, dest))
  where
    (ws', done) = case world^?tasks.at taskObj.traversed.at taskId.traversed of
        Just task -> workOn task ws
        Nothing -> (ws, Nothing)
    dest = Just $ ToObj taskObj

{-checkGoal :: World -> ObjId -> Goal -> Maybe (Maybe Work, Destination)-}
{-checkGoal world objId goal@(GoTo destination) = (Just (Nothing, destination))-}
{-checkGoal world objId goal@(WorkOn (taskObj, taskId)) = do-}
    {-task <- world^?tasks.at taskObj.traversed.at taskId.traversed-}
    {-if task^.enabled-}
        {-then if canWorkOn-}
                {-then (Just (Just (startWorkOn task), ToObj (task^.owner)))-}
                {-else (Just (Nothing, ToObj (task^.owner)))-}
        {-else Nothing-}
  {-where-}
    {-workConstant = 1-}
    {-canWorkOn = case world^?tasks.at taskObj.traversed.at taskId.traversed of-}
        {-Nothing -> False-}
        {-Just task -> task^.difficulty < skillLevel-}
          {-where-}
            {-skillLevel = case filter ((==task^.skill) . view skillType) <$> world^?skills.at objId.traversed of-}
                {-Nothing -> 0-}
                {-Just [] -> 0-}
                {-Just (skill:skills) ->  skill^.level-}
    {-skillLevel skillType' = case filter ((==skillType') . view skillType) <$> world^?skills.at objId.traversed of-}
        {-Nothing -> 0-}
        {-Just [] -> 0-}
        {-Just (skill:skills) ->  skill^.level-}
    {-startWorkOn :: Task -> Work-}
    {-startWorkOn task = Work (taskObj, taskId) newWork Nothing-}
      {-where-}
        {-newWork = workConstant * (world^.timeDelta) * fromIntegral (skillLevel (task^.skill))-}

{-foo :: ObjId -> Maybe (Maybe Work, Destination) -}
    {--> (IntMap (GoalPref, Maybe Goal), (IntMap Work, IntMap Destination))-}
    {--> (IntMap (GoalPref, Maybe Goal), (IntMap Work, IntMap Destination))-}
{-foo objId Nothing (goals, (works, destinations)) = (I.adjust (_2.~Nothing) objId goals, (works, destinations))-}
{-foo objId (Just (Nothing, destination)) (goals, (works, destinations)) = (I.adjust (_2.~Nothing) objId goals, (works, I.insert objId destination destinations))-}
{-foo objId (Just (Just work, destination)) (goals, (works, destinations)) = (I.adjust (_2.~Nothing) objId goals, (I.insert objId work works, I.insert objId destination destinations))-}

{-_x :: Lens Location Location X X-}
{-_x = lens getX setX where-}
    {-getX loc = case loc of-}
        {-OnMap (x,_) -> x-}
        {-InObj _ (x,_) -> x-}
    {-setX loc x' = case loc of-}
        {-OnMap (x,y) -> OnMap (x',y)-}
        {-InObj objId (x,y) -> InObj objId (x',y)-}

{-_y :: Lens Location Location Y Y-}
{-_y = lens getY setY where-}
    {-getY loc = case loc of-}
        {-OnMap (_,y) -> y-}
        {-InObj _ (_,y) -> y-}
    {-setY loc y' = case loc of-}
        {-OnMap (x,y) -> OnMap (x,y')-}
        {-InObj objId (x,y) -> InObj objId (x,y')-}

{--- Goal system-}

{-startWorkOn :: TaskId -> ObjId -> World -> Maybe Work-}
{-startWorkOn taskId objId world = do -}
    {-obj <- world^.objs.at objId-}
    {-task <- world^.tasks.at taskId-}
    {-return $ Work taskId 0 Nothing $ getSkill (task^.skill) obj ^. level-}

{--- This ignores distance, whether obj reasonably knows about aspects of the world, etc-}
{-chooseGoal :: GoalPref -> World -> Maybe Goal-}
{-chooseGoal NeverAct _ = Nothing-}
{-chooseGoal (UseSkill skillType) world = case I.elems $ I.mapMaybeWithKey  (\taskId task -> if skillType == task^.skill then Just taskId else Nothing) (world^.tasks) of-}
    {-[] -> Nothing-}
    {-(taskId:tasksIds) -> Just (WorkOn taskId)-}
{-chooseGoal (WorkOnType workType') world = case I.elems $ I.mapMaybeWithKey (\taskId task -> if workType' == task^.workType then Just taskId else Nothing) (world^.tasks) of-}
    {-[] -> Nothing-}
    {-(taskId:tasksIds) -> Just (WorkOn taskId)-}
{-chooseGoal (GoalPrefs (pref:prefs)) world = case chooseGoal pref world of-}
    {-Nothing -> chooseGoal (GoalPrefs prefs) world-}
    {-Just goal -> Just goal-}
    

{--- Event system-}

{-inTarget :: Target -> ObjId -> ObjId -> World -> Bool-}
{-inTarget None _ _ _ = False-}
{-inTarget Self targetter target world = targetter == target-}
{-inTarget (AtObj targetId) targetter target world = targetId == target-}
{-inTarget (WithinRadius radius) targetter target world = maybe False (uncurry overlap') $ do-}
    {-loc1 <- world^.objs.at targetter <&> (^.space.location)-}
    {-loc2 <- world^.objs.at target <&> (^.space.location)-}
    {-shape2 <- world^.objs.at target <&> (^.space.shape)-}
    {-return ((loc1, Circle radius), (loc2, shape2))-}
{-inTarget (InCircle locatn radius) targetter target world = maybe False (uncurry overlap') $ do-}
    {-loc <- world^.objs.at target <&> (^.space.location)-}
    {-shape <- world^.objs.at target <&> (^.space.shape)-}
    {-return ((locatn, Circle radius), (loc, shape))-}
{-inTarget (InRectangle locatn (width, height)) targetter target world = maybe False (uncurry overlap') $ do-}
    {-loc <- world^.objs.at target <&> (^.space.location)-}
    {-shape <- world^.objs.at target <&> (^.space.shape)-}
    {-return ((locatn, Rectangle width height), (loc, shape))-}
    

{-runEvent :: TaskId -> ObjId -> TaskEvent -> World -> World-}
{-runEvent taskId objId ResetThisTask = tasks.at taskId.traversed.workCompleted .~ 0-}
{-runEvent taskId objId DisableThisTask = tasks.at taskId.traversed.enabled .~ False-}
{-runEvent taskId objId (ActivateTask taskId') = tasks.at taskId'.traversed.enabled .~ True-}
{-runEvent taskId objId RemoveThisTask = removeTask taskId-}
{-runEvent taskId objId (AddTask taskId') = objs.at objId.traversed.task.tasks <>~ [taskId']-}
{-runEvent taskId objId (SetBlocking blocking') = objs.at objId.traversed.space.blocking .~ blocking'-}
{-runEvent taskId objId (CreateWork workType amount target) = id -}
{-runEvent taskId objId RemoveThisObj = objs %~ sans objId-}
{-runEvent taskId objId (ReplaceThisObjWith obj') = objs %~ I.insert objId obj'-}

{-modObj :: (MonadState World m) => ObjId -> (Obj -> Obj) -> m () -- World -> World-}
{-modObj objId f = modify $ objs %~ (I.adjust f objId)-}

{-modTask :: (MonadState World m) => TaskId -> (Task -> Task) -> m () -- World -> World-}
{-modTask taskId f = modify $ tasks %~ (I.adjust f taskId)-}

{-modTaskObj :: (MonadState World m) => TaskId -> (Obj -> Obj) -> m () -- World -> World-}
{-modTaskObj taskId f = do-}
    {-world <- get    -}
    {-case I.lookup taskId (world^.tasks) of-}
        {-Nothing -> put world-}
        {-Just task -> modObj (task^.owner) f-}

{-removeTask :: TaskId -> World -> World-}
{-removeTask taskId = tasks %~ I.delete taskId -- objs working on taskId will give up next tick-}

{--- LocationSystem -}

{-speedConstant :: Double-}
{-speedConstant = 1-}

{-moveBy :: (X,Y) -> Obj -> Obj-}
{-moveBy (x,y) obj = case obj^.space.location of-}
    {-OnMap (x1,y1) -> obj & space.location .~ OnMap (x+x1, y+y1)-}
    {-InObj objId (x1,y1) -> obj & space.location .~ InObj objId (x+x1, y+y1)-}

{-inSameSpace' :: Location -> Location -> Bool-}
{-inSameSpace' (OnMap (x1,y1)) (OnMap (x2,y2)) = True-}
{-inSameSpace' (InObj obj1 (x1,y1)) (InObj obj2 (x2,y2)) = obj1 == obj2-}
{-inSameSpace' _  _ = False-}

{-inSameSpace :: Obj -> Obj -> Bool-}
{-inSameSpace obj1 obj2 = inSameSpace' (obj1^.space.location) (obj2^.space.location)-}

{-coordinates :: Obj -> (X,Y)-}
{-coordinates obj = case obj^.space.location of-}
    {-OnMap (x,y) -> (x,y)-}
    {-InObj _ (x,y) -> (x,y)-}

{-surrounds :: (Shape, Point) -> Point -> Bool-}
{-surrounds (Rectangle w h, (x,y)) (x',y') = x <= x' && x' <= x+w && y <= y' && y' <= y+h-}
{-surrounds (Circle r, (x,y)) (x',y') = (x-x')^^2 + (y-y')^^2 <= r^^2-}

{-between :: (X,Y) -> (X,Y) -> (X,Y) -> Bool-}
{-between (x1,y1) (x2,y2) (testX, testY) = x1 <= testX && testX <= x2 && y1 <= testY && testY <= y2-}

{-rectanglesOverlap :: (X,Y) -> (Width, Height) -> (X, Y) -> (Width, Height) -> Bool-}
{-rectanglesOverlap (x1,y1) (w1,h1) (x2,y2) (w2,h2) =-}
    {-any (between (x2,y2) (x2+w2, y2+h2)) [(x1,y1),(x1+w1,y1),(x1,y1+h1),(x1+w1,y1+h1)] ||-}
    {-any (between (x1,y1) (x1+w1, y1+h1)) [(x2,y2),(x2+w2,y2),(x2,y2+h2),(x2+w2,y2+h2)]-}

{--- I am bad at geometry.-}
{-circleAndRectangleOverlap :: (X, Y, Radius) -> (X, Y, Width, Height) -> Bool-}
{-circleAndRectangleOverlap (x1,y1,r) (x2,y2,w,h) =-}
    {-any (`surrounds` (x1,y1)) (map ((,)(Circle r)) [(x2,y2),(x2+w,y2),(x2,y2+h),(x2+w,y2+h)])-}
    {-||-}
    {-any (`surrounds` (x1,y1)) [(Rectangle (w+2*r) h, (x2-r,y2)), (Rectangle w (h+2*r), (x2, y2-r))]-}

{-overlap' :: (Location, Shape) -> (Location, Shape) -> Bool-}
{-overlap' (loc1, shape1) (loc2, shape2)-}
    {-| inSameSpace' loc1 loc2 = -}
        {-case (shape1, shape2) of-}
            {-(Circle r1, Circle r2) -> -}
                {-(loc1^._x - loc2^._x)^^2 + (loc1^._y - loc2^._y)^^2 <= (r1 + r2)^^2-}
            {-(Rectangle w1 h1, Rectangle w2 h2) ->-}
                {-rectanglesOverlap (loc1^._x,loc1^._y) (w1,h1) (loc2^._x,loc2^._y) (w2,h2)-}
            {-(Circle r, Rectangle w h) -> -}
                {-circleAndRectangleOverlap (loc1^._x, loc1^._y, r) (loc2^._x, loc2^._y, w, h)-}
            {-(Rectangle w h, Circle r) -> -}
                {-circleAndRectangleOverlap (loc2^._x, loc2^._y, r) (loc1^._x, loc1^._y, w, h)-}
    {-| otherwise = False-}

{-overlap :: Obj -> Obj -> Bool-}
{-overlap obj1 obj2 = overlap' (obj1^.space.location, obj1^.space.shape) (obj2^.space.location, obj2^.space.shape)-}

{-collide :: Obj -> Obj -> Bool-}
{-collide obj1 obj2 = overlap obj1 obj2 && (obj1^.space.blocking && obj2^.space.blocking)-}

{-collidesWithAny :: Obj -> World -> Bool-}
{-collidesWithAny obj world = I.null $ I.filter (collide obj) (world ^. objs)-}

{-destinationCoordinates :: Obj -> World -> Maybe (X,Y)-}
{-destinationCoordinates obj world = case obj^.task.goal of-}
    {-GoTo (ToMap (x,y))-> Just (x,y)-}
    {-GoTo (ToObj objId)-> fmap coordinates $ I.lookup objId (world^.objs)-}
    {-[>GoTo (ToTask taskId)-> coordinates <$> do<]-}
        {-[>task <- I.lookup taskId (world^.tasks)<]-}
        {-[>I.lookup (task^.owner) (world^.objs)<]-}
    {-WorkOn taskId -> coordinates <$> do-}
        {-task <- I.lookup taskId (world^.tasks)-}
        {-I.lookup (task^.owner) (world^.objs)-}
    {-NoGoal -> Nothing-}

{-nextStep :: Time -> Obj -> World -> Maybe (X,Y)-}
{-nextStep time obj world = case destinationCoordinates obj world of-}
    {-Nothing -> Nothing-}
    {-Just (x2,y2) -> let (x1,y1) = coordinates obj-}
                        {-hyp = (x2-x1)^^2 + (y2-y1)^^2-}
                        {-s = obj^.space.speed in-}
                    {-Just (time/1000 * s * (x2-x1)/hyp, speedConstant * time * s * (y2-y1)/hyp)-}

{-moveObj :: Time -> World -> Obj -> Obj-}
{-moveObj time world obj = case nextStep time obj world of-}
    {-Nothing -> obj -- blocking shouldn't cause obj to give up on movement -- space.destination .~ Nothing $ obj-}
    {-Just (x,y) -> if collidesWithAny (moveBy (x,y) obj) world then obj else moveBy (x,y) obj-}

{-moveInWorld :: Time -> ObjId -> World -> World-}
{-moveInWorld time objId world = world & objs %~ (I.adjust (moveObj time world) (objId))-}

{-tickLocations :: Time -> World -> World-}
{--- tickLocations time world = I.foldr (moveInWorld time) world (world^.objs)-}
{-tickLocations time world = foldr (moveInWorld time) world (I.keys $ world^.objs)-}

{--- TaskSystem-}

{-workConstant :: Double-}
{-workConstant = 1-}

{-setGoal :: ObjId -> Goal -> World -> World-}
{-setGoal objId goal' = (objs.at objId.traversed.task.goal .~ goal')-}

{-isWorkingOnTask :: Obj -> TaskId -> Bool-}
{-isWorkingOnTask obj taskId = case (obj^.task.goal, obj^.task.work) of-}
    {-(WorkOn taskId', Just work) -> taskId' == work^.task-}
    {-_ -> False-}

{-getSkill :: SkillType -> Obj -> Skill-}
{-getSkill skillType obj = fromMaybe (Skill skillType 0  0) (M.lookup skillType (view (task.skills) obj))-}

{--- missing location check-}
{-canWorkOn :: Obj -> Task -> Bool-}
{-canWorkOn obj task = task^.enabled && getSkill (task^.skill) obj ^. level >= task ^. difficulty -}

{-workIsComplete :: Work -> Bool-}
{-workIsComplete work = view complete work >= 100-}

{-applyWork :: Work -> World -> World-}
{-applyWork work world = world & tasks.at (work^.task).traverse %~ \tsk -> tsk & workCompleted +~ (work^.level - tsk^.difficulty)-}
    {-[>| workIsComplete work = world & tasks.at (work^.task).traverse %~ \tsk -> tsk & workCompleted +~ 1 --(work^.level - tsk^.difficulty)<]-}
    {-[>| otherwise           = world<]-}

{-tickWork :: Time -> ObjId -> World -> World-}
{-tickWork time objId world = case mWork of-}
    {-Nothing -> world & objs.at objId.traversed.task.work .~ Nothing-}
                     {-& objs.at objId.traversed.task.goal %~ \g -> case g of-}
                        {-WorkOn _ -> NoGoal-}
                        {-goal -> goal-}
    {-Just work' -> if workIsComplete work' -}
        {-then applyWork work' $-}
             {-world -- & objs.at objId.traversed.task.work .~ Nothing-}
                   {--- & objs.at objId.traversed.task.goal .~ NoGoal-}
        {-else world & objs.at objId.traversed.task.work .~ Just work'-}
  {-where-}
    {-mWork = do-}
        {-obj <- world^.objs.at objId-}
        {-objWork <- obj^.task.work-}
        {-objTask <- world^.tasks.at (objWork^.task)-}
        {-if canWorkOn obj objTask && isWorkingOnTask obj (objWork^.task)-}
            {-then Just (complete +~ (workConstant * time * fromIntegral -}
                        {-(view level (getSkill (view skill objTask) obj))) $ objWork)-}
            {-else Nothing-}

{-tickWorks :: Time -> World -> World-}
{-tickWorks time world = foldr (tickWork time) world (I.keys $ view objs world)-}

{-taskIsComplete :: Task -> Bool-}
{-taskIsComplete task = view workCompleted task >= view workRequired task-}

{-tickTask :: TaskId -> Task -> World -> World-}
{-tickTask taskId task world  -}
    {-| taskIsComplete task = foldr (runEvent taskId (task^.owner)) world (task^.outcome)-}
    {-| otherwise = world-}

{-tickTasks :: World -> World-}
{-tickTasks world = I.foldWithKey tickTask world (world^.tasks)-}

{-tickWorld :: Time -> World -> World-}
{-tickWorld time world = tickLocations time $ tickTasks $ tickWorks time world-}

{-addTask :: MonadState World m => Task -> m TaskId-}
{-addTask task = do-}
    {-taskId <- nextTask <%= succ-}
    {-tasks %= I.insert taskId task-}
    {-return taskId-}

{-addObj :: MonadState World m => Obj -> m ObjId-}
{-addObj obj = do-}
    {-objId <- nextObj <%= succ-}
    {-objs %= I.insert objId obj-}
    {-return objId-}

{-authenticateGoals :: Game -> [(Player, (ObjId, Goal))] -> [(ObjId, Goal)]-}
{-authenticateGoals game = map snd . filter (\(player, (objId, goal)) -> -}
    {-objId `elem` M.findWithDefault [] player (game^.players))-}

{--- rewrite with monads, cleaner separation-}

{-tick :: Time -> [(Player, (ObjId, Goal))] -> ReaderT Time (State Game) ()-}
{-tick time goals = do-}

    {-game <- get-}
    {-let goals' = authenticateGoals game goals-}
    {-zoom TaskPals.state $ do-}
        {-updateGoals goals'-}
        {-tickMovement-}
        {-tickWorkNew-}
        {-events <- tickTasksNew-}
        {-runEvents events-}

{-updateGoals :: (MonadReader Time m, MonadState World m) => [(ObjId, Goal)] -> m ()-}
{-updateGoals goals = do-}
    {-world <- get-}
    {-put $ foldr (uncurry setGoal) world goals-}

{-tickMovement :: (MonadReader Time m, MonadState World m) => m ()-}
{-tickMovement = do-}
    {-world <- get-}
    {-time <- ask-}
    {-put $ foldr (moveInWorld time) world (I.keys $ world^.objs)-}

{-tickWorkNew :: (MonadReader Time m, MonadState World m) => m ()-}
{-tickWorkNew = undefined-}

{-tickTasksNew :: (MonadReader Time m, MonadState World m) => m [(TaskId, ObjId, TaskEvent)]-}
{-tickTasksNew = do-}
    {-world <- get-}
    {-return $ I.foldWithKey tickTaskNew [] (world^.tasks)-}

{-tickTaskNew :: TaskId -> Task -> [(TaskId, ObjId, TaskEvent)] -> [(TaskId, ObjId, TaskEvent)]-}
{-tickTaskNew taskId task events-}
    {-| taskIsComplete task = events ++ map ((,,) taskId (task^.owner)) (task^.outcome)-}
    {-| otherwise = events-}

{-runEvents :: (MonadReader Time m, MonadState World m) => [(TaskId, ObjId, TaskEvent)] -> m ()-}
{-runEvents events = do-}
    {-world <- get-}
    {-put $ foldr (\(taskId, objId, event) -> runEvent taskId objId event) world events-}


{--- websocket server needs to: read input into (Player, ObjId, Goal); periodically write state in some form-}
{----}
