{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances #-}

module TaskPals where

import Control.Lens
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

data SkillType = Combat | Medical | Mechanical | Chemical | Hacking
    deriving (Eq, Ord, Show)

data TaskType = Open | Break | Unlock | Hack | Fix | Heal | Barricade | Use

data World = World
    { _worldObjs :: IntMap Obj
    , _worldTasks :: IntMap Task
    }

data Obj = Obj
    { _objObjId :: ObjId
    , _objTasks :: [TaskId]
    , _objWork :: Maybe Work
    , _objSkills :: Map SkillType Skill
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

data Task = Task
    { _taskName :: Text
    , _taskTaskType :: TaskType
    , _taskSkill :: SkillType
    , _taskDifficulty :: Int
    , _taskWorkRequired :: Int
    , _taskWorkCompleted :: Int
    , _taskObject :: ObjId
    , _taskOutcome :: World -> World -- ? or Task -> World -> World or something else
    } 

makeFields ''Obj
makeFields ''Work
makeFields ''Skill
makeFields ''Task 
makeFields ''World

getSkill :: SkillType -> Obj -> Skill
getSkill skillType obj = fromMaybe (Skill skillType 0  0) (M.lookup skillType (view skills obj))

canWorkOn :: Obj -> Task -> Bool
canWorkOn obj task = getSkill (task^.skill) obj ^. level >= task ^. difficulty 

workIsComplete :: Work -> Bool
workIsComplete work = view complete work >= 100

applyWork :: Work -> World -> World
applyWork work world
    | workIsComplete work = over tasks (I.adjust (over workCompleted succ) (view task work)) world
    | otherwise           =  world

logWork :: Int -> Task -> Task
logWork int = workCompleted +~ int

tickWork' :: Time -> ObjId -> World -> Maybe Work
tickWork' time objId world = do
    obj <- I.lookup objId (view objs world)
    work <- view work obj
    task <- I.lookup (view task work) (view tasks world)
    if canWorkOn obj task 
        then Just (complete +~ (time * fromIntegral (view level (getSkill (view skill task) obj))) $ work) 
        else Nothing

tickWork'' :: Time -> ObjId -> World -> World
tickWork'' time objId world = case tickWork' time objId world of
    Nothing -> over objs (I.adjust (set work Nothing) objId) world
    Just work' -> if workIsComplete work' 
        then applyWork work' $
             over objs (I.adjust (set work Nothing) objId) world 
        else over objs (I.adjust (set work (Just work')) objId) world

tickWorks :: Time -> World -> World
tickWorks time world = foldr (tickWork'' time) world (I.keys $ view objs world)

taskIsComplete :: Task -> Bool
taskIsComplete task = view workCompleted task >= view workRequired task

tickTask :: Task -> World -> World
tickTask task world
    | taskIsComplete task = view outcome task world 
    | otherwise           =  world

tickTasks :: World -> World
tickTasks world = I.foldr tickTask world (view tasks world)

tickWorld :: Time -> World -> World
tickWorld time world = tickTasks $ tickWorks time world
