{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances #-}

module TaskPals where

import Control.Lens
import Data.IntMap
import Data.Text

data World = IntMap Obj

type ObjId = Int
type TaskId = Int

data Location = OnMap (Int, Int) | InObj ObjId
data Work = Work
    { _workTask :: TaskId
    , _workComplete :: Int
    }

data Obj = Obj
    { _objObjId :: ObjId
    , _objLocation :: Location
    , _objTasks :: [Task]
    , _objWork :: Work
    , _objSkills :: [Skill]
    }

-- items function as available skills

data SkillType = Combat | Medic | Mechanic | Chemist | Hacker

data TaskType = Open | Break | Unlock | Hack | Fix | Barricade

data Skill = Skill
    { _skillType :: SkillType
    , _skillLevel :: Int
    , _skillRange :: Int
    , _skillSpeed :: Int
    }

data Task = Task
    { _taskName :: Text
    , _taskType :: TaskType
    , _taskSkill :: SkillType
    , _taskLevel :: Int
    , _taskWorkRequired :: Int
    , _taskWorkCompleted :: Int
    , _taskObject :: ObjId
    , _taskOutcome :: World -> World -- ? or Task -> World -> World or something else
    } 

makeFields ''Obj
makeFields ''Skill
makeFields ''Task

{-logWork :: Pal -> Skill -> Task -> Task-}
{-logWork pal skill task = task & completed -~ (skill ^. level)-}

