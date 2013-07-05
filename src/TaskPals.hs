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

data SkillType = Combat | Medical | Mechanical | Chemical | Hacking

-- Heal / Fix undoes progress on Break?
-- Makes a broken thing easy to destroy (logical), makes damage undo repairs (logical)
data TaskType = Open | Break | Unlock | Hack | Fix | Heal | Barricade | Use

data Skill = Skill
    { _skillType :: SkillType
    , _skillLevel :: Int
    , _skillSpeed :: Int
    }

-- Is outcome needed or can it be computed from TaskType?
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
makeFields ''Work
makeFields ''Skill
makeFields ''Task 
