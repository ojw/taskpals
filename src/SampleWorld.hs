{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module SampleWorld where

import Control.Lens
import TaskPals
import Control.Monad.State as State
import qualified Data.Map as M
import qualified Data.Text as T

obj :: Obj
obj = Obj (TaskSystem [] M.empty Nothing NoGoal NeverAct) (SpatialSystem (OnMap (0,0)) (Circle 10) 10 Nothing True) (ObjIdentity "" [])

{-task :: Task-}
{-task = Task "" Generic Labor 1 1 0 0 [] 0-}

open :: Task
open = Task "Open" Open Labor 1 1 0 0 [RemoveThisTask, AddTask 0, SetBlocking False] 0

close :: Task
close = Task "Close" Close Labor 1 1 0 0 [RemoveThisTask, AddTask 0, SetBlocking True] 0

breakObj :: Task
breakObj = Task "Break" Break Labor 2 10 0 0 [RemoveThisObj] 0

hardBreak :: Task
hardBreak = breakObj & difficulty .~ 3 & workRequired .~ 20

door :: (MonadState World m) => m ObjId
door = do
    openId <- addTask open
    closeId <- addTask close
    breakId <- addTask breakObj
    modTask openId (outcome.traversed %~ (\oc -> case oc of
        AddTask _ -> AddTask closeId
        other -> other))
    modTask closeId (outcome.traversed %~ (\oc -> case oc of
        AddTask _ -> AddTask openId
        other -> other))
    addObj $ obj & task.tasks <>~ [openId, breakId]
                 & identity.name .~ "door"
                 & identity.tags .~ ["boring"]

pal :: T.Text -> Obj
pal name' = obj & task .~ TaskSystem [] (M.fromList [(Labor, Skill Labor 1 1)]) Nothing NoGoal NeverAct
               & identity.name .~ name'


-- mutually recursive tasks are a problem

world :: World
world = flip State.execState blankWorld $ do
    door
    addObj $ pal "James" & task.skills <>~ M.fromList [(Hacking, Skill Hacking 2 2)]
    return ()
