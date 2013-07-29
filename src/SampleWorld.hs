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
open = Task "Open" Open Labor 1 5 0 0 [ResetThisTask, DisableThisTask, ActivateTask 0, SetBlocking False] True 0

close :: Task
close = Task "Close" Close Labor 1 5 0 0 [ResetThisTask, DisableThisTask, ActivateTask 0, SetBlocking True] False 0

breakObj :: Task
breakObj = Task "Break" Break Labor 2 10 0 0 [RemoveThisObj] True 0

hardBreak :: Task
hardBreak = breakObj & difficulty .~ 3 & workRequired .~ 20

door :: (MonadState World m) => m (TaskId, TaskId, TaskId, ObjId)
door = do
    openId <- addTask open
    closeId <- addTask close
    breakId <- addTask breakObj
    modTask openId (outcome.traversed %~ (\oc -> case oc of
        ActivateTask _ -> ActivateTask closeId
        other -> other))
    modTask closeId (outcome.traversed %~ (\oc -> case oc of
        ActivateTask _ -> ActivateTask openId
        other -> other))
    doorId <- addObj $ obj & task.tasks <>~ [openId, closeId, breakId]
                 & identity.name .~ "door"
                 & identity.tags .~ ["boring"]
    tasks.at openId.traversed.owner .= doorId
    tasks.at closeId.traversed.owner .= doorId
    tasks.at breakId.traversed.owner .= doorId
    return (openId, closeId, breakId, doorId)

pal :: T.Text -> Obj
pal name' = obj & task .~ TaskSystem [] (M.fromList [(Labor, Skill Labor 3 2)]) Nothing NoGoal NeverAct
                & identity.name .~ name'


-- mutually recursive tasks are a problem

world :: World
world = flip State.execState blankWorld $ do
    (openId, closeId, breakId, doorId) <- door
    jamesId <- addObj $ pal "James" 
        & task.skills <>~ M.fromList [(Hacking, Skill Hacking 2 2)]
        & task.goal .~ WorkOn 2
    objs.at jamesId.traversed.task.work .= startWorkOn openId jamesId world
    return ()
