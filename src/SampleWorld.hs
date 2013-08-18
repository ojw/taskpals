{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module SampleWorld where

import Control.Lens
import Control.Monad.State as State
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.IntMap as I

import TaskPals

addComponent :: ObjId -> Component -> World -> World
addComponent objId component = case component of
    CompWork w -> work %~ I.insert objId w
    CompTasks t -> tasks %~ I.insert objId t
    CompPhysics p -> physics %~ I.insert objId p
    CompMeta m -> meta %~ I.insert objId m

addObj :: Obj -> World -> World
addObj obj world = let objId = world^.nextObj in
    nextObj %~ succ $ foldr (addComponent $ objId) world (obj objId)

addObjs :: [Obj] -> World -> World
addObjs objs world = foldr addObj world objs

door :: Obj
door objId = 
    [ CompPhysics $ PhysicsComponent (OnMap (0,40)) (Rectangle 10 3) 0 True
    , CompTasks $ I.fromList [ (1, Task "Open" Open Labor 0 2 0 [EnableTask (objId, 2), ResetThisTask, DisableThisTask, SetBlocking False] True)
                             , (2, Task "Close" Close Labor 0 2 0 [EnableTask (objId, 1), DisableThisTask, SetBlocking True] False)
                             , (3, Task "Break" Break Labor 2 5 0 [] True)
                             ]
    , CompMeta $ MetaComponent "Door" [] Nothing
    ]

james :: Obj
james objId =
    [ CompPhysics $ PhysicsComponent (OnMap (0,0)) (Circle 3) 3 True
    , CompTasks $ I.fromList [ (1, Task "Kill" Break Combat 0 10 0 [RemoveThisObj] True)
                             , (2, Task "Heal" Fix Medical 0 1 0 [ResetThisTask, RemoveWorkFrom (objId, 1) 1] True)
                             ]
    -- , CompWork $ WorkComponent NeverAct (Just $ WorkOn (2,1)) Nothing [(Skill Labor 1 1)]
    , CompWork $ WorkComponent NeverAct (Just $ GoTo (ToMap (2,2))) Nothing [(Skill Labor 1 1)]
    , CompMeta $ MetaComponent "James" ["pal"] (Just "James")
    ]

blankWorld :: World
blankWorld = World 1 I.empty I.empty I.empty I.empty

world :: World
world = addObjs [door, james] blankWorld
