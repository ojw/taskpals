{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Json where

import TaskPals
import Data.Aeson
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Map as M
import Control.Applicative
import Control.Monad ( mzero )
        
instance FromJSON Command where
    parseJSON (Object v) = Command <$> v .: "player" <*> v .: "obj" <*> v .: "goal"
    parseJSON _ = mzero

instance FromJSON Destination where
    parseJSON (Object v) = do
        inType <- v .: "destination"
        case inType of
            ("map" :: Text) -> ToMap <$> ( (,) <$> v .: "x" <*> v .: "y" )
            "obj" -> ToObj <$> v .: "objid"
    parseJSON _ = mzero

instance FromJSON Location where
    parseJSON (Object v) = do
        inType <- v .: "in"
        case inType of
            ("map" :: Text) -> OnMap <$> ( (,) <$> v .: "x" <*> v .: "y" )
            "obj" -> InObj <$> v .: "objid" <*> ( (,) <$> v .: "x" <*> v .: "y" )
    parseJSON _ = mzero

instance FromJSON Goal where
    parseJSON (Object v) = do
        goalType <- v .:? "goaltype" .!= "nogoal"
        case goalType of
            ("nogoal" :: Text) -> return $ NoGoal
            "destination" -> GoTo <$> v .: "destination"
    parseJSON _ = mzero

instance ToJSON World where
    toJSON (World objs tasks _ _) = object ["objs" .= objs, "tasks" .= tasks]

instance ToJSON Task where
    toJSON (Task name workType skill difficulty required completed owner _ visibility) = object
        [ "name" .= name
        , "type" .= workType
        , "skill" .= skill
        , "difficulty" .= difficulty
        , "required" .= required
        , "completed" .= completed
        , "owner" .= owner
        , "visibility" .= visibility
        ]

instance ToJSON WorkType where
    toJSON = toJSON . T.pack . show

instance ToJSON Obj where
    toJSON (Obj objId task space tags) = object
        [ "id" .= objId
        , "tasks" .= task
        , "space" .= space
        , "tags" .= tags
        ]

instance ToJSON SpatialSystem where
    toJSON (SpatialSystem location shape speed destination blocking) = object
        ["location" .= location
        , "shape" .= shape
        , "speed" .= speed
        , "destination" .= destination
        , "blocking" .= blocking]

instance ToJSON Location where
    toJSON (OnMap (x,y)) = object ["in" .= ("map" :: Text), "x" .= x, "y" .= y]
    toJSON (InObj objId (x,y)) = object ["in" .= ("obj" :: Text), "objid" .= objId, "x" .= x, "y" .= y]

instance ToJSON Shape where
    toJSON (Circle r) = object ["shape" .= ("circle" :: Text), "radius" .= r]
    toJSON (Rectangle w h) = object ["shape" .= ("rectangle" :: Text), "width" .= w, "height" .= h]

instance ToJSON Destination where
    toJSON (ToMap (x,y)) = object ["destination" .= ("map" :: Text), "x" .= x, "y" .= y]
    toJSON (ToObj objId) = object ["destination" .= ("obj" :: Text), "objid" .= objId]

instance ToJSON Goal where
    toJSON NoGoal = object ["goaltype" .= ("nogoal" :: Text)]
    toJSON (GoTo destination) = object ["goaltype" .= ("destination" :: Text), "destination" .= destination]
    toJSON (WorkOn taskId) = object ["goaltype" .= ("task" :: Text), "taskid" .= taskId]

instance ToJSON TaskSystem where
    toJSON (TaskSystem tasks skills work goal pref) = object
        ["tasks" .= tasks, "skills" .= M.mapKeys show skills, "work" .= work, "goal" .= goal]

instance ToJSON Work where
    toJSON (Work task complete target level) = object ["task" .= task, "complete" .= complete, "target" .= target, "level" .= level]

instance ToJSON Skill where
    toJSON (Skill skillType level speed) = object ["type" .= skillType, "level" .= level, "speed" .= speed]

instance ToJSON SkillType where
    toJSON skillType = toJSON $ T.pack $ show skillType

instance ToJSON Target where
    toJSON None = object ["type " .= ("none" :: Text)]
    toJSON Self = object ["type" .= ("self" :: Text)]
    toJSON (AtObj objId) = object ["type" .= ("obj" :: Text), "objid" .= objId]
    toJSON (WithinRadius radius) = object ["type" .= ("radius" :: Text), "radius" .= radius]
    toJSON (InCircle location radius) = object ["type" .= ("circle" :: Text), "location" .= location, "radius" .= radius]
    toJSON (InRectangle location (w,h)) = object ["type" .= ("rectangle" :: Text), "location" .= location, "width" .= w, "height" .= h]
