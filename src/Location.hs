module Location where

import Data.IntMap (IntMap)
import qualified Data.IntMap as I
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Foldable as Foldable
import Data.Maybe

type X = Int
type Y = Int
type Position = (X,Y)

type ObjId = Int

data Shape = Rectangle Int Int | Circle Int

newtype Space = Space (IntMap (IntMap (Set (ObjId, Blocking))))

data Blocking = Ghost | Low | Medium | High deriving (Enum, Ord, Eq, Show)

blocks :: Blocking -> Blocking -> Bool
blocks b1 b2 = fromEnum b1 + fromEnum b2 >= fromEnum High

footprint :: Shape -> Position -> Blocking -> Set (Position, Blocking)
footprint (Rectangle w h) (x0,y0) blocking = S.fromList 
    [((x+x0,y+y0), blocking) | x <- [-w..w-1], y <- [-h..h-1]]
footprint (Circle r) (x0,y0) blocking = S.fromList 
    [((x+x0,y+y0), blocking) | x <- [-r..r-1], y <- [-r..r-1], (fromIntegral x+0.5-fromIntegral x0)^2 + (fromIntegral y+0.5-fromIntegral y0)^2 < fromIntegral r^2]

adjustPosition :: ((Set (ObjId, Blocking)) -> (Set (ObjId, Blocking))) -> Position -> Space -> Maybe Space
adjustPosition f (x,y) (Space xMap) = do
    yMap <- I.lookup x xMap
    pos <- I.lookup y yMap
    let yMap' = I.adjust f y yMap
        xMap' = I.adjust (const yMap') x xMap
    return $ Space xMap'

testPosition :: ((Set (ObjId, Blocking)) -> Bool) -> Position -> Space -> Maybe Bool
testPosition pred (x,y) (Space xMap) = do
    yMap <- I.lookup x xMap
    pos <- I.lookup y yMap
    return $ pred pos

addPositionToSpace :: ObjId -> Position -> Blocking -> Space -> Maybe Space
addPositionToSpace objId position blocking space  
    | canOccupy space position blocking = adjustPosition (S.insert (objId, blocking)) position space
    | otherwise = Nothing

addShapeToSpace :: ObjId -> Shape -> Position -> Blocking -> Space -> Maybe Space
addShapeToSpace objId shape position blocking space = S.foldr f (Just space) positions
  where
    f :: (Position, Blocking) -> Maybe Space -> Maybe Space
    f (pos, block) = (>>= addPositionToSpace objId position blocking)
    positions :: Set (Position, Blocking)
    positions = footprint shape position blocking

-- only removes a position, not a shape
removePositionFromSpace :: ObjId -> Position -> Blocking -> Space -> Maybe Space
removePositionFromSpace objId position blocking space = adjustPosition (S.filter ((==objId) . fst)) position space

removeShapeFromSpace :: ObjId -> Shape -> Position -> Blocking -> Space -> Maybe Space
removeShapeFromSpace objId shape position blocking space = S.foldr f (Just space) positions
  where
    f :: (Position, Blocking) -> Maybe Space -> Maybe Space
    f (pos, block) = (>>= removePositionFromSpace objId position blocking)
    positions :: Set (Position, Blocking)
    positions = footprint shape position blocking
    

move :: X -> Y -> Position -> Position
move dx dy (x,y) = (x+dx, y+dy)

-- Can a (_, blocking) value be added to position in space?
canOccupy :: Space -> Position -> Blocking -> Bool
canOccupy space position blocking = fromMaybe False $ testPosition pred position space
  where
    pred = Foldable.all $ not . blocks blocking . snd

-- Fails with Nothing if the move is illegal
moveInSpace :: X -> Y -> ObjId -> Shape -> Position -> Blocking -> Space -> Maybe Space
moveInSpace dx dy objId shape position blocking space = do
    space' <- removeShapeFromSpace objId shape position blocking space
    let position' = move dx dy position
    addShapeToSpace objId shape position' blocking space' 
