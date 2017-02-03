
module LifeGame where

import LifeGame.Types

import qualified Data.Array as A
import qualified Data.Ix as Ix
import Data.Maybe (fromMaybe,isJust,fromJust,catMaybes)
import Data.List (sortBy,group,sort)

import Control.Lens
import Control.Arrow ((&&&))

import Linear.V2 (V2(..))
import Linear.Vector ((^+^),(^-^),(^/))

import qualified Graphics.Gloss as G
import qualified Graphics.Gloss.Interface.IO.Game as G

import qualified System.Random as R

(!?) :: (Ix.Ix i) => A.Array i a -> i -> Maybe a
arr !? i
  | Ix.inRange (A.bounds arr) i = pure $ arr A.! i
  | otherwise = Nothing

(//?) :: (Ix.Ix i) => A.Array i a -> [(i,a)] -> A.Array i a
arr //? xs = arr A.// (filter (\x->A.inRange (A.bounds arr) $ fst x) xs)

mostOf :: Ord a => [a] -> a
mostOf = snd . head . sortBy (flip compare) . map (length &&& head) . group . sort

getNeighbors :: Field -> V2 Int -> [Cell]
getNeighbors fld (V2 x y) = catMaybes [fromMaybe Nothing (fld !? V2 x' y') | x'<-[x-1,x,x+1], y'<-[y-1,y,y+1], not(x'==x&&y'==y)]

getNext :: Game -> V2 Int -> Maybe Cell
getNext game pos
    | length neighbors `elem` rule = Just (mostOf neighbors)
    | otherwise = Nothing
  where
    neighbors = getNeighbors (game^.cells) pos
    rule = case (game^.cells) A.! pos of
      Just _ -> game^.surviveRule
      Nothing -> game^.bornRule

{-
getNext' :: Game -> V2 Int -> Bool
getNext' game pos = getNeighborNum (game^.cells) pos `elem` rule
  where
    rule = case (game^.cells) A.! pos of
      True -> game^.surviveRule
      False -> game^.bornRule
-}

update :: Float -> Game -> IO Game
update _ game = return $ game { _cells=nextcells }
  where
    nextcells = A.listArray (V2 0 0,game^.fieldSize ^-^ V2 1 1) $ map (getNext game) $ A.indices (game^.cells)

render :: Game -> IO G.Picture
render game = return $ G.Translate (negate $ fromIntegral $ div screenx 2) (fromIntegral $ div screeny 2) $ G.Pictures $ map toPic cs
  where
    (V2 sx sy) = fmap fromIntegral $ game^.cellSize
    cs = map ((_1%~fmap fromIntegral).(_2%~fromJust)) $ filter (isJust.snd) $ A.assocs (game^.cells)
    (screenx, screeny) = game^.screenSize
    rect = G.Polygon [(0,0),(sx-1,0),(sx-1,negate sy+1),(0,negate sy+1)]
    toPic :: (V2 Int,Cell) -> G.Picture
    toPic (V2 y x, cell) = G.Translate (fromIntegral x*sx) (negate $ fromIntegral y*sy) $ G.Color (cellColor cell) rect

    cellColor RedCell = G.red
    cellColor BlueCell = G.blue

event :: G.Event -> Game -> IO Game
event (G.EventKey (G.Char 'W') G.Down _ _) game = do
  fld <- createRandomCells (game^.fieldSize)
  return $ game&cells.~fld
event (G.EventKey (G.MouseButton G.LeftButton) G.Down _ (mx,my)) game = return $ game&cells%~(//? mkObj lightSpaceship RedCell clickedPos)
  where
    (V2 hsx hsy) = (`div`2) <$> (t2v $ game^.screenSize)
    (V2 cx cy) = game^.cellSize
    clickedPos = V2 (div (hsy-(floor my)) cy) (div (floor mx+hsx) cx)
event (G.EventResize size) game = return $ game&screenSize.~size
event _ g = return g

defaultGame = Game {
    _cells = A.listArray (V2 0 0,fldSz ^-^ V2 1 1) $ cycle [Nothing]
  , _cellSize = V2 5 5
  , _fieldSize = fldSz
  , _screenSize = (600,600)
  , _surviveRule = [2,3]
  , _bornRule = [3]
  }
  where
    fldSz = V2 120 120

t2v :: (a,a) -> V2 a
t2v = uncurry V2
v2t :: V2 a -> (a,a)
v2t (V2 x y) = (x,y)

mkObj :: [V2 Int] -> Cell -> V2 Int -> [(V2 Int,Maybe Cell)]
mkObj base cell pos = zip (map (^+^pos) base) $ cycle [Just cell]

dot = [V2 0 0]
block = [V2 0 0, V2 0 1, V2 1 0, V2 1 1]
glider = [V2 0 1, V2 1 2, V2 2 0, V2 2 1, V2 2 2]
lightSpaceship = [V2 0 1, V2 0 3, V2 1 4, V2 2 0, V2 2 4, V2 3 4, V2 4 1, V2 4 4, V2 5 2, V2 5 3, V2 5 4]

createRandomCells :: V2 Int -> IO Field
createRandomCells size = do
  xs <- (R.newStdGen >>= return . fmap (<0.3) . R.randomRs (0,1::Float))
  return $ A.listArray (V2 0 0,size ^-^ V2 1 1) $ map toCell xs
  where
    toCell False = Nothing
    toCell True = Just BlueCell

playGame :: IO ()
playGame = do
  field <- createRandomCells (defaultGame^.fieldSize)
  let game = defaultGame{ _cells=field, _screenSize=(600,600) }
  G.playIO
    (G.InWindow "lifegame" (game^.screenSize) (80,80))
    G.white
    5
    game
    render
    event
    update
