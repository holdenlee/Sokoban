{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, ViewPatterns #-}
{-# OPTIONS

 -XTupleSections

#-}

import FRP.Helm
import FRP.Helm.Graphics
import FRP.Helm.Keyboard
import FRP.Helm.Signal as S
import FRP.Elerea.Simple
import qualified FRP.Helm.Window as Window
import Data.Array as A
import Data.Array (Array)
import Data.Maybe
import Algebra.Module
import NumericPrelude
import Control.Lens
import Data.Traversable (sequenceA)
import Data.List

--key helpers
combine :: [SignalGen (Signal a)] -> SignalGen (Signal [a])
combine = (fmap sequenceA) . sequenceA

keySignal :: [(Key, a)] -> SignalGen (Signal (Maybe a))
keySignal li = lift (\x -> 
                      let 
                          zp = filter fst $ zip x $ map snd li
                      in
                        if null zp then Nothing else Just $ snd (head zp)
                        ) $ combine $ map (whenPressed . fst) li

whenPressed :: Key -> SignalGen (Signal Bool)
whenPressed k = lift fst $ S.foldp (\x (_, y) -> (not y && x, x)) (False, False) (isDown k)

dirSignal :: SignalGen (Signal (Maybe (Int, Int)))
dirSignal = keySignal [(LeftKey, (-1,0)), (RightKey, (1,0)), (UpKey, (0,1)), (DownKey, (0,-1))]

data Base = Goal | Empty deriving (Show, Eq) 
data Object = Player | Block | Wall | None deriving (Show, Eq)
data Model = Model { _baseLayer :: Array (Int, Int) Base,
                     _objLayer :: Array (Int, Int) Object,
                     _playerLoc :: (Int, Int),
                     _inGoal :: Int,
                     _face :: (Int, Int), 
                     _w :: Int,
                     _h :: Int}

makeLenses ''Model

{-|
* means goal
o means block
@ means block at goal
L means player
E means player at goal
X means wall
  means empty
-}
readChar :: Char -> (Base, Object)
readChar c = case c of
               '*' -> (Goal, None)
               'o' -> (Empty, Block)
               '@' -> (Goal, Block)
               'L' -> (Empty, Player)
               'E' -> (Goal, Player)
               'X' -> (Empty, Wall)
               _   -> (Empty, None)

gameString = "X  X\n o  \n L* \nX  X"

readGame :: String -> Model
readGame str = 
    let
        l = lines str
        li = concat $ reverse l
        cli = map readChar li
        (baseL, objL) = unzip cli
        xw = length (l!!0)
        xh = length l
        r = [1..xh] >>= (\x -> map (,x) [1..xw]) 
        ploc = fromJust $ findIndex (==Player) objL
        goals = length $ findIndices (==Goal) baseL
        ingoals = length $ findIndices (==(Goal, Block)) cli
    in
      Model {_baseLayer = array ((1,1),(xw,xh)) $ zip r baseL,
             _objLayer = array ((1,1),(xw,xh)) $ zip r objL,
             _playerLoc = r!!ploc,
             _inGoal = ingoals,
             _face = (1,0),
             _w = xw,
             _h = xh}

startGame :: Model 
startGame = readGame gameString

imagePath = "iceblox.png"

{-| Rendering -}
renderBase :: Base -> Element
renderBase b = case b of
  Goal -> croppedImage (210,30) 30 30 imagePath
  Empty -> collage 30 30 []

renderObj :: Object -> Element
renderObj o = case o of
  Player -> croppedImage (60,0) 30 30 imagePath
  Block -> croppedImage (0,60) 30 30 imagePath
  Wall -> croppedImage (180, 30) 30 30 imagePath
  None -> collage 30 30 [move (0,0) $ filled black $ square 30]

{- Array and list helpers -}
groupN :: Int -> [a] -> [[a]]
groupN n li = 
    case li of 
      [] -> []
      _ -> (take n li):(groupN n $ drop n li)

toList2 :: Array (Int, Int) a -> [[a]]
toList2 arr = 
    let ((x1,y1),(x2,y2)) = bounds arr
    in groupN (x2-x1+1) $ A.elems arr

repeat2 :: Int -> Int -> a -> Array (Int, Int) a
repeat2 x y z = listArray ((1,1),(x,y)) $ repeat z

--safe version of !
aget :: (Ix i) => i -> Array i a -> Maybe a
aget ix arr = if inRange (A.bounds arr) ix
              then Just $ arr!ix
              else Nothing
                
--if out of bounds, then treat it as a wall
getObj :: (Int, Int) -> Array (Int, Int) Object -> Object
getObj (x,y) li = fromMaybe Wall $ aget (x,y) li

step :: Maybe (Int, Int) -> Model -> Model
step mp m =
 case mp of
   Nothing -> m
   Just (x, y) -> 
       let 
           curLoc = m ^. playerLoc
           loc1 = curLoc + (x,y)
           loc2 = loc1 + (x,y)
           s1 = getObj loc1 (_objLayer m)
           s2 = getObj loc2 (_objLayer m)
       in
         (case (s1 ,s2) of
            (Block, None) -> m & objLayer %~ (// [(curLoc, None), (loc1, Player), (loc2, Block)])
                               & playerLoc .~ loc1
                               & inGoal %~ (+ 
                                            (case (aget curLoc (_baseLayer m), aget loc1 (_baseLayer m)) of
                                               (Just Goal, _) -> -1
                                               (_, Just Goal) -> 1
                                               (_, _) -> 0))
            (None, _) -> m & objLayer %~ (// [(curLoc, None), (loc1, Player)])
                           & playerLoc .~ loc1
            (_, _) -> m) & face .~ (x, y)

putAt :: Int -> Int -> Int -> Int -> Element -> Form
putAt _ mh (fromIntegral -> x) (fromIntegral -> y) = move (30*(x-1),30*((fromIntegral mh)-y)) . toForm
-- -(mw/2)
-- -(mh/2)
render :: Model -> Element
render m = let 
    mw = _w m
    mh = _h m
 in
  collage (mw * 30) (mh * 30)
                   ((map (\((x,y),obj) -> putAt mw mh x y (renderObj obj)) $ assocs (_objLayer m))++
                    (map (\((x,y),base) -> putAt mw mh x y (renderBase base)) $ assocs (_baseLayer m)))

main :: IO ()
main = run $ lift render $ S.foldp step startGame dirSignal
