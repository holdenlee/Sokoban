import Array exposing (Array, fromList, toList, get, set)
import Signal as S
import List exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Keyboard exposing (..)
import Debug
import Maybe exposing (Maybe, andThen, withDefault)

type Base = Goal | Empty
type Object = Player | Block | Wall | None
type alias Model = { baseLayer : Array (Array Base),
                     objLayer : Array (Array Object),
                     playerLoc : (Int, Int),
                     inGoal : Int,
                     face : (Int, Int), 
                     w : Int,
                     h : Int}

startGame : Model 
startGame = {baseLayer = repeat2 4 4 Empty |> set2 (2,1) Goal,
             objLayer = repeat2 4 4 None |>
                        (\x -> foldl (\pos arr -> set2 pos Wall arr) x [(0,0), (0,3), (3,0), (3,3)]) |>
                        set2 (1,1) Player |>
                        set2 (1,2) Block,
             playerLoc = (1,1),
             inGoal = 0,
             face = (1,0),
             w = 4,
             h = 4}

imagePath = "https://dl.dropboxusercontent.com/u/27883775/wiki/apps/elm/iceblox.gif"

renderBase : Base -> Element
renderBase b = case b of
  Goal -> opacity 0.5 <| croppedImage (210,30) 30 30 imagePath
  Empty -> collage 30 30 []

renderObj : Object -> Element
renderObj o = case o of
  Player -> croppedImage (60,0) 30 30 imagePath
  Block -> croppedImage (0,60) 30 30 imagePath
  Wall -> croppedImage (180, 30) 30 30 imagePath
  None -> collage 30 30 [filled black <| square 30]

(.+) : (Int, Int) -> (Int, Int) -> (Int, Int)
(.+) (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

-- | Array functions
get2 : (Int, Int) -> Array (Array a) -> Maybe a
get2 (x,y) arr = (get x arr) `andThen` (get y)

set2 : (Int, Int) -> a -> Array (Array a) -> Array (Array a)
set2 (x,y) z arr = 
  case get x arr of
    Nothing -> arr
    Just orig -> set x (set y z orig) arr

toList2 : Array (Array a) -> List (List a)
toList2 = toList << Array.map toList 

fromList2 : List (List a) -> Array (Array a)
fromList2 = fromList << map fromList

repeat2 : Int -> Int -> a -> Array (Array a)
repeat2 x y z = Array.repeat y (Array.repeat x z)

--if out of bounds, then treat it as a wall
getObj : (Int, Int) -> Array (Array Object) -> Object
getObj (x,y) li = withDefault Wall <| get2 (x,y) li

step : (Int, Int) -> Model -> Model
step (x,y) m = Debug.watch (toString m) <|
  let 
    curLoc = m.playerLoc
    loc1 = curLoc .+ (x,y)
    loc2 = loc1 .+ (x,y)
    s1 = getObj loc1 m.objLayer
    s2 = getObj loc2 m.objLayer
  in
   (case (s1 ,s2) of
      (Block, None) -> {m | objLayer = m.objLayer |> set2 curLoc None
                                                 |> set2 loc1 Player
                                                 |> set2 loc2 Block,
                          playerLoc = loc1,
                          inGoal = m.inGoal + 
                            case (get2 curLoc m.baseLayer, get2 loc1 m.baseLayer) of
                              (Just Goal, _) -> -1
                              (_, Just Goal) -> 1
                              (_, _) -> 0}
      (None, _) -> {m | objLayer = m.objLayer |> set2 curLoc None
                                             |> set2 loc1 Player,
                      playerLoc = loc1}
      (_, _) -> m) |> (\m1 -> {m1 | face = (x,y)})

render : Model -> Element
render m = layers 
           [flow right <| map (flow up) <| map (map renderObj) <| toList2 m.objLayer,
            flow right <| map (flow up) <| map (map renderBase) <| toList2 m.baseLayer
           ]

whenPress : Signal Bool -> Signal Bool
whenPress k = S.filter identity False <| S.dropRepeats k

arrowSignal : Signal (Int, Int)
arrowSignal = S.mergeMany [S.map (always (-1,0)) <| whenPress <| isDown 37,
                           S.map (always (1,0)) <| whenPress <| isDown 39,
                           S.map (always (0,1)) <| whenPress <| isDown 38,
                           S.map (always (0,-1)) <| whenPress <| isDown 40]

main : Signal Element
main = S.map render <| S.foldp step startGame arrowSignal