{-# LANGUAGE NoImplicitPrelude, TemplateHaskell, ViewPatterns #-}

{-# OPTIONS

 -XTupleSections

#-}

import FRP.Helm
import FRP.Helm.Graphics
import FRP.Helm.Keyboard
import FRP.Helm.Signal as S
import FRP.Helm.Text
import FRP.Elerea.Simple
import qualified FRP.Helm.Window as Window
import Data.Array as A
import Data.Array (Array)
import Data.Maybe
import Algebra.Module
import NumericPrelude
import Control.Lens
import Data.Traversable (sequenceA)

combine :: [SignalGen (Signal a)] -> SignalGen (Signal [a])
combine = (fmap sequenceA) . sequenceA

getKey :: [(Key, a)] -> SignalGen (Signal (Maybe a))
getKey li = lift (\x -> 
                      let 
                          zp = filter fst $ zip x $ map snd li
                      in
                        if null zp then Nothing else Just $ snd (head zp)
                        ) $ combine $ map (whenPressed . fst) li

whenPressed :: Key -> SignalGen (Signal Bool)
whenPressed k = lift fst $ S.foldp (\x (_, y) -> (not y && x, x)) (False, False) (isDown k)
--go from off to on

s :: SignalGen (Signal Int)
s = S.countIf id (whenPressed UpKey)

--(Show a) => a
render :: Int -> Element
render x = collage 1000 1000 [filled white $ square (100*(fromIntegral x))]
--[move (0,0) $ toForm $ plainText $ show x, filled white $ square 1000]

main :: IO ()
main = run $ fmap (fmap render) s
