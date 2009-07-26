module UI where

import FRP.Peakachu
import FRP.Peakachu.Backend.GLUT
import Data.List (genericTake)
import Data.Monoid
import Graphics.UI.GLUT

keyState :: Key -> UI -> Event KeyState
keyState key ui =
  mappend (ereturn Up) .
  fmap m $
  efilter f (glutKeyboardMouseEvent ui)
  where
    m (_, state, _, _) = state
    f (k, _, _, _) = k == key

delayEvent :: Integral i => i -> Event a -> Event a
delayEvent count =
  fmap last .
  edrop count .
  escanl step []
  where
    step xs x = x : genericTake count xs


