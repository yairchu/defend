module UI where

import FRP.Peakachu
import FRP.Peakachu.Backend.GLUT
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

eWithPrev :: Event a -> Event (a, a)
eWithPrev =
  fmap f . edrop (1::Int) . escanl step []
  where
    step xs x = x : take 1 xs
    f l = (head l, l !! 1)

