module UI (
  drawingTime, eWithPrev, keyState
  ) where

import FRP.Peakachu
import FRP.Peakachu.Backend.GLUT
import FRP.Peakachu.Backend.Time (zipTime)
import Data.Monoid
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime)
import Graphics.UI.GLUT

keyState :: Key -> UI -> Event KeyState
keyState key =
  runEventMerge .
  mappend (EventMerge (ereturn Up)) .
  EventMerge .
  fmap m .
  efilter f .
  glutKeyboardMouseEvent
  where
    m (_, state, _, _) = state
    f (k, _, _, _) = k == key

eWithPrev :: Event a -> Event (a, a)
eWithPrev =
  fmap f . edrop (2::Int) . escanl step []
  where
    step xs x = x : take 1 xs
    f l = (l !! 1, head l)

-- when idle (on IdleEvent), draw
-- otherwise, process the remaining events
-- while drawing at a minimal FPS rate
drawingTime :: NominalDiffTime -> UI -> Event UTCTime
drawingTime framePerAtLeast =
  eMapMaybe f .
  escanl step Nothing .
  zipTime
  where
    step _ (now, IdleEvent) = Just (True, now)
    step Nothing (now, _) = Just (True, now)
    step (Just (_, prev)) (now, _)
      | diffUTCTime now prev >= framePerAtLeast = Just (True, now)
      | otherwise = Just (False, prev)
    f (Just (True, now)) = Just now
    f _ = Nothing

