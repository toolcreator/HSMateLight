module Simple (
   Event(..)
  ,stringEvent
  ,runMate
  ,parseAddress
  ,Config(..)
  ,Frame(..)
  ) where

import MateLight
import Data.Typeable
import Control.Monad.State

data Event a = Event String a deriving (Eq, Ord, Show, Read)

runMate :: (Frame f) => Config -> ([Event String] -> s -> (f, s)) -> s -> IO ()
runMate conf fkt = runMateM conf $ state . fkt . map stringEvent

-- misc
castEvent :: Typeable a => EventT -> Maybe (Event a)
castEvent (EventT mod a) = Event mod `fmap` cast a
stringEvent :: EventT -> Event String
stringEvent (EventT mod a) = Event mod $ show a
