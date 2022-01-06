module Node.FS.Stats.Lens where

import Prelude

import Data.Function.Uncurried (Fn0)
import Data.JSDate (JSDate)
import Data.Lens (Lens')
import Data.Lens.Iso (Iso', iso)
import Node.FS.Stats (StatsObj, Stats(..))
import Prelude (identity)

_StatsObj :: Iso' StatsObj
  { dev :: Number
  , mode :: Number
  , nlink :: Number
  , uid :: Number
  , gid :: Number
  , rdev :: Number
  , ino :: Number
  , size :: Number
  , atime :: JSDate
  , mtime :: JSDate
  , ctime :: JSDate
  , isFile :: Fn0 Boolean
  , isDirectory :: Fn0 Boolean
  , isBlockDevice :: Fn0 Boolean
  , isCharacterDevice :: Fn0 Boolean
  , isFIFO :: Fn0 Boolean
  , isSocket :: Fn0 Boolean
  }
_StatsObj = identity

_Stats :: Lens' Stats StatsObj
_Stats = iso (\(Stats a) -> a) Stats
