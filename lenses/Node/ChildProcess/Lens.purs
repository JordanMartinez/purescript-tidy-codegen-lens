module Node.ChildProcess.Lens where

import Prelude

import Data.Either (Either(..))
import Data.Lens.Iso (Iso')
import Data.Lens.Prism (Prism', prism)
import Data.Maybe (Maybe)
import Data.Posix (Gid, Uid)
import Data.Posix.Signal (Signal)
import Effect.Exception as Exception
import Foreign.Object (Object)
import Node.Buffer (Buffer)
import Node.ChildProcess (Error, ExecOptions, ExecResult, ExecSyncOptions, SpawnOptions, Exit(..), StdIOBehaviour(..))
import Node.Encoding (Encoding)
import Node.FS as FS
import Node.Stream (Stream)
import Prelude (Unit, const, identity, unit)

_Normally :: Prism' Exit Int
_Normally = prism Normally case _ of
  Normally a -> Right a
  other -> Left other

_BySignal :: Prism' Exit Signal
_BySignal = prism BySignal case _ of
  BySignal a -> Right a
  other -> Left other

_SpawnOptions :: Iso' SpawnOptions
  { cwd :: Maybe String
  , stdio :: Array (Maybe StdIOBehaviour)
  , env :: Maybe (Object String)
  , detached :: Boolean
  , uid :: Maybe Uid
  , gid :: Maybe Gid
  }
_SpawnOptions = identity

_ExecOptions :: Iso' ExecOptions
  { cwd :: Maybe String
  , env :: Maybe (Object String)
  , encoding :: Maybe Encoding
  , shell :: Maybe String
  , timeout :: Maybe Number
  , maxBuffer :: Maybe Int
  , killSignal :: Maybe Signal
  , uid :: Maybe Uid
  , gid :: Maybe Gid
  }
_ExecOptions = identity

_ExecResult :: Iso' ExecResult
  { stderr :: Buffer
  , stdout :: Buffer
  , error :: Maybe Exception.Error
  }
_ExecResult = identity

_ExecSyncOptions :: Iso' ExecSyncOptions
  { cwd :: Maybe String
  , input :: Maybe String
  , stdio :: Array (Maybe StdIOBehaviour)
  , env :: Maybe (Object String)
  , timeout :: Maybe Number
  , maxBuffer :: Maybe Int
  , killSignal :: Maybe Signal
  , uid :: Maybe Uid
  , gid :: Maybe Gid
  }
_ExecSyncOptions = identity

_Error :: Iso' Error
  { code :: String
  , errno :: String
  , syscall :: String
  }
_Error = identity

_Pipe :: Prism' StdIOBehaviour Unit
_Pipe = prism (const Pipe) case _ of
  Pipe -> Right unit
  other -> Left other

_Ignore :: Prism' StdIOBehaviour Unit
_Ignore = prism (const Ignore) case _ of
  Ignore -> Right unit
  other -> Left other

_ShareStream :: Prism' StdIOBehaviour (forall r. Stream r)
_ShareStream = prism ShareStream case _ of
  ShareStream a -> Right a
  other -> Left other

_ShareFD :: Prism' StdIOBehaviour FS.FileDescriptor
_ShareFD = prism ShareFD case _ of
  ShareFD a -> Right a
  other -> Left other
