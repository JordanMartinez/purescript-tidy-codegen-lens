module Data.Posix.Signal.Lens where

import Data.Either (Either(..))
import Data.Lens.Prism (Prism', prism)
import Data.Posix.Signal (Signal(..))
import Prelude (Unit, const, unit)

_SIGABRT :: Prism' Signal Unit
_SIGABRT = prism (const SIGABRT) case _ of
  SIGABRT -> Right unit
  other -> Left other

_SIGALRM :: Prism' Signal Unit
_SIGALRM = prism (const SIGALRM) case _ of
  SIGALRM -> Right unit
  other -> Left other

_SIGBUS :: Prism' Signal Unit
_SIGBUS = prism (const SIGBUS) case _ of
  SIGBUS -> Right unit
  other -> Left other

_SIGCHLD :: Prism' Signal Unit
_SIGCHLD = prism (const SIGCHLD) case _ of
  SIGCHLD -> Right unit
  other -> Left other

_SIGCLD :: Prism' Signal Unit
_SIGCLD = prism (const SIGCLD) case _ of
  SIGCLD -> Right unit
  other -> Left other

_SIGCONT :: Prism' Signal Unit
_SIGCONT = prism (const SIGCONT) case _ of
  SIGCONT -> Right unit
  other -> Left other

_SIGEMT :: Prism' Signal Unit
_SIGEMT = prism (const SIGEMT) case _ of
  SIGEMT -> Right unit
  other -> Left other

_SIGFPE :: Prism' Signal Unit
_SIGFPE = prism (const SIGFPE) case _ of
  SIGFPE -> Right unit
  other -> Left other

_SIGHUP :: Prism' Signal Unit
_SIGHUP = prism (const SIGHUP) case _ of
  SIGHUP -> Right unit
  other -> Left other

_SIGILL :: Prism' Signal Unit
_SIGILL = prism (const SIGILL) case _ of
  SIGILL -> Right unit
  other -> Left other

_SIGINFO :: Prism' Signal Unit
_SIGINFO = prism (const SIGINFO) case _ of
  SIGINFO -> Right unit
  other -> Left other

_SIGINT :: Prism' Signal Unit
_SIGINT = prism (const SIGINT) case _ of
  SIGINT -> Right unit
  other -> Left other

_SIGIO :: Prism' Signal Unit
_SIGIO = prism (const SIGIO) case _ of
  SIGIO -> Right unit
  other -> Left other

_SIGIOT :: Prism' Signal Unit
_SIGIOT = prism (const SIGIOT) case _ of
  SIGIOT -> Right unit
  other -> Left other

_SIGKILL :: Prism' Signal Unit
_SIGKILL = prism (const SIGKILL) case _ of
  SIGKILL -> Right unit
  other -> Left other

_SIGLOST :: Prism' Signal Unit
_SIGLOST = prism (const SIGLOST) case _ of
  SIGLOST -> Right unit
  other -> Left other

_SIGPIPE :: Prism' Signal Unit
_SIGPIPE = prism (const SIGPIPE) case _ of
  SIGPIPE -> Right unit
  other -> Left other

_SIGPOLL :: Prism' Signal Unit
_SIGPOLL = prism (const SIGPOLL) case _ of
  SIGPOLL -> Right unit
  other -> Left other

_SIGPROF :: Prism' Signal Unit
_SIGPROF = prism (const SIGPROF) case _ of
  SIGPROF -> Right unit
  other -> Left other

_SIGPWR :: Prism' Signal Unit
_SIGPWR = prism (const SIGPWR) case _ of
  SIGPWR -> Right unit
  other -> Left other

_SIGQUIT :: Prism' Signal Unit
_SIGQUIT = prism (const SIGQUIT) case _ of
  SIGQUIT -> Right unit
  other -> Left other

_SIGSEGV :: Prism' Signal Unit
_SIGSEGV = prism (const SIGSEGV) case _ of
  SIGSEGV -> Right unit
  other -> Left other

_SIGSTKFLT :: Prism' Signal Unit
_SIGSTKFLT = prism (const SIGSTKFLT) case _ of
  SIGSTKFLT -> Right unit
  other -> Left other

_SIGSTOP :: Prism' Signal Unit
_SIGSTOP = prism (const SIGSTOP) case _ of
  SIGSTOP -> Right unit
  other -> Left other

_SIGSYS :: Prism' Signal Unit
_SIGSYS = prism (const SIGSYS) case _ of
  SIGSYS -> Right unit
  other -> Left other

_SIGTERM :: Prism' Signal Unit
_SIGTERM = prism (const SIGTERM) case _ of
  SIGTERM -> Right unit
  other -> Left other

_SIGTRAP :: Prism' Signal Unit
_SIGTRAP = prism (const SIGTRAP) case _ of
  SIGTRAP -> Right unit
  other -> Left other

_SIGTSTP :: Prism' Signal Unit
_SIGTSTP = prism (const SIGTSTP) case _ of
  SIGTSTP -> Right unit
  other -> Left other

_SIGTTIN :: Prism' Signal Unit
_SIGTTIN = prism (const SIGTTIN) case _ of
  SIGTTIN -> Right unit
  other -> Left other

_SIGTTOU :: Prism' Signal Unit
_SIGTTOU = prism (const SIGTTOU) case _ of
  SIGTTOU -> Right unit
  other -> Left other

_SIGUNUSED :: Prism' Signal Unit
_SIGUNUSED = prism (const SIGUNUSED) case _ of
  SIGUNUSED -> Right unit
  other -> Left other

_SIGURG :: Prism' Signal Unit
_SIGURG = prism (const SIGURG) case _ of
  SIGURG -> Right unit
  other -> Left other

_SIGUSR1 :: Prism' Signal Unit
_SIGUSR1 = prism (const SIGUSR1) case _ of
  SIGUSR1 -> Right unit
  other -> Left other

_SIGUSR2 :: Prism' Signal Unit
_SIGUSR2 = prism (const SIGUSR2) case _ of
  SIGUSR2 -> Right unit
  other -> Left other

_SIGVTALRM :: Prism' Signal Unit
_SIGVTALRM = prism (const SIGVTALRM) case _ of
  SIGVTALRM -> Right unit
  other -> Left other

_SIGWINCH :: Prism' Signal Unit
_SIGWINCH = prism (const SIGWINCH) case _ of
  SIGWINCH -> Right unit
  other -> Left other

_SIGXCPU :: Prism' Signal Unit
_SIGXCPU = prism (const SIGXCPU) case _ of
  SIGXCPU -> Right unit
  other -> Left other

_SIGXFSZ :: Prism' Signal Unit
_SIGXFSZ = prism (const SIGXFSZ) case _ of
  SIGXFSZ -> Right unit
  other -> Left other
