module PureScript.CST.Layout.Lens where

import Data.Either (Either(..))
import Data.Lens.Iso (Iso')
import Data.Lens.Prism (Prism', prism)
import Data.List (List)
import Data.Tuple (Tuple)
import Prelude (Unit, const, identity, unit)
import PureScript.CST.Layout (LayoutStack, LayoutDelim(..))
import PureScript.CST.Types (SourcePos)

_LayoutStack :: Iso' LayoutStack (List (Tuple SourcePos LayoutDelim))
_LayoutStack = identity

_LytRoot :: Prism' LayoutDelim Unit
_LytRoot = prism (const LytRoot) case _ of
  LytRoot -> Right unit
  other -> Left other

_LytTopDecl :: Prism' LayoutDelim Unit
_LytTopDecl = prism (const LytTopDecl) case _ of
  LytTopDecl -> Right unit
  other -> Left other

_LytTopDeclHead :: Prism' LayoutDelim Unit
_LytTopDeclHead = prism (const LytTopDeclHead) case _ of
  LytTopDeclHead -> Right unit
  other -> Left other

_LytDeclGuard :: Prism' LayoutDelim Unit
_LytDeclGuard = prism (const LytDeclGuard) case _ of
  LytDeclGuard -> Right unit
  other -> Left other

_LytCase :: Prism' LayoutDelim Unit
_LytCase = prism (const LytCase) case _ of
  LytCase -> Right unit
  other -> Left other

_LytCaseBinders :: Prism' LayoutDelim Unit
_LytCaseBinders = prism (const LytCaseBinders) case _ of
  LytCaseBinders -> Right unit
  other -> Left other

_LytCaseGuard :: Prism' LayoutDelim Unit
_LytCaseGuard = prism (const LytCaseGuard) case _ of
  LytCaseGuard -> Right unit
  other -> Left other

_LytLambdaBinders :: Prism' LayoutDelim Unit
_LytLambdaBinders = prism (const LytLambdaBinders) case _ of
  LytLambdaBinders -> Right unit
  other -> Left other

_LytParen :: Prism' LayoutDelim Unit
_LytParen = prism (const LytParen) case _ of
  LytParen -> Right unit
  other -> Left other

_LytBrace :: Prism' LayoutDelim Unit
_LytBrace = prism (const LytBrace) case _ of
  LytBrace -> Right unit
  other -> Left other

_LytSquare :: Prism' LayoutDelim Unit
_LytSquare = prism (const LytSquare) case _ of
  LytSquare -> Right unit
  other -> Left other

_LytIf :: Prism' LayoutDelim Unit
_LytIf = prism (const LytIf) case _ of
  LytIf -> Right unit
  other -> Left other

_LytThen :: Prism' LayoutDelim Unit
_LytThen = prism (const LytThen) case _ of
  LytThen -> Right unit
  other -> Left other

_LytProperty :: Prism' LayoutDelim Unit
_LytProperty = prism (const LytProperty) case _ of
  LytProperty -> Right unit
  other -> Left other

_LytForall :: Prism' LayoutDelim Unit
_LytForall = prism (const LytForall) case _ of
  LytForall -> Right unit
  other -> Left other

_LytTick :: Prism' LayoutDelim Unit
_LytTick = prism (const LytTick) case _ of
  LytTick -> Right unit
  other -> Left other

_LytLet :: Prism' LayoutDelim Unit
_LytLet = prism (const LytLet) case _ of
  LytLet -> Right unit
  other -> Left other

_LytLetStmt :: Prism' LayoutDelim Unit
_LytLetStmt = prism (const LytLetStmt) case _ of
  LytLetStmt -> Right unit
  other -> Left other

_LytWhere :: Prism' LayoutDelim Unit
_LytWhere = prism (const LytWhere) case _ of
  LytWhere -> Right unit
  other -> Left other

_LytOf :: Prism' LayoutDelim Unit
_LytOf = prism (const LytOf) case _ of
  LytOf -> Right unit
  other -> Left other

_LytDo :: Prism' LayoutDelim Unit
_LytDo = prism (const LytDo) case _ of
  LytDo -> Right unit
  other -> Left other

_LytAdo :: Prism' LayoutDelim Unit
_LytAdo = prism (const LytAdo) case _ of
  LytAdo -> Right unit
  other -> Left other
