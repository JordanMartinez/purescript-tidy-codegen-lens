module PureScript.CST.Traversal.Lens where

import Data.Lens.Iso (Iso')
import Data.Tuple (Tuple)
import Prelude (identity)
import PureScript.CST.Traversal (MonoidalRewrite, PureRewrite, PureRewriteWithContext, Rewrite, RewriteWithContext)

_Rewrite :: forall e f g. Iso' (Rewrite e f g) (g e -> f (g e))
_Rewrite = identity

_RewriteWithContext
  :: forall c e f g. Iso' (RewriteWithContext c e f g) (c -> g e -> f (Tuple c (g e)))
_RewriteWithContext = identity

_MonoidalRewrite :: forall e m g. Iso' (MonoidalRewrite e m g) (g e -> m)
_MonoidalRewrite = identity

_PureRewrite :: forall e g. Iso' (PureRewrite e g) (g e -> g e)
_PureRewrite = identity

_PureRewriteWithContext
  :: forall c e g. Iso' (PureRewriteWithContext c e g) (c -> g e -> Tuple c (g e))
_PureRewriteWithContext = identity
