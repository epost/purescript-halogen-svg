module Core where
-- Like Halogen.HTML.Core

import Data.Maybe (Maybe(..))
import Halogen.HTML.Core (HTML, Prop(Attribute), Namespace(Namespace), AttrName(AttrName))
import Halogen.VDom (ElemName, VDom(Elem))
import Unsafe.Coerce (unsafeCoerce)

ns :: Namespace
ns = Namespace "http://www.w3.org/2000/svg"

element :: forall p i. ElemName -> Array (Prop i) -> Array (HTML p i) -> HTML p i
element = coe (\name props children -> Elem (Just ns) name props children)
  where
    coe :: (ElemName -> Array (Prop i) -> Array (VDom (Array (Prop i)) p) -> VDom (Array (Prop i)) p)
        -> ElemName -> Array (Prop i) -> Array (HTML p i) -> HTML p i
    coe = unsafeCoerce

attr :: forall i. AttrName -> String -> Prop i
attr (AttrName name) = Attribute Nothing name
