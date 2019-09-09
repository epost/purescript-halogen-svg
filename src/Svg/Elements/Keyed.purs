module Svg.Elements.Keyed where

import Prelude

import Core as Core

import Data.Tuple (Tuple)
import Halogen.HTML.Core (ElemName(ElemName), HTML)
import Halogen.HTML.Elements (keyedNS)
import Halogen.HTML.Properties (IProp)
import Svg.Indexed as I

-- Not exported by Halogen.HTML.Elements.Keyed
type KeyedNode r w i
   = Array (IProp r i)
     -> Array (Tuple String (HTML w i))
     -> HTML w i

svg :: forall p i. KeyedNode I.SVGsvg p i
svg = keyedNS Core.ns $ ElemName "svg"

g :: forall p i. KeyedNode I.SVGg p i
g = keyedNS Core.ns $ ElemName "g"

text :: forall p i. KeyedNode I.SVGtext p i
text = keyedNS Core.ns (ElemName "text")

foreignObject :: forall p i . KeyedNode I.SVGforeignObject p i
foreignObject = keyedNS Core.ns (ElemName "foreignObject")

marker :: forall p i. KeyedNode I.SVGmarker p i
marker = keyedNS Core.ns (ElemName "marker")

defs :: forall p i. Array (Tuple String (HTML p i)) -> HTML p i
defs = keyedNS Core.ns (ElemName "defs") []
