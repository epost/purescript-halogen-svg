module Halogen.Svg.Util where

import Prelude ((<<<))
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Web.HTML (HTMLElement)

type CssSelector = String

foreign import _beginElements :: CssSelector -> EffectFnAff Int

beginElements :: CssSelector -> Aff Int
beginElements = fromEffectFnAff <<< _beginElements

type Vec2 a = { x :: a, y :: a }
foreign import domToSvgCoordinates :: HTMLElement -> Vec2 Int -> Vec2 Int
