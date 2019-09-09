module Svg.Attributes where
-- Like Halogen.HTML.Properties

import Prelude

import Core as Core
import Data.Maybe (Maybe(..), maybe)
import Data.String (joinWith)
import Halogen.HTML.Core (Prop, AttrName(AttrName), Namespace(Namespace))
import Halogen.HTML.Properties (IProp, attrNS)
import Svg.Types (Align, printAlign, MeetOrSlice, printMeetOrSlice, PaintServer, StrokeLinecap, Transform, printTransform, D, printD, TextAnchor, printTextAnchor, FontSize, Baseline, printBaseline, Orient, printOrient, MarkerUnit, printMarkerUnit, PatternUnits, Point)
import Unsafe.Coerce (unsafeCoerce)

attr :: forall r i. AttrName -> String -> IProp r i
attr = coe Core.attr
  where
    coe :: (AttrName -> String -> Prop i) -> AttrName -> String -> IProp r i
    coe = unsafeCoerce

cx :: forall r i. String -> IProp (cx :: Number | r) i
cx = attr (AttrName "cx")

cy :: forall r i. String -> IProp (cy :: Number | r) i
cy = attr (AttrName "cy")

r :: forall s i. String -> IProp (r :: Number | s) i
r = attr (AttrName "r")

viewBox :: forall r i. Number -> Number -> Number -> Number -> IProp (viewBox :: String | r) i
viewBox x' y' w' h' = attr (AttrName "viewBox") (joinWith " " $ map show [x', y', w', h'])

preserveAspectRatio :: forall r i. Maybe {x :: Align, y :: Align} -> MeetOrSlice -> IProp (preserveAspectRatio :: String | r) i
preserveAspectRatio align slice =
  attr (AttrName "preserveAspectRatio") (joinWith " " $ [align_str, printMeetOrSlice slice])
  where
    align_str = case align of
      Nothing -> "none"
      Just align' -> joinWith "" $ ["x", printAlign align'.x, "Y", printAlign align'.y]

rx :: forall r i. String -> IProp (rx :: Number | r) i
rx = attr (AttrName "rx")

ry :: forall r i. String -> IProp (ry :: Number | r) i
ry = attr (AttrName "ry")

width :: forall r i. String -> IProp (width :: Number | r) i
width = attr (AttrName "width")

height :: forall r i. String -> IProp (height :: Number | r) i
height = attr (AttrName "height")

x :: forall r i. String -> IProp (x :: Number | r) i
x = attr (AttrName "x")

y :: forall r i. String -> IProp (y :: Number | r) i
y = attr (AttrName "y")

x1 :: forall r i. String -> IProp (x1 :: Number | r) i
x1 = attr (AttrName "x1")

y1 :: forall r i. String -> IProp (y1 :: Number | r) i
y1 = attr (AttrName "y1")

x2 :: forall r i. String -> IProp (x2 :: Number | r) i
x2 = attr (AttrName "x2")

y2 :: forall r i. String -> IProp (y2 :: Number | r) i
y2 = attr (AttrName "y2")

stroke :: forall r i. PaintServer -> IProp (stroke :: String | r) i
stroke = attr (AttrName "stroke") <<< show

strokeLinecap :: forall r i. StrokeLinecap -> IProp (strokeLinecap :: String | r) i
strokeLinecap = attr (AttrName "stroke-linecap") <<< show

fill :: forall r i. PaintServer -> IProp (fill :: String | r) i
fill = attr (AttrName "fill") <<< show

transform :: forall r i . Array Transform -> IProp (transform :: String | r) i
transform = attr (AttrName "transform") <<< joinWith " " <<< map printTransform

d :: forall r i . Array D -> IProp (d :: String | r) i
d = attr (AttrName "d") <<< joinWith " " <<< map printD

text_anchor :: forall r i . TextAnchor -> IProp (text_anchor :: String | r) i
text_anchor = attr (AttrName "text-anchor") <<< printTextAnchor

font_size :: forall r i. FontSize -> IProp (font_size :: String | r) i
font_size = attr (AttrName "font-size") <<< show

dominant_baseline :: forall r i . Baseline -> IProp (transform :: String | r) i
dominant_baseline = attr (AttrName "dominant-baseline") <<< printBaseline

-- TODO shouldn't this be 'classes' taking an (Array Classname), like the rest of Halogen?
class_ :: forall r i . String -> IProp (class :: String | r) i
class_ = attr (AttrName "class")

id :: forall r i . String -> IProp (id :: String | r) i
id = attr (AttrName "id")

markerWidth :: forall r i. String -> IProp (markerWidth :: Number | r) i
markerWidth = attr (AttrName "markerWidth")

markerHeight :: forall r i. String -> IProp (markerHeight :: Number | r) i
markerHeight = attr (AttrName "markerHeight")

refX :: forall r i. String -> IProp (refX :: Number | r) i
refX = attr (AttrName "refX")

refY :: forall r i. String -> IProp (refY :: Number | r) i
refY = attr (AttrName "refY")

orient :: forall r i. Orient -> IProp (orient :: String | r) i
orient = attr (AttrName "orient") <<< printOrient

markerUnits :: forall r i. MarkerUnit -> IProp (markerUnits :: String | r) i
markerUnits = attr (AttrName "markerUnits") <<< printMarkerUnit

strokeWidth :: forall r i. String -> IProp (strokeWidth :: Number | r) i
strokeWidth = attr (AttrName "stroke-width")

markerStart :: forall r i. String -> IProp (markerStart :: String | r) i
markerStart = attr (AttrName "marker-start")

markerMid :: forall r i. String -> IProp (markerMid :: String | r) i
markerMid = attr (AttrName "marker-mid")

markerEnd :: forall r i. String -> IProp (markerEnd :: String | r) i
markerEnd = attr (AttrName "marker-end")

href :: forall r i. String -> IProp (href :: String | r) i
href = attr (AttrName "href")

patternContentUnits :: forall r i. PatternUnits -> IProp (patternContentUnits :: String | r) i
patternContentUnits = attr (AttrName "patternContentUnits") <<< show

patternTransform :: forall r i. Transform -> IProp (patternTransform :: String | r) i
patternTransform = attr (AttrName "patternTransform") <<< printTransform

patternUnits :: forall r i. PatternUnits -> IProp (patternUnits :: String | r) i
patternUnits = attr (AttrName "patternUnits") <<< show

points :: forall r i. Array Point -> IProp (points :: String | r) i
points = attr (AttrName "points") <<< show

pathLength :: forall r i. Number -> IProp (points :: String | r) i
pathLength = attr (AttrName "pathLenth") <<< show

--------------------------------------------------------------------------------

-- | https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/dur
data DurationF a = Duration (Maybe a) (Maybe a) (Maybe a) (Maybe a) -- ^ TODO hours minutes seconds millis

derive instance functorDurationF :: Functor DurationF

printDurationF :: forall a. Show a => DurationF a -> String
printDurationF (Duration h m s i) = f "h" h <> f "m" m <> f "s" s <> f "i" i
  where f u = maybe "" (\v -> show v <> u)

type Duration = DurationF Number

-- TODO derive Show instance for DurationF

printDuration :: Duration -> String
printDuration = printDurationF

-- TODO add other constructors
seconds :: Number -> Duration
seconds s = Duration Nothing Nothing (Just s) Nothing

data FillState = Freeze | Remove

printFillState :: FillState -> String
printFillState = case _ of
  Freeze -> "freeze"
  Remove -> "remove"

dur :: forall r i. Duration -> IProp (dur :: String | r) i
dur = attr (AttrName "dur") <<< printDuration

-- TODO ADT or free string?
attributeName :: forall r i. String -> IProp (attributeName :: String | r) i
attributeName = attr (AttrName "attributeName")

-- https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/from
from :: forall r i. String -> IProp (from :: String | r) i
from = attr (AttrName "from")

-- https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/to
to :: forall r i. String -> IProp (to :: String | r) i
to = attr (AttrName "to")

-- https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/begin
begin :: forall r i. String -> IProp (begin :: String | r) i
begin = attr (AttrName "begin")

repeatCount :: forall r i. Int -> IProp (repeatCount :: Int | r) i
repeatCount = attr (AttrName "repeatCount") <<< show

-- TODO this is just 'fill', but that functino is already specialised to Color in this module
fillAnim :: forall r i. FillState -> IProp (fill :: String | r) i
fillAnim = attr (AttrName "fill") <<< printFillState

-- TODO xlink:href seems to have some issues, among others around its namespace
xlinkHref :: forall r i. String -> IProp (xlinkHref :: String | r) i
-- xlinkHref = attr (AttrName "xlink:href")
-- xlinkHref = attrNS (Namespace "xlink") (AttrName "href")
xlinkHref = attrNS (Namespace "xlink") (AttrName "xlink:href")

-- TODO copied from `d`; adapt where needed
path :: forall r i . Array D -> IProp (path :: String | r) i
path = attr (AttrName "path") <<< joinWith " " <<< map printD
