module Svg.Types where

import Prelude
import Data.String (joinWith, toUpper)

data Color = RGB Int Int Int
           | RGBA Int Int Int Number

printColor :: Color -> String
printColor (RGB r' g' b') = "rgb(" <> (joinWith "," $ map show [r', g', b']) <> ")"
printColor (RGBA r' g' b' o') = "rgba(" <> (joinWith "," $ map show [r', g', b']) <> "," <> show o' <> ")"

data PaintServer
  = PaintNone
  | PaintColor Color
  | Pattern String
  | Gradient String
  | Hatch String

instance showPaintServer :: Show PaintServer where
  show PaintNone = "None"
  show (PaintColor color) = printColor color
  show (Pattern pattern) = pattern
  show (Gradient gradient) = gradient
  show (Hatch hatch) = hatch

data Transform
  = Matrix Number Number Number Number Number Number
  | Translate Number Number
  | Scale Number Number
  | Rotate Number Number Number
  | SkewX Number
  | SkewY Number

printTransform :: Transform -> String
printTransform (Matrix a' b' c' d' e' f') =
  "matrix(" <> (joinWith "," $ map show [a', b', c', d', e', f']) <> ")"
printTransform (Translate x' y') = "translate(" <> (joinWith "," $ map show [x', y']) <> ")"
printTransform (Scale x' y') = "scale(" <> (joinWith "," $ map show [x', y']) <> ")"
printTransform (Rotate a' x' y') = "rotate(" <> (joinWith "," $ map show [a', x', y']) <> ")"
printTransform (SkewX a') = "skewX(" <> show a' <> ")"
printTransform (SkewY a') = "skewY(" <> show a' <> ")"


data TextAnchor = Start | AnchorMiddle | End

printTextAnchor :: TextAnchor -> String
printTextAnchor Start = "start"
printTextAnchor AnchorMiddle = "middle"
printTextAnchor End = "end"

data CSSLength
  = Cm Number
  | Mm Number
  | Inches Number
  | Px Number
  | Pt Number
  | Pc Number
  | Em Number
  | Ex Number
  | Rem Number
  | Vw Number
  | Vh Number
  | Vmin Number
  | Vmax Number
  | Pct Number
  | Nil

instance showCSSLength :: Show CSSLength where
  show (Cm i) = (show i) <> "cm"
  show (Mm i) = (show i) <> "mm"
  show (Inches i) = (show i) <> "in"
  show (Px i) = (show i) <> "px"
  show (Pt i) = (show i) <> "pt"
  show (Pc i) = (show i) <> "pc"
  show (Em i) = (show i) <> "em"
  show (Ex i) = (show i) <> "ex"
  show (Rem i) = (show i) <> "rem"
  show (Vw i) = (show i) <> "vw"
  show (Vh i) = (show i) <> "vh"
  show (Vmin i) = (show i) <> "vmin"
  show (Vmax i) = (show i) <> "vmax"
  show (Pct i) = (show i) <> "%"
  show Nil = "0"

data FontSize
  = XXSmall
  | XSmall
  | Small
  | Medium
  | Large
  | XLarge
  | XXLarge
  | Smaller
  | Larger
  | FontSizeLength CSSLength

instance showFontSize :: Show FontSize where
  show XXSmall = "xx-small"
  show XSmall = "x-small"
  show Small = "small"
  show Medium = "medium"
  show Large = "large"
  show XLarge = "x-large"
  show XXLarge = "xx-large"
  show Smaller = "smaller"
  show Larger = "larger"
  show (FontSizeLength l) = show l

data Orient
  = AutoOrient
  | AutoStartReverse

instance showOrient :: Show Orient where
  show AutoOrient = "auto"
  show AutoStartReverse = "auto-start-reverse"

printOrient :: Orient -> String
printOrient AutoOrient = "auto"
printOrient AutoStartReverse = "auto-start-reverse"

data MarkerUnit
  = UserSpaceOnUse
  | StrokeWidth

instance showMarkerUnit :: Show MarkerUnit where
  show UserSpaceOnUse = "userSpaceOnUse"
  show StrokeWidth = "strokeWidth"

printMarkerUnit :: MarkerUnit -> String
printMarkerUnit UserSpaceOnUse = "userSpaceOnUse"
printMarkerUnit StrokeWidth = "strokeWidth"

data Baseline
  = Auto | UseScript | NoChange | ResetSize | Ideographic | Alphabetic | Hanging
  | Mathematical | Central | BaselineMiddle | TextAfterEdge | TextBeforeEdge

printBaseline :: Baseline -> String
printBaseline Auto = "auto"
printBaseline UseScript = "use-script"
printBaseline NoChange = "no-change"
printBaseline ResetSize = "reset-size"
printBaseline Ideographic = "ideographic"
printBaseline Alphabetic = "alphabetic"
printBaseline Hanging = "hanging"
printBaseline Mathematical = "mathematical"
printBaseline Central = "central"
printBaseline BaselineMiddle = "middle"
printBaseline TextAfterEdge = "text-after-edge"
printBaseline TextBeforeEdge = "text-before-edge"

data D = Rel Command | Abs Command
printD :: D -> String
printD (Abs cmd) = (toUpper p.command) <> p.params
  where p = printCommand cmd
printD (Rel cmd) = p.command <> p.params
  where p = printCommand cmd

data Command
  = M Number Number
  | L Number Number
  | C Number Number Number Number Number Number
  | S Number Number Number Number
  | Q Number Number Number Number
  | T Number Number
  | A Number Number Number Boolean Boolean Number Number
  | Z

printCommand :: Command -> {command :: String, params :: String}
printCommand (M x' y') = {command: "m", params: joinWith "," $ map show [x', y']}
printCommand (L x' y') = {command: "l", params: joinWith "," $ map show [x', y']}
printCommand (C x1' y1' x2' y2' x' y') =
  {command: "c" , params: joinWith "," $ map show [x1', y1', x2', y2', x', y']}
printCommand (S x2' y2' x' y') =
  {command: "s" , params: joinWith "," $ map show [x2', y2', x', y']}
printCommand (Q x1' y1' x' y') =
  {command: "q" , params: joinWith "," $ map show [x1', y1', x', y']}
printCommand (T x' y') = {command: "t", params: joinWith "," $ map show [x', y']}
printCommand (A rx' ry' rot large sweep x' y') =
  {command: "a", params: joinWith ","
                 $ map show [ rx', ry', rot ]
                 <> [ large_flag, sweep_flag ]
                 <> map show [ x', y' ]}
  where
  large_flag = if large then "0" else "1"
  sweep_flag = if sweep then "0" else "1"
printCommand Z = {command: "z", params: ""}

data Align = Min | Mid | Max

printAlign :: Align -> String
printAlign Min = "Min"
printAlign Mid = "Mid"
printAlign Max = "Max"

data MeetOrSlice = Meet | Slice
printMeetOrSlice :: MeetOrSlice -> String
printMeetOrSlice Meet = "meet"
printMeetOrSlice Slice = "slice"

data StrokeLinecap
  = Butt
  | Round
  | Square

instance showStrokeLinecap :: Show StrokeLinecap where
  show Butt = "butt"
  show Round = "round"
  show Square = "square"

data PatternUnits
  = PatternUnitsUserSpaceOnUse
  | ObjectBoundingBox

instance showPatternUnits :: Show PatternUnits where
  show PatternUnitsUserSpaceOnUse = "userSpaceOnUse"
  show ObjectBoundingBox = "objectBoundingBox"

data Point = Point Number Number

instance showPoint :: Show Point where
  show (Point a b) = show a <> "," <> show b

printPoints :: Array Point -> String
printPoints points = joinWith " " $ map show points
