module CanvasHs.Data where

type Point = (Int, Int)

type Path = [Point]

type Color = (Int, Int, Int, Int)

class Defaults a where
    defaults = a

data EventData = EventData {
                    -- | The ID of the event, should be unique.
                    eventId :: String,
                    -- | Toggles whether to react on mouseDown.
                    mouseDown :: Bool,
                    -- | Toggles whether to react on mouseClick.
                    mouseClick :: Bool,
                    -- | Toggles whether to react on mouseUp.
                    mouseUp :: Bool,
                    -- | Toggles whether to react on mouseDoubleClick.
                    mouseDoubleClick :: Bool,
                    -- | Toggles whether to react on mouseDrag.
                    mouseDrag :: Bool,
                    -- | Toggles whether to react on mouseEnter.
                    mouseEnter :: Bool,
                    -- | Toggles whether to react on mouseLeave.
                    mouseLeave :: Bool
                } deriving (Eq, Show)

instance Defaults EventData where
    defaults = Event "" False False False False False False False

data FontSize = Pt Int | Px Int

data Alignment 
    -- | Aligns the start of the text to the specified point.
    = Start 
    -- | Aligns the end of the text to the specified point.
    | End 
    -- | Aligns the center of the text to the speciied point.
    | Center

data TextData = TextData {
                    -- | The font for this text, no guarantees are made about availability
                    font :: String,
                    -- | The fontsize in Points or Pixels
                    size :: FontSize,
                    -- | Toggles bold.
                    bold :: Bool,
                    -- | Toggles italic.
                    italic :: Bool,
                    -- | Toggles underline.
                    underline :: Bool,
                    -- | Specifies how to align this text.
                    alignment :: Alignment
                } deriving (Eq, Show)

instance Defaults TextData where
    defaults = TextData "Arial" (Pt 12) False False False Center

data Shape 
    -- | A rectangle. Has a startpoint (left upper corner) and width, height
    = Rect Point Int Int
    -- | A circle. Has a centerpoint and a radius.
    | Circle Point Int
    -- | An arch. Has a centerpoint, radius, startangle and endangle (counterclockwise).
    | Arc Point Int Int Int
    -- | A line. Has a path containing its points, doesn't connect start and end.
    | Line Path
    -- | A polygon. Has a path containing its points, does connect start with end.
    | Polygon Path
    -- | A text. Has a centerpoint a string containing the text and some extra data.
    | Text Point String TextData
    -- | Applies fill. Has a color and a shape that needs to be filled.s
    | Fill Color Shape
    -- | Applies stroke. Has a color, a strokewidth and a shape that needs to be stroked.
    | Stroke Color Int Shape
    -- | Applies rotate. Has a rotation (counterclockwise) and a shape that needs to be rotated.
    | Rotate Int Shape
    -- | Applies translation. Has xdiff, ydiff and a shape that needs to be translated.
    | Translate Int Int Shape
    -- | Applies scale. Has xscale, yscale and a shape that needs to be scaled.
    | Scale Float Float Shape
    {- 
        | Adds eventdata to a shape.
          The eventdata contains booleans for the events that the shape is interested in.
    -}
    | Event EventData Shape
    -- | A container. Has width and height and a list of shapes in this container.
    | Container Int Int [Shape]
