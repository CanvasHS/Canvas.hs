module CanvasHs.Data where

-- | Convenience type for a point (x, y)
type Point = (Int, Int)

-- | Convenience type for a path consisting of points
type Path = [Point]

-- | Convenience type for a RGBA color
type Color = (Int, Int, Int, Int)

-- | A class that defines a function `defaults` that returns a default value
class Defaults a where
    defaults :: a

-- | Stores data about what events a shape is interested in
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
                    -- | Toggles whether to react on mouseOver
                    mouseOver :: Bool,
                    -- | Toggles whether to react on mouseOut.
                    mouseOut :: Bool
                } deriving (Eq, Show)

-- | Defines defaults for EventData
instance Defaults EventData where
    defaults = EventData "" False False False False False False False

-- | Defines textsize
data FontSize = Pt Int | Px Int
		deriving (Eq, Show)
	
-- | Ways to align text	
data Alignment 
    -- | Aligns the start of the text to the specified point.
    = Start 
    -- | Aligns the end of the text to the specified point.
    | End 
    -- | Aligns the center of the text to the speciied point.
    | Center
		deriving (Eq, Show)

-- | Record holding formatting data for text, eg fonts and sizes
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

-- | Defines defaults for TextData
instance Defaults TextData where
    defaults = TextData "Arial" (Pt 12) False False False Center

-- | All drawable objects that the user can define (also includes some modifying objects like scale)
data Shape 
    -- | A rectangle. Has a startpoint (left upper corner) and width, height
    = Rect Point Int Int
    -- | A circle. Has a centerpoint and a radius.
    | Circle Point Int
    -- | An arch. Has a centerpoint, radius, startangle and endangle (counterclockwise).\
    -- | Not yet implemented
    | Arc Point Int Int Int
    -- | A line. Has a path containing its points, doesn't connect start and end.
    | Line Path
    -- | A polygon. Has a path containing its points, does connect start with end.
    | Polygon Path
    -- | A text. Has a centerpoint a string containing the text and some extra data.
    | Text Point String TextData
    -- | Applies fill. Has a color and a shape that needs to be filled.
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

-- | Keymodifiers that can be enabled in a keyboard event
data Modifier 
    = Shift | Ctrl | Alt | Super
    deriving(Eq, Show)

-- | The events the user can expect to get as input
data Event
    -- | A mousedown event consisting of a point and ID string of the interested object
    = MouseDown Point String
    -- | A mouseclick event consisting of a point and ID string of the interested object
    | MouseClick Point String
    -- | A mouseup event consisting of a point and ID string of the interested object
    | MouseUp Point String
    -- | A mousedoubleclick event consisting of a point and ID string of the interested object
    | MouseDoubleClick Point String
    -- TODO: Kan je ook in het niets draggen?
    -- | A mousedrag event with start and end, both consisting of a Point an ID string
    | MouseDrag Point String Point String
    -- | A mouseover event, therefore both Point and an ID string are set.
    | MouseOver Point String
    -- | A mouseout event, therefore both Point and an ID string are set.
    | MouseOut Point String
    -- | A keydown event, consist of a keycharacter that was pressed and a list of modifiers that were active
    | KeyDown Char [Modifier]
    -- | A keyup event, consist of a keycharacter that was pressed and a list of modifiers that were active
    | KeyUp Char [Modifier]
    -- | A keyclick event, consist of a keycharacter that was pressed and a list of modifiers that were active
    | KeyClick Char [Modifier]
    -- | A scroll event consisting of a xdiff and a ydiff
    | Scroll Int Int
	-- | Start event is thrown when the server is started to notify user
	| StartEvent
    deriving(Eq, Show)
