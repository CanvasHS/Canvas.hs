-- Canvas.Hs, control javascript canvas with Haskell
-- Copyright (C) 2013, Lennart Buit, Joost van Doorn, Pim Jager, Martijn Roo,
-- Thijs Scheepers
--
-- This library is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Lesser General Public
-- License as published by the Free Software Foundation; either
-- version 2.1 of the License, or (at your option) any later version.
-- 
-- This library is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- Lesser General Public License for more details.
-- 
-- You should have received a copy of the GNU Lesser General Public
-- License along with this library; if not, write to the Free Software
-- Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301
-- USA

{- | 
    The CanvasHs.Data module exposes all data types needed for the Event handler function.
    It exposes Events, Shapes, Actions and Output and all their needed sub-types
-}
module CanvasHs.Data where

import qualified Data.ByteString.Lazy as BSL

-- | Represents a Point on the canvas as a tuple of two Ints: (x, y) 
type Point = (Int, Int)

-- | Represents a path as a list of 'Point's
type Path = [Point]

-- | Represents a RGBA color as tuple of 3 Ints for RGB (value should range from 0 to 255) 
--   and a float for A (value should range from 0 to 1.0), (r, g, b, a)
--   these value ranges are not enforced by the encoding and values outside of the range will
--   be send to the canvas unchanged and could result in unexpected behaviour.
type Color = (Int, Int, Int, Float)

-- | A class that defines a function `defaults` that returns a default value for a given record
class Defaults a where
    defaults :: a

-- | Used in conjunction with the 'Event' Shape to decribe which events a 'Shape' is interested in.
data EventData = EventData {
                    -- | The ID of the event, should be unique. Duplicate id's could result in unexpected behaviour
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
                    mouseOut :: Bool,
                    -- | Toggles whether to react on scrollevents.
                    scroll :: Bool
                } deriving (Eq, Show)

-- | The 'Defaults' for 'EventData', has eventId="" and False for all possible events
instance Defaults EventData where
    defaults = EventData "" False False False False False False False False

-- | Defines fontsize as an Int
type FontSize = Int
	
-- | Defines text alignment, used in conjunction with 'TextData'
data Alignment 
    -- | Aligns the start of the text to the specified point.
    = AlignLeft 
    -- | Aligns the end of the text to the specified point.
    | AlignRight 
    -- | Aligns the center of the text to the specified point.
    | AlignCenter
		deriving (Eq, Show)

-- | Used in conjunction with the 'Text' shape to describe how to draw text
data TextData = TextData {
                    -- | The font for this text, no guarantees are made about availability
                    font :: String,
                    -- | The fontsize in Points or Pixels
                    size :: FontSize,
                    -- | Toggles bold.
                    bold :: Bool,
                    -- | Toggles italic.
                    italic :: Bool,
                    -- | Toggles underline. Note that as of yet underline is not implemented by the canvas
                    underline :: Bool,
                    -- | Specifies how to align this text.
                    alignment :: Alignment
                } deriving (Eq, Show)

-- | The 'Defaults' for 'TextData', has Arial 12, is left Aligned and is not bold, italic, or underlined.
instance Defaults TextData where
    defaults = TextData "Arial" 12 False False False AlignLeft

-- | All drawable Shapes and the transformations that can be applied to them. In practice these will be combined
--   in one big shape tree. By default shapes are filled in black (0,0,0,1.0)
data Shape 
    -- | A rectangle. Has a startpoint (left upper corner) and width, height
    = Rect Point Int Int
    -- | A circle. Has a centerpoint and a radius
    | Circle Point Int
    -- | An arch. Has a centerpoint, radius, startangle and endangle (counterclockwise)
    -- | Not yet implemented
    | Arc Point Int Int Int
    -- | A line. Has a path containing its points, doesn't connect start to end.
    | Line Path
    -- | A polygon. Has a path containing its points, does connect start to end to form a closed shape
    | Polygon Path
    -- | Text. Has a point (how to align to that point is set using the alignment field in TextData)
    --   a string containing the text to draw and 'TextData' containing extra data on how to draw the text
    | Text Point String TextData
    -- | Applies fill. Has a 'Color' and a shape that the fill will be applied to.
    | Fill Color Shape
    -- | Applies stroke. Has a 'Color', a strokewidth and a shape that the stroke will be applied to.
    | Stroke Color Int Shape
    -- | Applies rotation. Has a rotation (counterclockwise) in degrees and a shape that needs to be rotated.
    | Rotate Int Shape
    -- | Applies translation. Has an xdiff and ydiff (how much the Shape should be moved in x an y direction)
    --   and a shape that needs to be translated.
    | Translate Int Int Shape
    -- | Applies scale. Has xscale, yscale (how much the shape should be scaled in width and height)
    --   and a shape that needs to be scaled.
    | Scale Float Float Shape
    -- | Indicates that the given shape is interested in Events. 'EventData' describes this interest 
    | Event EventData Shape
    -- | Overrides the normal rotationpoint or scalepoint with the one specified
    | Offset Point Shape
    -- | A container. Has width and height and a list of shapes in this container. 
    --   Note that by default a container is drawn at point (0, 0) and it can be moved using 'Translate'
    | Container Int Int [Shape]

-- | A BlockingAction can not be combined with other Shapes or Actions in Output. Like an 'Action' it will instruct 
--   CanvasHs to do something. It will execute and then trigger an 'Event' with the result of the BlockingAction
data BlockingAction 
    -- | Loads a file as string. Has a filepath to load from
    = LoadFileString String
    -- | Loads a file in binary mode. Has a filepath to load from
    | LoadFileBinary String    
    
-- | An Action will instruct CanvasHs to do something on either the haskell side, such as saving a file or
--   starting a timer, or on the canvas side such as prompting for input or downloading a file. These Actions 
--   may or may not result in an 'Event' and could be done either by Haskell or the Canvas (javascript)
data Action
    -- | Saves a file as string. Has a filepath to save to, and a String of the file contents. 
    --   If the file already has contents it will be overwritten
    = SaveFileString String String
     -- | Saves a file in binary mode. Has a filepath to save to, and a ByteString of the file contents. When the file already has contents it will be overwritten
    | SaveFileBinary String BSL.ByteString
    -- | Starts a repeating Timer. Has a timeout in ms and a String identifying the Timer.
    | Timer Int String
    -- | Turns the debug console on or off. Has a Bool, True means show, False means hide
    --   is send to javascript
    | Debug Bool
    -- | Turns file drag'n'drop acceptance on or off. Has a Bool (True means accept, False means don't accept)
    --   and a Bool (True means accept multiple files, false means don't accept multiple files)
    --   Could result in one or multiple UploadComplete events
    --   is send to javascript
    | DragNDrop Bool Bool
    -- | Changes the window display type to the specified 'WindowDisplayType'
    --   is send to javascript
    | DisplayType WindowDisplayType
    -- | Sends a file to the javascript so the user can download it. Has the filename and filecontents as String
    --   is send to javacript
    | Download String String
    -- | Asks the user to select a file to upload, the Bool indicates if multiple files can be selected or not. 
    --   could result in one or multiple UploadComplete events. 
    --   is send to javascript
    | RequestUpload Bool
    -- | Prompts the user a message and asks for a certain value, with a default for that value.
    --   Has a String of the message to show and a String of the default value.
    | Prompt String String
    
-- | The window display type for use in conjunction with the 'DisplayType' 'Event'. FixedSize has a Width and Height
data WindowDisplayType = FullWindow | FullScreen | FixedSize Int Int
    
-- | RegularOutput is output consisting of Maybe a 'Shape' to draw and a list of 'Action's, Nothing implies nothing
--   should be changed on the canvas and an empty list implies no actions have to be taken
type RegularOutput = (Maybe Shape, [Action])

-- | Output is the return type of the handler. It is either a BlockingAction or RemoteOutput
--   It can't have both a BlockingAction and a Shape to draw, because the BlockingAction will 
--   trigger handler, which could then return also return a Shape, we then would not know
--   which Shape to draw.
data Output = Block BlockingAction | Out RegularOutput
    
-- | Modifier keys that could be held when pressing another key, for use in conjuncting with the 
--   'KeyUp', 'KeyDown' and 'KeyClick' 'Event's. 
data Modifier 
    = Shift | Ctrl | Alt
    deriving(Eq, Show)

-- | The events that can be triggered by CanvasHs. These could either be the result of user input (such as
--   i.e. MouseDown or KeyUp) or could be triggered by an 'Action' such as FileLoaded or Tick, or could be 
--   triggered by CanvasHs itself (StartEvent)
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
    -- | A mouseover event, consisting of a point and ID string of the interested object
    | MouseOver Point String
    -- | A mouseout event, consisting of a point and ID string of the interested object
    | MouseOut Point String
    -- | A keydown event, consist of a key that was pressed and a list of modifiers that were active
    | KeyDown String [Modifier]
    -- | A keyup event, consist of a key that was pressed and a list of modifiers that were active
    | KeyUp String [Modifier]
    -- | A keyclick event, consist of a key that was pressed and a list of modifiers that were active
    | KeyClick String [Modifier]
    -- | A scroll event consisting of an ID string of the interested object, 
    --   a xdiff and an ydiff (how much was scrolled in the x and y direction)
    | Scroll String Int Int
    -- | Start event is triggered when the server is started to notify user
    | StartEvent
    -- | When a file requested using the LoadFileString 'Action' has been loaded. 
    --   Has a filepath and file contents as String
    | FileLoadedString String String
    -- | When a file requested using the LoadFileString Action has been loaded. Has a filepath and file contents as ByteString
    | FileLoadedBinary String BSL.ByteString
    -- | Tick event from a Timer. Has a string identifying the Timer
    | Tick String
    -- | An upload has been completed. Has contents
    | UploadComplete (String, BSL.ByteString)
    -- | A reseizewindoweventm has a new width and height

    | WindowResize Int Int
    -- | A response of the user to the prompt. Has a String of the response.
    | PromptResponse String
    deriving(Eq, Show)
