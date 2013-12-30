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

module CanvasHs.Data where

import qualified Data.ByteString.Lazy.UTF8 as BS8
import qualified Data.ByteString as BS

-- | Convenience type for a point (x, y)
type Point = (Int, Int)

-- | Convenience type for a path consisting of points
type Path = [Point]

-- | Convenience type for a RGBA color
type Color = (Int, Int, Int, Float)

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
                    mouseOut :: Bool,
                    -- | Toggles whether to react on scrollevents.
                    scroll :: Bool
                } deriving (Eq, Show)

-- | Defines defaults for EventData
instance Defaults EventData where
    defaults = EventData "" False False False False False False False False

-- | Defines textsize
type FontSize = Int
	
-- | Ways to align text	
data Alignment 
    -- | Aligns the start of the text to the specified point.
    = AlignLeft 
    -- | Aligns the end of the text to the specified point.
    | AlignRight 
    -- | Aligns the center of the text to the speciied point.
    | AlignCenter
		deriving (Eq, Show)

-- | Record holding formatting data for text, eg fonts and sizes
data TextData = TextData {
                    -- | The font for this text, no guarantees are made about availability
                    font :: String,
                    -- | The fontsize in Points or Pixels
                    size :: FontSize,
                    -- | Toggles bold.
                    -- TODO: implement this
                    bold :: Bool,
                    -- | Toggles italic.
                    -- TODO: implement this
                    italic :: Bool,
                    -- | Toggles underline.
                    -- TODO: implement this
                    underline :: Bool,
                    -- | Specifies how to align this text.
                    -- TODO: implement this
                    alignment :: Alignment
                } deriving (Eq, Show)

-- | Defines defaults for TextData
instance Defaults TextData where
    defaults = TextData "Arial" 12 False False False AlignLeft

-- | All drawable objects that the user can define (also includes some modifying objects like scale)
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
    -- | Overrides normal rotationpoint or scalepoint with one specified
    | Offset Point Shape
    -- | A container. Has width and height and a list of shapes in this container.
    | Container Int Int [Shape]

-- | Actions which will trigger an Event, such as LoadFile or Upload
data BlockingAction 
    -- | Loads a file as string. Has a filepath to load from
    = LoadFileString String
    -- | Loads a file in binary mode. Has a filepath to load from
    | LoadFileBinary String    
    
-- | Actions which don't trigger events such as SaveFile, Download, Debug
data Action
    -- | Saves a file as string. Has a filepath to save to, and a String of the file contents. When the file already has contents it will be overwritten
    = SaveFileString String String
     -- | Saves a file in binary mode. Has a filepath to save to, and a ByteString of the file contents. When the file already has contents it will be overwritten
    | SaveFileBinary String BS.ByteString
    -- | Starts a repeating Timer. Has a timeout in ms and a String identifying the Timer.
    | Timer Int String
    -- | Turns the debug console on or off. Has a Bool, True means show, False means hide, send to javascript
    | Debug Bool
    -- | Turns file drag'n'drop acceptance on or off. Has a Bool (True means accept, False means don't accept)
    -- | and a Bool (True means accept multiple files, false means don't accept multiple files)
    -- | Could result in one or multiple UploadComplete events
    -- | send to javascript
    | DragNDrop Bool Bool
    -- | changes the window display type, is eihter FixedSize, FullWindow or FullScreen, send to javascript.
    | DisplayType WindowDisplayType
    -- | Sends a file to the javascript so the user can download it. Has the filecontents as String, send to javacript
    | Download String String
    -- | Asks the user to select a file to upload, the Bool indicates if multiple files can be selected or not. 
    -- | could result in one or multiple UploadComplete events, send to javascript
    | RequestUpload Bool
    -- | Prompts the user a message and asks for a certain value, with a default for that value.
    -- | Has a String of the message to show and a String of the default value.
    | Prompt String String
    
    
-- | The window display type. FixedSize as a Width and Height
data WindowDisplayType = FullWindow | FullScreen | FixedSize Int Int
    
    
-- | RegularOutput is output consisting of a shape to draw and a list of actions, an empty list implies no actions
-- | have to be taken
type RegularOutput = (Maybe Shape, [Action])

-- | Output is the return type of the handler. It is either a BlockingAction or RemoteOutput
-- | It can't have both a BlockingAction and a Shape to draw, because the BlockingAction will 
-- | trigger handler, which could then return also return a Shape, we then would not know
-- | which Shape to draw.
data Output = Block BlockingAction | Out RegularOutput
    
-- | Keymodifiers that can be enabled in a keyboard event
data Modifier 
    = Shift | Ctrl | Alt
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
    | KeyDown String [Modifier]
    -- | A keyup event, consist of a keycharacter that was pressed and a list of modifiers that were active
    | KeyUp String [Modifier]
    -- | A keyclick event, consist of a keycharacter that was pressed and a list of modifiers that were active
    | KeyClick String [Modifier]
    -- | A scroll event consisting of an eventId (the shape that was scrolled) xdiff and a ydiff
    | Scroll String Int Int
    -- | Start event is thrown when the server is started to notify user
    | StartEvent
    -- | When a file requested using the LoadFileString Action has been loaded. Has a filepath and file contents as String
    | FileLoadedString String String
    -- | When a file requested using the LoadFileString Action has been loaded. Has a filepath and file contents as ByteString
    | FileLoadedBinary String BS.ByteString
    -- | Tick event from a Timer. Has a string identifying the Timer
    | Tick String
    -- | An upload has been completed. Has contents
    | UploadComplete (String, BS8.ByteString)
    -- | A reseizewindoweventm has a new width and height
    | WindowResize Int Int
    -- | An response of the user to the prompt. Has a String of the response.
    | PromptResponse String
    deriving(Eq, Show)
