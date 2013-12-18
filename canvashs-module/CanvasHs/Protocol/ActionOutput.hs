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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module CanvasHs.Protocol.ActionOutput
(   actionEncode
,   JSONAction
)   where

import GHC.Generics
import Data.Aeson.TH
import qualified Data.Text as T

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Lazy.UTF8 as BUL
import qualified Data.ByteString.Base64.Lazy as B64

import qualified CanvasHs.Data as D

data JSONAction
    = JSONAction {
        --keep these exactly like this, 'action' is dropped in the ToJSON instance
        actionaction :: T.Text,
        actiondata :: JSONActionData
    } deriving (Show)
    
data JSONActionData
    = JSONActionData {
        --keep these exactly like this, 'd' is dropped in the ToJSON instance
        dtype :: Maybe Int,
        dwidth :: Maybe Int,
        dheight :: Maybe Int,
        denabled :: Maybe Bool,
        dmultiple :: Maybe Bool,
        dfilename :: Maybe BU.ByteString,
        dfilecontents :: Maybe BSL.ByteString
    } deriving (Show)
    
   
$(deriveJSON defaultOptions{fieldLabelModifier = drop 6} ''JSONAction)

$(deriveJSON defaultOptions{omitNothingFields=True, fieldLabelModifier = drop 1} ''JSONActionData)

-- | Converts Action to JSONAction, this can be used with Aeson
actionEncode :: D.Action -> JSONAction
actionEncode (D.Debug a)        = JSONAction{actionaction = "debugger"
                                            ,actiondata = emptyActionData{denabled = Just a}
                                            }
actionEncode (D.DragNDrop a m)  = JSONAction{actionaction = "acceptfiledragndrop"
                                            ,actiondata = emptyActionData{denabled = Just a
                                                                         ,dmultiple = Just m
                                                                         }
                                            }
actionEncode (D.DisplayType w)  = JSONAction{actionaction = "windowdisplaytype"
                                            ,actiondata = wdtEncode w
                                            }
actionEncode (D.Download fn fc)       = JSONAction{actionaction = "download"
                                            ,actiondata = emptyActionData{dfilecontents = Just filecontents, dfilename = Just filename}}
                                        where
                                            filecontents = B64.encode $ BUL.fromString fc
                                            filename = BU.fromString fn
                                            
actionEncode (D.RequestUpload b)    = JSONAction{actionaction = "requestupload" 
                                                ,actiondata = emptyActionData{dmultiple = Just b}
                                                }

wdtEncode :: D.WindowDisplayType -> JSONActionData
wdtEncode (D.FixedSize w h) = emptyActionData   {dtype  = Just 0
                                                ,dwidth = Just w
                                                ,dheight= Just h
                                                }
wdtEncode (D.FullWindow)    = emptyActionData   {dtype  = Just 1}
wdtEncode (D.FullScreen)    = emptyActionData   {dtype  = Just 2}
                                        
emptyActionData :: JSONActionData
emptyActionData
    = JSONActionData {
            --keep these exactly like this, 'd' is dropped in the ToJSON instance
            dtype = Nothing,
            dwidth = Nothing,
            dheight = Nothing,
            denabled = Nothing,
            dmultiple = Nothing,
            dfilename = Nothing,
            dfilecontents = Nothing
        }
