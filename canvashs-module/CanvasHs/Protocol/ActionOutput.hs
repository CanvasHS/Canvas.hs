{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module CanvasHs.Protocol.ActionOutput
(   actionEncode
,   JSONAction
)   where

import GHC.Generics
import Data.Aeson.TH
import qualified Data.Text as T

import qualified Data.ByteString.Lazy.UTF8 as B
import qualified Data.ByteString.Base64.Lazy as B64

import qualified CanvasHs.Data as D

data JSONAction
    = JSONAction {
        --keep these exactly like this, 'action' is dropped in the ToJSON instance
        actiontype :: T.Text,
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
        dfilename :: Maybe T.Text,
        dfilecontents :: Maybe T.Text --TODO: fix type
    } deriving (Show)
    
   
$(deriveJSON defaultOptions{fieldLabelModifier = drop 6} ''JSONAction)

$(deriveJSON defaultOptions{omitNothingFields=True, fieldLabelModifier = drop 1} ''JSONActionData)

-- | Converts Action to JSONAction, this can be used with Aeson
actionEncode :: D.Action -> JSONAction
actionEncode (D.Debug a)        = JSONAction{actiontype = "debugger"
                                            ,actiondata = emptyActionData{denabled = Just a}
                                            }
actionEncode (D.DragNDrop a m)  = JSONAction{actiontype = "acceptfiledragndrop"
                                            ,actiondata = emptyActionData{denabled = Just a
                                                                         ,dmultiple = Just m
                                                                         }
                                            }
actionEncode (D.DisplayType w)  = JSONAction{actiontype = "windowdisplaytype"
                                            ,actiondata = wdtEncode w
                                            }
actionEncode (D.Download fn fc)       = JSONAction{actiontype = "savefile"
                                            ,actiondata = emptyActionData{dfilecontents = Just filecontents, dfilename = Just filename}}
                                        where
                                            -- we maken er een bytestring van, die decoden we naar b64 dan weer
                                            -- naar string dan weer naar text, capiche?
                                            filecontents = T.pack $ B.toString $ B64.encode $ B.fromString $ fc
                                            filename = T.pack $ fn
                                            

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
