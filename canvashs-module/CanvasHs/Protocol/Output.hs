{-# LANGUAGE DeriveGeneric #-}

module CanvasHs.Protocol.Output where

import GHC.Generics

import CanvasHs.Data

data JSONShape
    = JSONShape { 
        shapeType      :: Text, 
        shapeData      :: JSONShapeData, 
        shapeEventData :: Maybe JSONEventData 
    }

data JSONShapeData
    = JSONShapeData { 
--        stroke         :: Maybe Text, 
        strokeWidth    :: Maybe Int, 
--        fill           :: Maybe Text, 
        scaleX         :: Maybe Float, 
        scaleY         :: Maybe Float, 
        rotateDeg      :: Maybe Int, 
        fontSize       :: Maybe Int, 
        fontFamily     :: Maybe Text, 
        points         :: Maybe [Int], 
        x              :: Maybe Int, 
        y              :: Maybe Int, 
        width          :: Maybe Int, 
        height         :: Maybe Int, 
        radius         :: Maybe Int 
    }

data JSONEventData
    = JSONEventData { 
        eventId        :: Maybe Text, 
        listen         :: Maybe [Text]
    }

instance ToJSON JSONShape
instance ToJSON JSONShapeData
instance TOJSON JSONEventData

