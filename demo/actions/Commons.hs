module Commons where

import CanvasHs
import CanvasHs.Data

roundedRect :: (Int, Int) -> Int -> Int -> Int -> Shape
roundedRect (x, y) width height radius = Translate x y $ Container width height [
        -- The top left rounded corner
        -- should go from +90 to +180 degrees, so is rotated 90 degrees
        Rotate 90 $ Arc (radius, radius) radius 90,
        -- The top right rounded corner
        -- should go from 0 to +90 degrees, so isn't rotated at all
        Arc (width - radius, radius) radius 90,
        -- The bottom left rounded corner
        -- should go from +180 to +270 degrees, so is rotated 180 degrees
        Rotate 180 $ Arc (radius, height - radius) radius 90,
        -- The bottom right rounded corner
        -- should go from +270 to 0 degrees, or from 0 to -90 degrees
        Rotate (-90) $ Arc (width - radius, height - radius) radius 90,
        -- Top line
        Line [(radius, 0), (width - radius, 0)],
        -- Bottom line
        Line [(radius, height), (width - radius, height)],
        -- Left line
        Line [(0, radius), (0, height - radius)],
        -- Right line
        Line [(width, radius), (width, height - radius)],
        -- The fill polygon, without this only the corners would be filled
        -- This code draws a cross
        -- The stroke on this shape make sure that this polygon is never stroked
        Stroke (0,0,0,0.0) 0 $ Polygon [
            (radius, 0), 
            (radius, radius), 
            (0, radius),
            (0, height - radius),
            (radius, height - radius),
            (radius, height),
            (width - radius, height),
            (width - radius, height - radius),
            (width, height - radius),
            (width, radius),
            (width - radius, radius),
            (width - radius, 0)
        ]
    ]
