module Ex01 where

-- needed to display the picture in the playground
import Codec.Picture

-- our line graphics programming interface
import ShapeGraphics



pointByTuple:: (Float, Float) -> Point
pointByTuple (a, b) = Point a b

tupleArrToPointArr:: [(Float, Float)] -> [Point]
tupleArrToPointArr arr = map pointByTuple arr


-- Part 1
-- picture of a house
housePic :: Picture
housePic = [door, house] ++ chimneyHouse
  where
    house :: PictureObject
    house = Path (tupleArrToPointArr houseCOs) green Solid
    door :: PictureObject
    door  = Path (tupleArrToPointArr doorCOs) red Solid
    
-- these are the coordinates - convert them to a list of Point
houseCOs :: [(Float, Float)]
houseCOs = [(300, 750), (300, 450), (270, 450), (500, 200),
        -- the addded cor
        (730, 450), (700, 450), (700, 750)]

doorCOs :: [(Float, Float)]
doorCOs = [(420, 750), (420, 550), (580, 550), (580, 750)]

grey :: Colour
grey = Colour 255 255 255 128

smokeCOs :: [(Float, Float)]
smokeCOs = [(635, 240), (625, 230), (635, 220), (625, 210)]

chimmyHouseCos :: [(Float, Float)]
chimmyHouseCos = [(615, 325), (615, 250), (650, 250),(650, 363)]
  -- smoke :: PictureObject
-- smoke = error "'smoke' unimplemented"

chimny:: PictureObject
chimny = Path (tupleArrToPointArr chimmyHouseCos) green Solid

blackChimney:: PictureObject
blackChimney = Path (
    tupleArrToPointArr (
      (head chimmyHouseCos ): (last chimmyHouseCos ):[]
    )
  ) black Solid

smoke :: PictureObject
smoke = Path (tupleArrToPointArr smokeCOs) grey Solid

chimneyHouse :: Picture
chimneyHouse = [ blackChimney,chimny, smoke]


-- Part 2
movePoint :: Point -> Vector -> Point
movePoint (Point x y) (Vector xv yv)
  = Point (x + xv) (y + yv)

movePictureObject :: Vector -> PictureObject -> PictureObject
movePictureObject vec (Path points colour lineStyle) 
  = Path (map (\x -> movePoint x vec) points) colour lineStyle
movePictureObject vec (
    Circle centerPO radiusPO colourPO lineStylePO fillStylePO
  ) 
  = Circle (movePoint centerPO vec) radiusPO colourPO lineStylePO fillStylePO
movePictureObject vec (
    Ellipse centerPO widthPO heightPO rotationPO colourPO lineStylePO fillStylePO
  )
  = Ellipse (movePoint centerPO vec)
      widthPO heightPO rotationPO colourPO lineStylePO fillStylePO
movePictureObject vec (
    Polygon pointsPO colourPO lineStylePO fillStylePO
  )
  = Polygon (map (\x -> movePoint x vec) pointsPO) colourPO lineStylePO fillStylePO
  
-- let myRed = red { opacityC = 180 }
-- let xy = (Point 400 400)
-- let circ = Circle xy 100 myRed Solid SolidFill
-- let v = (Vector 100 100)
-- writeToFile [circ, movePictureObject v circ]
-- :q

-- Part 3

-- generate the picture consisting of circles:
-- [Circle (Point 400 400) (400/n) col Solid SolidFill,
--  Circle (Point 400 400) 2 * (400/n) col Solid SolidFill,
--  ....
--  Circle (Point 400 400) 400 col Solid SolidFill]
simpleCirclePic :: Colour -> Float -> Picture
simpleCirclePic col n = circlesArr
  where  
    drawCricle :: Float -> PictureObject
    drawCricle i = Circle (Point 400 400) (i*400/n) col Solid SolidFill
    circlesArr:: [PictureObject]
    circlesArr = map drawCricle [1 .. n]
    -- circlesArr = drawCricle 1

-- use 'writeToFile' to write a picture to file "ex01.png" to test your
-- program if you are not using Haskell for Mac
-- e.g., call
-- writeToFile [house, door]

writeToFile pic
  = writePng "ex01.png" (drawPicture 3 pic)
