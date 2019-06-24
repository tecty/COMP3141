module Art where  

import ShapeGraphics
import Codec.Picture


art :: Picture
art = snow 3 1.5 

-- art = [snowPiece (Point 400 400) 0 1 1 white]

snow :: Int -> Float -> Picture 
snow depth size = concat $ map (\x-> snowPieceRec (Point 400 400) x size 1 myBlue depth)  [0.0 , 2/8*pi ..14/8*pi]
-- snow depth = concat $ map (\x-> snowPieceRec (Point 400 400) x 1 1 myBlue depth)  [0.0]
    where
      myBlue  = Colour 168 244 255 255
      myWhite = Colour 255 255 255 128
      -- vec = Vector 1 1
      -- rotateVec = map (\x-> rotateVector x vec)  [0.0 , 1/4*pi ..2*pi]

      snowPiece :: Point -> Float -> Float ->  Float -> Colour -> PictureObject
      snowPiece pos angle size length color
        = Polygon points color Solid SolidFill
          where 
            vec1 = scaleVector size $ rotateVector angle  $ Vector ((-30.0 )*length)  (-10.0) 
            vec2 = scaleVector size $ rotateVector angle  $ Vector ((-100.0)*length)  (0.0)   
            vec3 = scaleVector size $ rotateVector angle  $ Vector ((-30.0 )*length)  (10.0)  

            -- points = [(Point 400 400),(Point 375 375),(Point 300 400),(Point 375 425)]
            points = [
                pos,
                movePoint vec1 pos,
                movePoint vec2 pos,
                movePoint vec3 pos
              ]

      toWhite:: Colour  -> Colour
      toWhite (Colour r g b y) = 
        Colour (min (r + (( 255 - 168) `div` depth )) 255)
          (min (g + (( 255 - 244) `div` depth ))  255)
          255
          (max (y - ((255 - 178) `div` depth)) 178)
          -- (max (255 - (255 - 128) * n `div` depth) 128)
      movePointByAngleAndSize :: Point -> Float -> Float -> Point
      movePointByAngleAndSize pos angle size 
        =  movePoint (scaleVector size $ rotateVector angle $ Vector (-100.0)  (0.0)) pos
      snowPieceRec :: Point -> Float -> Float -> Float -> Colour -> Int  -> Picture
      snowPieceRec pos angle size length color n
        | n <= 1 = [snowPiece pos angle (2.5 * size) (0.75 * length) (toWhite color)]
        -- grew up 
        | n `mod` 2 ==0  = [ 
            snowPiece pos angle (size * 0.5) (length * 2) (toWhite color) ,
            -- flower on edge 
            snowPiece (
                movePointByAngleAndSize pos (angle) (size * 0.95)
              ) (angle + 2/7*pi) (size * 0.5) (length * 1.35) (toWhite color) ,
            snowPiece (
                movePointByAngleAndSize pos (angle) (size * 0.95) 
              ) (angle - 2/7*pi) (size * 0.5) (length *1.35) (toWhite color)
          ] ++ snowPieceRec (
              movePointByAngleAndSize pos angle size
            ) angle (size * 0.5) length color (n - 1)
        | n `mod` 2 ==1  = [ 
            snowPiece pos angle (size * 0.5) (length * 2) color 
          ] ++ 
          -- flower on edge 
          snowPieceRec (
              movePointByAngleAndSize pos (angle) (size * 0.95)
            ) (angle + 2/7*pi) (size * 0.4) (length ) color (n - 1) ++
          snowPieceRec (
              movePointByAngleAndSize pos (angle) (size * 0.95) 
            ) (angle - 2/7*pi) (size * 0.4) (length ) color (n - 1) ++
          snowPieceRec (
              movePointByAngleAndSize pos (angle) (size * 0.60)
            ) (angle + 4/9*pi) (size * 0.25) (length *0.5 ) color (n - 3) ++
          snowPieceRec (
              movePointByAngleAndSize pos (angle) (size * 0.60) 
          ) (angle - 4/9*pi) (size * 0.25) (length *0.5 ) color (n - 3) ++
            
          snowPieceRec (
            movePointByAngleAndSize pos angle size
          ) angle (size * 0.5) length color (n - 1)
  
-- art = fracTree 15 100 10 -- replace with something else

-- fracTree :: Float -> Float -> Int -> Picture
-- fracTree width height n
--   = fTree (Point  (400 - width / 2) 800) (Vector 0 (-height))
--                   (Vector width 0) red n
--   where
    
--     toBlue :: Colour -> Colour
--     toBlue (Colour r g b o) = 
--       Colour (max 0 (r - 15)) g (min 255 (b + 15)) o
--     angle = pi/8
--     fTree :: Point -> Vector -> Vector -> Colour -> Int -> Picture
--     fTree pos vec1 vec2 col n
--       | n <= 1 = [Polygon [pos, movePoint vec1 pos, 
--                                 movePoint vec2 $ movePoint vec1 pos, 
--                                 movePoint vec2 pos]
--                           col
--                           Solid
--                           SolidFill]
                          
--       | otherwise = fTree pos vec1 vec2 col (n - 1) ++
--                     fTree (movePoint vec1 pos) 
--                           (scaleVector 0.8 $ rotateVector (0.5 * angle) vec1)
--                           (scaleVector 0.8 $ rotateVector (0.5 * angle) vec2) 
--                           (toBlue col) (n - 1) ++
--                     fTree (movePoint vec1 pos) 
--                           (scaleVector 0.8 $ rotateVector (-angle) vec1)
--                           (scaleVector 0.8 $ rotateVector (-angle) vec2) 
--                           (toBlue col) (n - 1) 

      
scaleVector :: Float -> Vector -> Vector
scaleVector fac (Vector x y)
  = Vector (fac * x) (fac * y)                           
 
rotateVector :: Float -> Vector -> Vector
rotateVector alpha (Vector vx vy)
  = Vector (cos alpha * vx - sin alpha * vy)
           (sin alpha * vx + cos alpha * vy)

movePoint :: Vector -> Point -> Point
movePoint (Vector xv yv) (Point xp yp)
  = Point (xv + xp) (yv + yp)

-- use 'writeToFile' to write a picture to file "ex01.png" to test your
-- program if you are not using Haskell for Mac
-- e.g., call
-- writeToFile [house, door]

writeToFile pic
  = writePng "art.png" (drawPicture 3 art)
