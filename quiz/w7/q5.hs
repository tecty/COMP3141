data Direction = L | R
forward    :: IO ()
forward = error ""
obstructed :: IO Bool
obstructed = error ""

turn       :: Direction -> IO ()
turn = error ""

robot = do
    sensed <- obstructed
    if sensed 
      then turn L
      else forward
    robot