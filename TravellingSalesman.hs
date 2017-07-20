	coordinates :: [(Float,Float)]
	coordinates = [(0,1),(1,2),(2,3),(2,4),(0,6)]
	 
	distance :: (Float, Float) -> (Float, Float) -> Float
	distance (x1,y1) (x2,y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)
	 
	tourLength :: [(Float, Float)] -> [Int] -> Float
	tourLength pairs tour = let coords = map (pairs !!) tour
	  in sum $ zipWith distance coords (tail coords)
	 
	tourLength' :: [(Float, Float)] -> [Int] -> Float
	tourLength' pairs tour = tourLength pairs (tour ++ (head tour))
	 
	main :: IO ()
	main = print $ tourLength' coordinates [2,1,0,3,4]
