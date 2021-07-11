--Ceng242 THE1--
data Cell = H | P | O | X deriving (Show,Eq)
data Result = Fail | Caught (Int,Int) deriving (Show,Eq)
data Direction = N | S | E | W deriving (Show,Eq)
    
simulate :: [[Cell]] -> [(Direction, Direction)] -> Result
-- DO NOT CHANGE ABOVE THIS LINE, WRITE YOUR CODE BELOW --
simulate _ [] = Fail
simulate board directions
   | isH == isP = Caught isH
   | otherwise = simulate newB  (tail(directions))
   where newB     = updatedBoard board newOrder  
         newOrder = changeTheOrder flatten board 0 0
         flatten  = flatUpdate board clean (head(directions)) clean
         clean    = clearBoard board
         isH      = nextLoc (currH board clean) (fst(head(directions))) (board) (clean) 
         isP      = nextLoc (currP board clean) (snd(head(directions))) (board) (clean)
         clearBoard board = [(a,b) | a <- [0..x], b <- [0..y]]
            where x = x_axis board
                  y = y_axis board
         x_axis (b:bs) = length (b) -1 
         y_axis (b:bs) = length (b:bs) -1
         currH b (c:cs) 
            | b !! snd(c) !! fst(c) == H = c 
            | otherwise = currH b (cs)
         currP b (c:cs)
            | b !! snd(c) !! fst(c) == P = c
            | otherwise = currP b (cs)
         obst _ [] = []
         obst b (c:cs) 
            | b !! snd(c) !! fst(c) == X = [c] ++ (obst (b) (cs))  
            | otherwise = obst (b) (cs)
         nextLoc (x,y) compass  (b:bs) clear
            | (compass == W && x >= 1 && not((x-1,y) `elem` engel))              = (x-1,y)       
            | (compass == E && x < (x_axis (b:bs)) && not((x+1,y) `elem` engel)) = (x+1,y)
            | (compass == S && y < (y_axis (b:bs)) && not((x,y+1) `elem` engel)) = (x,y+1)
            | (compass == N && y >= 1 && not((x,y-1) `elem` engel))              = (x,y-1)
            | otherwise                                                          = (x,y)
            where engel = obst (b:bs) clear
         flatUpdate _ [] _ _= []
         flatUpdate (b:bs) (c:cs) (x,y) (dc:dcs)
            | locH == (c) = H : flatUpdate (b:bs) (cs) (x,y) (dc:dcs)  
            | locP == (c) = P : flatUpdate (b:bs) (cs) (x,y) (dc:dcs)
            | ((b:bs) !! snd(c) !! fst(c)) == X = X : (flatUpdate (b:bs) (cs) (x,y) (dc:dcs))
            | otherwise = O : flatUpdate (b:bs) (cs) (x,y) (dc:dcs)
            where locH = nextLoc (currH (b:bs) (dc:dcs)) x (b:bs) (dc:dcs)
                  locP = nextLoc (currP (b:bs) (dc:dcs)) y (b:bs) (dc:dcs)
         changeTheOrder unordered board row column 
            | (column == x && row == y) =  [unordered !! ((column * (y+1)) + row)]
            | (column == x) = unordered !! (column * (y+1) + row): changeTheOrder unordered board (row+1) 0
            | otherwise = unordered !! (column * (y+1) + row) :changeTheOrder unordered board row (column+1)  
            where x = x_axis board
                  y = y_axis board
         updatedBoard _ [] = []
         updatedBoard board flatB
            | length flatB == 1 = [take (x+1) flatB]   
            | otherwise = [take (x+1) flatB] ++ updatedBoard (board) (drop (x+1) flatB)
            where x = x_axis board 
