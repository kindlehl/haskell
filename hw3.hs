data Cmd3 = Pen Mode
		 | Moveto Int Int
		 | Seq Cmd3 Cmd3
		   deriving Show

data Mode = Up
		  | Down
		    deriving (Eq, Show)

type State = (Mode,Int,Int)
type Line = (Int, Int, Int, Int)
type Lines = [Line]

semS :: Cmd3 -> State -> (State, Lines)
semS (Pen mode)   	(ms, x, y) 	= ((mode, x, y),[])
semS (Moveto xd yd) (ms, x, y) 	| ms == Up 		= ((Up  , xd, yd)	, [])
								| ms == Down 	= ((Down, xd, yd)	, [(x, y, xd, yd)])

semS (Seq c1 c2)	(ms, x, y) 	= {-State-} (fst(semS c2 ( fst ( semS c1 (ms, x, y) ) )), 
								  {-Lines-}	snd ( semS c1 (ms, x, y) ) ++ snd (semS c2 ( fst ( semS c1 (ms, x, y) ) )))

sem' :: Cmd3 -> Lines
sem' c = snd ( semS c (Up, 0, 0) )

d1 = semS (Pen Down) (Up, 0, 0)

d2 = semS (Seq	(Seq (Moveto 0 0) (Moveto 1 1) )
				(Seq (Moveto 2 2) (Moveto 3 3) )) (Down, 9, 9)
d3 = Seq (Pen Down) (Moveto 1 9)

d4 = Seq (Pen Up) (Moveto 1 9)

d5 = Seq (Seq (Pen Down) (Moveto 1 1))
		 (Seq (Pen Down) (Moveto 2 2))
		 
d6 = Seq(Seq(Seq((Seq (Pen Down)(Moveto 1 1))) (Moveto 3 3)) (Moveto 5 5))(Moveto 7 7)

ltest2 = Pen Down `Seq` Moveto 1 1 `Seq` Moveto 3 5
ftest2 = sem' ltest2 -- [(0, 0, 1, 1), (1, 1, 3, 5)]

