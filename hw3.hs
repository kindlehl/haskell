import Data.List

---------------------------------- Ex 1 ------------------------------
data Cmd = LD Int
         | ADD
         | MULT
         | DUP
         | DEF String Prog
         | CALL String
         deriving Show


type Stack = [Int]
type D = Maybe Stack -> Maybe Stack

semCmd :: Cmd -> D
semCmd (LD e)  s1  = case s1 of  
                    Just xs -> Just (e:xs)
                    _ -> Nothing
semCmd (ADD)  s1   = case s1 of  
                    Just (e:e':xs) -> Just([e + e'] ++ xs)
                    _ -> Nothing
semCmd (MULT)  s1  = case s1 of  
                    Just (e:e':xs) -> Just([e * e'] ++ xs)
                    _ -> Nothing
semCmd (DUP)  s1  = case s1 of  
                    Just (e:xs) -> Just(e:e:xs)
                    _ -> Nothing

type Prog = [Cmd]
sem :: Prog -> D
sem [] s1 = s1
sem (x:xs) s1 = sem xs (semCmd x s1)

test1 = [LD 3, DUP, ADD, DUP, MULT]
test2 = [LD 3, ADD]
test3 = []

---------------------------------- Ex 2 ------------------------------

---------------------------------- 2-a --------------------------------
-- Added in Exercise 1

---------------------------------- 2-b --------------------------------

type Macros = [(String, Prog)]
type State = Maybe (Stack, Macros) -> Maybe (Stack, Macros)

semCmd2 :: Cmd -> State
semCmd2 (LD x) commandseq =  case commandseq of  
                    Just (xs, m) -> Just (x:xs, m)
                    _ -> Nothing
semCmd2 (ADD) commandseq =  case commandseq of  
                    Just (x:x':xs, m) -> Just([x + x'] ++ xs, m)
                    _ -> Nothing
semCmd2 (MULT) commandseq  = case commandseq of  
                    Just (x:x':xs, m) -> Just([x * x'] ++ xs, m)
                    _ -> Nothing
semCmd2 (DUP) commandseq = case commandseq of  
                    Just (x:xs, m) -> Just(x:x:xs, m)
                    _ -> Nothing
semCmd2 (DEF macroname prog) commandseq = case commandseq of
                            Just (xs, m) -> Just(xs, (macroname,prog):m)
                            _ -> Nothing
semCmd2 (CALL macroname) commandseq = case commandseq of
                            Just (xs, m) -> 
                                case findDef macroname  m of
                                    Just (n, prog)-> sem2 prog commandseq
                                    _->Nothing

findDef :: String -> Macros -> Maybe(String, Prog) 
findDef macroname commandseq = find (\c -> fst c == macroname) commandseq

---------------------------------- 2-c --------------------------------

sem2 :: Prog -> State
sem2 []     commandseq = commandseq
sem2 (x:xs) commandseq = sem2 xs (semCmd2 x commandseq) 



---------------------------------- Ex 3 ------------------------------
data Cmd3 = Pen Mode
		 | Moveto Int Int
		 | Seq Cmd3 Cmd3
		   deriving Show

data Mode = Up
		  | Down
		    deriving (Eq, Show)

type State2 = (Mode,Int,Int)
type Line = (Int, Int, Int, Int)
type Lines = [Line]

semS :: Cmd3 -> State2 -> (State2, Lines)
semS (Pen mode)   	(ms, x, y) 	= ((mode, x, y),[])
semS (Moveto xd yd) (ms, x, y) 	| ms == Up 		= ((Up  , xd, yd)	, [])
								| ms == Down 	= ((Down, xd, yd)	, [(x, y, xd, yd)])

semS (Seq c1 c2)	(ms, x, y) 	= {-State2-} (fst(semS c2 ( fst ( semS c1 (ms, x, y) ) )), 
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

