module Minilogo where

import           Data.List

import           Prelude   hiding (Num)

-- Team Member
-- ID: 933620525 Name: Yeojin Kim
-- ID: 933612315 Name: Kuan-Lun Tseng
-- ID: 933661736 Name: Xiangtian Xue
-- ID: 932545221 Name: Hunter Lannon
-- ID: 931932335 Name: Curt Haldorson



----------------------------------1---------------------------------
----------------------------------a---------------------------------

{-
cmd  :: = pen mode
	    | moveto (pos,pos)
	    | def name ( pars ) cmd
	    | call name ( vals )
	    | cmd; cmd
mode :: = up | down
pos  :: = num | name
pars :: = name, pars | name
vals :: = num, vals | num
-}

type Num = Int
type Name = String

data Mode = Up | Down deriving Show

data Pos = PosNum Num | PosStr Name deriving Show

data Pars = ParsAnd Name Pars
		  | ParsStr Name
			deriving Show

data Vals = ValsAnd Num Vals
		  | ValsNum Num
			deriving Show

data Cmd = Pen Mode
		 | Moveto (Pos ,Pos)
		 | Def Name (Pars) Cmd
		 | Call Name (Vals)
		 | And Cmd Cmd
		 | Noop
		   deriving Show

----------------------------------1---------------------------------
----------------------------------b---------------------------------
--Vector in abstract syntax

vector = Def "vector" (ParsAnd "x1" (ParsAnd "y1" (ParsAnd "x2" (ParsStr "y2"))))
					  (And (And (Moveto (PosStr "x1", PosStr "y1")) (Pen Down))
						   (And (Moveto (PosStr "x2", PosStr "y2")) (Pen Up)))


----------------------------------1---------------------------------
----------------------------------c---------------------------------
steps :: Num -> Cmd
steps 0 = Noop
steps x = And (stairs(x))(And (Moveto (PosNum x, PosNum x)) (Pen Up))

stairs :: Num -> Cmd
stairs 0 = (Pen Down)
stairs x = And(stairs(x-1))(And(Moveto(PosNum (x-1),PosNum (x-1)))(Moveto(PosNum (x-1),PosNum x)))

----------------------------------2---------------------------------
----------------------------------a---------------------------------
{-
circuit :: = gates links
gates   :: = num:gateFn ; gates | ϵ
gateFn  :: = and | or | xor | not
links   :: = from num.num to num.num; links | ϵ
-}
{-
data Circuit = Circuit Gates Links
			   deriving Show

data Gates = Gates Num GateFn Gates
		   | GatesNULL
		     deriving Show

data GateFn = And
			| Or
			| Xor
			| Not
			  deriving Show

data Links = From (Num, Num) (Num, Num) Links
		   | LinksNULL
		     deriving Show

halfadder = Circuit
			(Gates 1 Xor (Gates 2 And GatesNULL))
			(From (1, 1) (2, 1) (From (1, 2) (2, 2) LinksNULL))


ppGateFn :: GateFn -> Name
ppGateFn And 	= ":and;\n"
ppGateFn Or		= ":or;\n"
ppGateFn Xor	= ":xor;\n"
ppGateFn Not	= ":not;\n"
-}
{-
ppGates :: Gates -> Name
ppGates Noop  = ""
ppGates n x = n++ppGateFn x
-}

data Circuit = Circuits Gates Links
                deriving Show

type Gate = (Int, GateFn)

type Gates = [Gate]

data GateFn = AND
            | OR
            | XOR
            | NOT
            deriving Show

type Link = (Int, Int, Int, Int)

type Links =  [Link]

----------------------------------2---------------------------------
----------------------------------b---------------------------------

halfAdder =  Circuits [(1, XOR), (2, AND)] [(1,1,2,1), (1,2,2,2)]

----------------------------------2---------------------------------
----------------------------------c---------------------------------

{-
ppNoun :: Noun -> String
ppNoun Dogs  = "dogs"
ppNoun Teeth = "teeth"

ppVerb :: Verb -> String
ppVerb Have = "have"

ppSent :: Sentence -> String
ppSent (Phrase n v n') = ppNoun n++" "++ppVerb v++" "++ppNoun n'
ppSent (And s s')      = ppSent s++" and\n "++ppSent s'
-}

ppCircuit :: Circuit -> String
ppCircuit (Circuits x y) = ppGates x++ppLinks y


ppGates :: Gates -> String
ppGates []     = ""
ppGates (x:xs) = ppGatesHelper x  ++ ppGates xs


ppGatesHelper :: Gate -> String
ppGatesHelper (x, y) = ppItoS x++ppGateFn y


ppGateFn :: GateFn -> String
ppGateFn AND = ":and;\n"
ppGateFn OR  =  ":or;\n"
ppGateFn XOR = ":xor;\n"
ppGateFn NOT = ":not;\n"


ppLinks :: Links -> String
ppLinks []     = ""
ppLinks (x:xs) = ppLinksHelper x ++ ppLinks xs


ppLinksHelper :: Link -> String
ppLinksHelper (x1, y1, x2, y2) = "From "++ppItoS x1++"."++ppItoS y1++" to "++ppItoS x2++"."++ppItoS y2++";\n"

ppItoS :: Int -> String
ppItoS 0 = "0"
ppItoS 1 = "1"
ppItoS 2 = "2"
ppItoS 3 = "3"
ppItoS 4 = "4"
ppItoS 5 = "5"
ppItoS 6 = "6"
ppItoS 7 = "7"
ppItoS 8 = "8"
ppItoS 9 = "9"

----------------------------------3---------------------------------
----------------------------------a---------------------------------

data Expr = N Int
		  | Plus  Expr Expr
		  | Times Expr Expr
		  | Neg	  Expr
		    deriving Show

data Op = Add
		| Multiply
		| Negate
		  deriving Show

data Exp = Num Int
		 | Apply Op [Exp]
		   deriving Show

abstractSyntax = Times ( Neg (Plus (N 3) (N 4))) (N 7)

alternativeAbstractSyntax = Apply Multiply[Apply Negate[Apply Add[Num 3,Num 4]],Num 7]

----------------------------------3---------------------------------
----------------------------------b---------------------------------

-- Answer
-- First abstract syntax symbols follows a pattern match, uniquely identifying rules and giving us clear image to understand the grammar.
-- In alternative syntax, every internal node contains rule names "Apply" here. However, alternative syntax brings us convenience for reading (more closer to natural language expression), although it comes with a lengthy syntax penalty.

----------------------------------3---------------------------------
----------------------------------c---------------------------------

translate :: Expr -> Exp
translate (N x)       = Num x
translate (Plus x y)  = Apply Add [translate x,translate y]
translate (Times x y) = Apply Multiply [translate x,translate y]
translate (Neg x)     = Apply Negate [translate x]
