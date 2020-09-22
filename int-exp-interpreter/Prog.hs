-- Autor(es): Gianfranco Drago - Matias Hernández

{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}

module Prog where

-- Tipos
type Var = String

data Exp where {
    V    :: Var -> Exp;
    N  :: Int -> Exp;
    (:+)  :: Exp -> Exp -> Exp;
    (:-)  :: Exp -> Exp -> Exp;
    (:*) :: Exp -> Exp -> Exp  }
  deriving(Eq,Show)

-- Se define que :* tiene mayor precedencia que :+ y :-
infixl 6 :+
infixl 6 :-
infixl 8 :*

type Memoria = [(Var,Int)]

data Prog where {
    (:=)  :: Var -> Exp -> Prog;
    (:>)  :: Prog -> Prog -> Prog;
    If    :: Exp -> Prog -> Prog -> Prog;
    While :: Exp -> Prog -> Prog  }
  deriving(Eq,Show)

-- Se define que := tiene mayor precedencia que :>
infixr 5 :=
infixr 3 :>

-- 1
(@@) :: Var -> Memoria -> Int
(@@) = \v -> \m -> 	case m of { [] -> error "Error";
						x:xs -> case x of { 
									(a,b) -> case (v == a) of { 
												True -> b;
												False -> (v@@xs);
												};
									};
						};

-- 2
upd :: (Var,Int) -> Memoria -> Memoria
upd = \n -> \m -> 	case n of {
					   (a,b) -> n:(quitarElementoMemoria a m);
					}; 

-- Quita la variable coincidente en la memoria 

quitarElementoMemoria :: Var -> Memoria -> Memoria
quitarElementoMemoria = \v -> \m -> case m of { 
										[] -> [];
										x:xs -> case x of { 
													(a,b) -> case (v == a) of { 
																	True -> xs;
																   	False -> x:(quitarElementoMemoria v xs);
															 }; 
												};
									};

-- 3
eval :: Exp -> Memoria -> Int
eval = \e -> \m -> 	case e of { 
						V v -> v@@m;
						N n -> n;
						(e1 :+ e2) -> (eval e1 m) + (eval e2 m);
						(e1 :- e2) -> (eval e1 m) - (eval e2 m);
						(e1 :* e2) -> (eval e1 m) * (eval e2 m);
					};

-- 4
run :: Prog -> Memoria -> Memoria
run = \p -> \m -> 	case p of { 
						(v := e) -> upd (v, eval e m) m;
						(p1 :> p2) -> run p2 (run p1 m);
						If e p1 p2 -> 	case eval e m of { 
											0 -> run p2 m;
											_ -> run p1 m;
										};
						While e p1 -> 	case eval e m of {
											0 -> m;
											_ -> run p (run p1 m);
										};
					};

-- Ejemplos
p0 :: Prog
p0 = "x" := N 1 :> "x" :=  V "x" :+ N 10

p1 :: Prog
p1 =  "x" := N 1 :> "y" := N 2 :>
      If  (V "y" :- V "x")
      ("z" := N 10)
      ("z" := N 20)

p2 :: Prog
p2 = "x" := N 10 :> "y" := N 5 :> 
     While (V "x") ( "y" := V "y" :+ N 2 :> "x" :=  V "x" :-N 1)

-- 5
swap:: Prog
swap = "auxSwap" := V "x" :>
	   "x" := V "y" :> 
	   "y" := V "auxSwap"

-- 6
fact :: Int -> Prog
fact = \n -> case n of {
				0 -> "fact" := N 1; 
				_ -> case n < 0 of {
						False -> ("nat" := N n :>
									"fact" := N 1 :>
									While (V "nat")
									(
										"fact" := V "fact" :* V "nat" :>
										"nat" := V "nat" :- N 1
									));                 
						True -> error "Número negativo"
					};
			};
					   

{-- Si n es negativo, la función quedaría en loop, ya que en la condición del while nunca llega a 
cero, pero en este caso no queda en loop, devuelve una excepción de que el número negativo.
--}
