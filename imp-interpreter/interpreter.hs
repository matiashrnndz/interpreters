module Interpreter where
import Prelude
import Data.Maybe

{--------------- Autores ---------------}

-- Matias Herández
-- Gianfranco Drago

{--------------- ESPECIFICACIÓN IMP ---------------}

-- Sintáxis Imp

type Id = String

type Prog = [Inst]

type Branch = (Id, ([Id], Prog))

data Inst = Asig [(Id, Exp)]
          | Case Id [Branch]
          | While Id [Branch]
 deriving (Show)

data Exp = V Id
         | K Id [Exp]
 deriving (Show)

-- Semántica Operacional

data Val = KV Id [Val]

type Mem = [(Id, Val)]

lkup :: Mem -> Id -> Val
lkup mem id = case lookup id mem of {
                     Just val -> val;
                     Nothing -> error("Variable " ++ id ++ " no existe en memoria.")
              }

update :: Mem -> [(Id, Val)] -> Mem
update mem mem' = mem'++mem

-- Evaluación de expresiones

eval :: Mem -> Exp -> Val
eval mem (V id) = lkup mem id
eval mem (K id exps) = KV id (map (eval mem) exps)

-- Semántica de una paso de ejecución

step :: (Mem, Prog) -> (Mem, Prog)
step (mem, (inst:insts)) = case inst of {
                                Asig lst -> case unzip lst of {
                                                 (ids, exps) -> ((update mem (zip ids (map (eval mem) exps))), insts) 
                                            };
                                Case id bs -> case (lkup mem id) of {
                                                   KV c vs -> case lookup c bs of { 
                                                                   Just (xs, p) -> (update mem (zip xs vs), p++insts);
                                                                   Nothing -> error("Variable " ++ c ++ " no existe en memoria.")
                                                              }                                
                                              };
                                While id bs -> case (lkup mem id) of {
                                                    KV c vs -> case lookup c bs of {
                                                                    Just (xs, p) -> ((update mem (zip xs vs)), p++(inst:insts));
                                                                    Nothing -> (mem, (insts))
                                                               }
                                               }
                           }

-- Reglas de ejecución completa

total :: Prog -> Mem -> Mem
total [] mem = mem 
total p mem = case step (mem, p) of {
                   (mem', p') -> total p' mem' 
              }

{--------------- EJERCICIOS ---------------}

-- EJERCICIO 1 - Determinen si un natural dado es o no par

neg :: Prog
neg = [ 
        Case "b" 
                [("True", ([],
                        [Asig [("b", K "False" [])]])),
                ("False", ([],
                        [Asig [("b", K "True" [])]]))]
      ]

par :: Prog
par = [
        While "n"
                [("S", (["x"],
                        (Asig [("n", V "x")]) : neg))]
      ]

-- EJERCICIO 2 - Calcule la suma de dos naturales

suma :: Prog
suma = [
        While "n"
                [("S",(["x"],
                        [Asig [("n", V "x")],
                         Asig [("r", K "S" [V "r"])]]))]
       ]

-- EJERCICIO 3 - Calcule el largo de una lista de naturales

largo :: Prog
largo = [
        Asig [("l", K "O" [])],
        While "n"
                [(":",(["x","xs"],
                        [Asig [("n", V "xs")],
                         Asig [("l", K "S" [V "l"])]]))]
        ]

-- EJERCICIO 4 - Calcule si un natural n es igual a otro natural m

iguales :: Prog
iguales = [
           Asig [("b", K "True" [])],
           While "n"
               [("S",(["x"], 
                        [Case "m"
                                [("O",([],
                                        [Asig [("b", K "False" [])],
                                         Asig [("n", V "O")]])),
                                ("S",(["y"],
                                        [Asig [("n", V "x")],
                                         Asig [("m", V "y")]]))]]))], 
           Case "b" 
                [("True",([],
                        [Case "m"
                                [("O",([],
                                        [])),
                                ("S",(["x"],
                                        [Asig [("b", K "False" [])]]))]])),
                ("False",([],
                        []))]
          ]

-- EJERCICIO 5 - Calcule el n- ́esimo n ́umero de Fibonacci

fibonacci :: Prog
fibonacci = [
            Asig [("actual", K "O" [])],
            Asig [("proximo", K "S" [V "O"])],
            While "i"
                [("S", (["z"],
                        [Asig [("i", V "z")],
                         Asig [("n", V "actual")],
                         Asig [("r", V "proximo")]]
                         ++suma
                         ++[Asig [("actual", V "proximo")],
                         Asig [("proximo", V "r")]]))]
            ]

{--------------- PRUEBAS PARA LA ESPECIFICACIÓN ---------------}

instance Show Val where
 show (KV id vs) = id++" (" ++ concat (map show vs) ++ ")"

nat_to_val::Int -> Val
nat_to_val 0 = KV "O" []
nat_to_val n = KV "S" [nat_to_val (n-1)]

--Programa 1: programa que se cuelga porque no avanza en el while

fallido::Prog
fallido = [
          While ("n")[
                ("S",(["x"],[
                Asig [("n", K "S" [V "x"])]
                ]))
          ],
          Asig [("return",V "n")]
          ]

memFallido::Int -> Mem
memFallido n = [("n", nat_to_val n)]


pruebaFallido::Int -> Val
pruebaFallido n = lkup (total fallido (memFallido n)) "return"

--Programa 2: programa que se cuelga porque no avanza en el while y consume toda la memoria

overflow::Prog
overflow = [
                While ("n")[
                        ("S",(["x"],[
                                Asig [("n", K "S" [V "n"])]
                                ]))
                        ],
                Asig [("return",V "n")]
                ]

memOverflow::Int -> Mem
memOverflow n = [("n", nat_to_val n)]


pruebaOverflow::Int -> Val
pruebaOverflow n = lkup (total overflow (memOverflow n)) "return"


--Programa 3: programa que recorre un arbol y cuenta la cantidad de nodos que tiene el árbol

cantN::Prog
cantN = [
                Asig [("pend", K ":" [V "arb", K "[]" []]), ("ac", K "O" [])],
                While "pend" [
                        (":", (["x", "xs"], [Case "x" [
                                                ("V",([],[Asig [("pend", V "xs")]])),
                                                ("N",(["x","i","d"], [Asig [("pend", K ":" [V "i", K ":" [V "d", V "xs"]]),("ac", K "S" [V "ac"])]]))
                                ]]))
                ],
                Asig [("return", V "ac")]
        ]

data Arb a = Vacio | Nodo a (Arb a) (Arb a)

tree2Mem::Arb Int -> Val
tree2Mem Vacio = KV "V" []
tree2Mem (Nodo x i d) = KV "N" [nat_to_val x, tree2Mem i, tree2Mem d]

memCantN::Arb Int -> Mem
memCantN arb = [("arb", tree2Mem arb)]

pruebaCantN::Arb Int -> Val
pruebaCantN arb = lkup (total cantN (memCantN arb)) "return"

prueba1::Val 
prueba1 = pruebaCantN (Vacio)

prueba2::Val 
prueba2 = pruebaCantN (Nodo 1 Vacio Vacio)

prueba3::Val 
prueba3 = pruebaCantN (Nodo 1 (Nodo 2 (Nodo 3 (Nodo 4 Vacio Vacio) (Nodo 5 Vacio Vacio)) (Nodo 6 Vacio Vacio)) Vacio)