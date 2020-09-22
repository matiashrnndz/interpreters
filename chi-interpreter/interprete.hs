module interprete where
import Prelude

{-- Autores
Matias Herández
Gianfranco Drago
--}

-- Sintáxis Chi
type Id = String

data Exp = V Id
         | K Id
         | Lam [Id] Exp
         | Exp :. [Exp]
         | Case Exp [Branch]
         | Rec Id Exp
 deriving (Show)
 
-- Branch del case
type Branch = (Id, ([Id], Exp))

-- Sustituciónes múltiples
type Subst = [(Id, Exp)]

-- POS : Retorna la expresión asociada a x en s, o (V x) si x no se encuentra en s
lkup :: Subst -> Id -> Exp
lkup subst id = case lookup id subst of {
                     Just exp -> exp;
                     Nothing -> V id
                }

-- POS : Retorna la sustitución s con las identificadoras [x] dadas de baja.
(-.) :: Subst -> [Id] -> Subst
subst -. ids = filter (\(id,_) -> not(elem id ids)) subst

-- Sustitución sobre expresiones
(<-.) :: Exp -> Subst -> Exp         
(V id) <-. subst = lkup subst id
(K id) <-. subst = K id
(Lam ids exp) <-. subst = (Lam ids (exp <-. (subst -. ids)))
(exp :. exps) <-. subst = (exp <-. subst) :. (map (\x -> (x <-. subst)) exps)
(Case exp branches) <-. subst = Case (exp <-. subst) (map (\branch -> (branch <-* subst)) branches)
(Rec id exp) <-. subst = Rec id (exp <-. (subst -. [id]))

-- Sustitución sobre ramas
(<-*) :: Branch -> Subst -> Branch
(c, (xs, exp)) <-* subst = (c, (xs, exp <-. (subst -. xs)))

-- Evaluación débil
evalDebil :: Exp -> Exp
evalDebil (V id) = error("Variable " ++ id ++ " no ligada.")
evalDebil (K id) = (K id):.[]
evalDebil (Lam ids exp) = Lam ids exp
evalDebil (exp :. exps) = case evalDebil exp of {
                               Lam ids exp' -> case (length ids == length exps) of {
                                                    True -> evalDebil (exp' <-. (zip ids exps));                                  
                                                    False -> error("No tienen el mismo largo las dos listas.")
                                               };
                               (K id) :. exps' -> (K id) :. (exps' ++ exps)
                          };       
evalDebil (Case exp branches) = case evalDebil exp of {
                                     (K id):. exps -> case lookup id branches of { 
                                                           Just (ids, exp') -> case (length ids == length exps) of {
                                                                                    True -> evalDebil (exp' <-. (zip ids exps));
                                                                                    False -> error("No tienen el mismo largo las dos listas.")
                                                                               };
                                                           Nothing -> error("El constructor no fue considerado en la lista de ramas.")
                                                      };
                                     _ -> error("No es un constructor.")
                                };
evalDebil (Rec id exp) = evalDebil (exp <-. [(id, Rec id exp)])

-- Evaluación fuerte
evalFuerte :: Exp -> Exp
evalFuerte exp = case evalDebil exp of {
                      Lam ids exp' -> Lam ids exp';
                      (K id) :. exps -> (K id) :. map evalFuerte exps
                 }

-- Sintaxis N
data N = O | S N
 deriving (Show, Eq, Ord)






-- Ejercicios --

-- Función Neg
hNeg :: Bool -> Bool
hNeg = \b -> case b of {
                  True -> False;
                  False -> True
             }

cNeg :: Exp
cNeg = Lam ["b"] (Case (V "b") [
                       ("True", ([], K "False")),
                       ("False", ([], K "True"))
                  ])

-- Función Par
hPar :: N -> Bool
hPar = \n -> case n of {
                  O -> True;
                  S x -> not (hPar x)
             }

cPar :: Exp
cPar = Rec ("Par")((Lam["n"]) (Case (V "n") [
                                               ("O", ([], K "True")),
                                               ("S", (["x"], cNeg:.[V "Par":.[V "x"]]))
                               ]))

-- Función Map
hMap :: (a -> b) -> [a] -> [b]
hMap = \f -> \l -> case l of {
                        [] -> [];
                        (x:xs) -> (f x) : (hMap f xs)
                   }

cMap :: Exp
cMap = Rec ("cMap")((Lam["f", "l"]) (Case (V "l")[
                                         ("[]", ([], K "[]")),
                                         ("<>", (["x", "xs"], K "<>":.[V "f":.[V "x"], V "cMap":.[V "f", V "xs"]]))       
                                   ]))

-- Función Suma
hSuma :: N -> N -> N
hSuma = \n -> \m -> case n of {
                         O -> m;
                         S x -> S (hSuma x m)
                    }

cSuma :: Exp
cSuma = Rec "cSuma" (Lam ["n", "m"] (Case (V "n") [
                                          ("O", ([], V "m")),
                                          ("S", (["x"], K "S" :. [V "cSuma" :. [V "x", V "m"]]))
                                    ]))

 -- Función Length
hLength :: [a]-> N
hLength = \l -> case l of {
                     [] -> O;
                     x:xs -> S (hLength xs)
                }

cLength :: Exp
cLength =  Rec "cLength" (Lam["l"] (Case (V "l")[
                                         ("[]",([], K "O")),
                                         ("<>", (["x","xs"], K "S" :. [V "cLength" :. [V "xs"]]))
                                    ]))
