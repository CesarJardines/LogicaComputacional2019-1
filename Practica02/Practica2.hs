module Practica2 where
import LProp
import Data.List

--Jardines Mendoza César Eduardo 

-- Alias de tipos: Sólo sirve para referirnos
-- a un tipo existente por un nuevo nombre, 
-- pero podemos seguir manipulándolo como
-- al tipo subyacente.
type Interpretacion = [(Char, Bool)] -- El tipo Interpretacion representa
                                     -- una asignación de variables proposicionales.

-- Por ejemplo, [('A', True), ('B', False)]
-- asigna la variable A a verdadero y B a falso.


-- interpretaciones toma una lista de nombres de variables
-- proposicionales y regresa una lista de todas las posibles
-- interpretaciones para esas variables.
-- Nótese que si la lista de entrada es de longitud n,
-- la lista de salida tendrá longitud 2^n.
interpretaciones :: [Char] -> [Interpretacion]
interpretaciones [] = [[]]
interpretaciones (c:cs)  = let ints = interpretaciones cs in
                               [(c, True):xs | xs <- ints] ++
                               [(c, False):xs | xs <- ints]
                              -- ^ listas por comprensión.


-- busca :: Prop ->Interpretacion ->Bool funcion que recibe una
--variable proposicional (PVar) y una interpretacion y busca la asigna-
--cion de verdad de la variable en la interpretacion.
-- >>> busca (PVar 'A') [('B', True), ('A', False)] ==> False
busca :: Prop -> Interpretacion -> Bool
busca _ [] = error "No se encontró una asignación en la interpretación dada."
busca (PVar vp) ((p,b):xs) = if((PVar vp) == (PVar p)) then b else busca (PVar vp) xs
--          ^
--          L_ nótese que aquí estamos des-estructurando una tupla al mismo 
--         tiempo que la lista.


-- eval evalúa una fórmula proposicional según una 
-- interpretación dada.
eval :: Prop -> Interpretacion -> Bool
eval PTrue _ = True
eval PFalse _ = False
eval (PNeg (PVar vp)) list = busca (PVar vp) list
eval (POr (PVar vp1) (PVar vp2)) list = if(((busca (PVar vp1) list) == False) && ((busca (PVar vp2) list)) == False) 
																						then False 
																						else True
eval (PAnd (PVar vp1) (PVar vp2)) list = if(((busca (PVar vp1) list) == True) && ((busca (PVar vp2) list)) == True) 
																						then True 
																						else False
eval (PImpl (PVar vp1) (PVar vp2)) list = if(((busca (PVar vp1) list) == True) && ((busca (PVar vp2) list)) == False) 
																						then False 
																						else True
eval (PEquiv (PVar vp1) (PVar vp2)) list = if((busca (PVar vp1) list) == (busca (PVar vp2) list)) 
																						then True 
																						else False

-- satisfacible toma una fórmula proposicional
-- y devuelve si es satisfacible, esto es, existe
-- una interpretación con la que se evalúa a True.
-- Este es un problema NP-Completo, pero esto nos 
-- importa muy poco, lo hacemos por fuerza bruta 
-- como se debe.
satisfacible :: Prop -> Bool
satisfacible (PVar pr) = True
satisfacible (PNeg p) = not(satisfacible p)
satisfacible (POr pr1 pr2) = satisfacible pr1 || satisfacible pr2
satisfacible (PAnd pr1 pr2) = satisfacible pr1 && satisfacible pr2
satisfacible (PImpl pr1 pr2) = not(satisfacible pr1) || satisfacible pr2
satisfacible (PEquiv pr1 pr2) = (not(satisfacible pr1) || satisfacible pr2) && (not(satisfacible pr2) || satisfacible pr1)










