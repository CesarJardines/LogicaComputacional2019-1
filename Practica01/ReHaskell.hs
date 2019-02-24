-- @author Jardines Mendoza César Eduardo --
-- @version 2.0 --
-- Logica Computacional 2019-1 --
-- Practica 1  Facultad de Ciencias UNAM--

--1.- Remove --
-- >rm 2[1,2,3] => [1,3] --
remove :: Eq a => a -> [a] -> [a]
remove a [] = []
remove a (x:xs) = if(a /= x) then [x] ++ remove a xs else remove a xs

--(\\)--
-- >[1,2,3,4,5]\\[3,2,1] => [4,5] --

(\\) :: Eq a => [a] -> [a] -> [a]
(\\) (x:ls) [] = (x:ls)
(\\) (x:ls) (y:ys) = if (elem y (x:ls)) then (\\) (remove y (x:ls)) ys else (\\) (x:ls) ys 

-- rmdup --
-- >rmdup [3,2,1,1,2,2,4,5,3,2,1,6] => [3,2,1,4,5,6] --
rmdup :: Eq a =>[a] ->[a]
rmdup [] = []
rmdup (x:xs) = if(notElem x xs) then [x]++rmdup xs else rmdup xs

--Se define el tipo de dato Tree--
data Tree a = Empty
            | Node a (Tree a) (Tree a)
            deriving (Show, Eq)

--insert--
-- >insert 0 Empty
--Node 0 Empty Empty
-- >insert 1 (Node 0 Empty Empty)
--Node 0 (Node 1 Empty Empty) Empty
-- >insert -1 (insert 1 (insert 0 Empty))
--Node 0 (Node 1 Empty Empty) (Node -1 Empty Empty)
insert :: Ord a =>a ->Tree a ->Tree a
insert a (Empty) = Node a (Empty) (Empty)
insert n (Node a t1 t2) = if(n < a) then Node a (insert n t1) t2 else Node a t1 (insert n t2)

--FromList--
-- >fromList [0,-1,1] => Node 0 (Node 1 Empty Empty) (Node -1 Empty Empty)
fromList :: Ord a =>[a] ->Tree a
fromList [] = Empty
fromList (x:xs) = fromListAux (x:xs) Empty

-- fromlistAux --
fromListAux :: Ord a => [a] -> Tree a -> Tree a
fromListAux [] t = t
fromListAux (x:xs) t = fromListAux (xs) (insert x t)

-- inOrder --
-- >inOrder (Node 2 (Node 1 (Node 0 Empty Empty) Empty) (Node 3 Empty Empty)) => [0, 1, 2, 3]
inOrder :: Tree a ->[a]
inOrder Empty = []
inOrder (Node a t1 t2) = inOrder t1 ++ [a] ++  inOrder t2

-- Sort  --
-- >sort [5,4,10,21,-3,0,-15] => [-15,-3,0,4,5,10,21]
sort :: (Ord a) => [a] -> [a]
sort [] = []
sort (x:xs) = sort [y | y <- xs, y <= x] ++ [x] ++ sort [y | y <- xs, y > x]