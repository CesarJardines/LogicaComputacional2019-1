%Nombre: Jardines Mendoza César Eduardo 
%Al realizar la practica pedi ayuda a algunos compañeros todo lo que esta acá fue producto del trabajo de todos
%Logica
%Practica 3

%Ejercicio 1
%Funcion que devuelve la reversa de una lista. 
reversa([],[]). 
reversa([X|L1], L) :- reversa(L1,Y), append(Y, [X], L). 

%Funcion que nos dice si una lista es palindromo, es decir, su propia reversa.
palindromo(X):- reversa(X, L), X==L.

%Ejercicio 2. Automata. Tranciones.
%Funciones para trancicionar entre dos estados.

%Tranciones para A.
transition(o,a,b).  
transition(z,a,a).
transition(v,a,c).

%Tranciones para B.
transition(v,b,c).
transition(o,b,b).
transition(z,b,a).

%Tranciones para C
transition(o,c,c).
transition(z,c,a).
transition(o,c,a).

%Automata. Process.
%Funcion para interpretar una cadena desde un estado y devolver otro estado.
process([],StateTransition,StateTransition).
process([X|LS],StateTransition,ResultTransition) :- transition(X,StateTransition,State), process(LS,State,ResultTransition).


%Ejercicio 3 
%protagonista(Cesar).
%Viuda esposa de Cesar (Daniela)
%Padre Cesar (Jose)
%hijo de Cesar y Daniela (Blanca) 
%hijo natural  de Cesar y Daniela(Luis).
%hijo natural de pepa y Blanca Fernanda.

%parejas
parejas(Cesar,Daniela).%principal
parejas(Jose,Blanca).%	Pareja del padre 

%padres

%relaciones de Cesar.
padre(Cesar,Blanca).
padre(Cesar,Jose).
padre(Cesar,Luis).

%relaciones de Jose.
padre(Jose,Cesar).
padre(Jose,Daniela).

%relaciones Daniela 
padre(Daniela,Luis).
padre(Daniela,Jose).
padre(Daniela,Blanca).

%relaciones de Blanca 
padre(Blanca,fernanda).
padre(Blanca,Cesar).


%Relaciones 
espareja(X,Y):- parejas(Y,X).
esHijo(X,Y):-padre(Y,X).
esPadre(X,Y):-padre(X,Y).
suegro(X,Y):- padre(X,Z),parejas(Y,Z).
yerno(X,Y):- suegro(Y,X).
abuelo(X,Y):-padre(Z,Y),padre(X,Z).
nieto(X,Y):- abuelo(Y,X).
hermano(X,Y):-padre(Z,X),padre(Z,Y),not(X=Y).
tio(X,Y):-hermano(X,Z),esHijo(Y,Z).
visabuelo(X,Y):-abuelo(Z,Y),padre(X,Z).
	