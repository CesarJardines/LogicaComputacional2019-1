(*Jardines Mendoza C\u00e9sar Eduardo*)
Set Implicit Arguments.

(*1. \u00c1rbol binario de dos tipos*)
Inductive ABTree (A B: Type): Type :=
| RootA: A -> ABTree A B
| RootB: B -> ABTree A B
| NodeA: A -> ABTree A B -> ABTree A B -> ABTree A B
| NodeB: B -> ABTree A B -> ABTree A B -> ABTree A B.



(*Lista.*)
Inductive AList (A: Type): Type :=
| nil: AList A
| ConsA: A -> AList A -> AList A.

(*1.Concatenacion de dos listas.*)
Fixpoint conc (A: Type) (l1 l2: AList A): AList A :=
match l2 with
| nil _ => l1
| ConsA x xs => conc (ConsA x l1) xs
end.

(*2. Devuelve una lista con los elementos de tipo 
 A de un ABTree A B*)
Fixpoint elemsA (A B: Type) (t: ABTree A B): AList A :=
match t with
| RootA _ _ => nil A
| RootB _ _ => nil A
| NodeA e i d => ConsA e (conc (elemsA i) (elemsA d))
| NodeB _ i d => conc (elemsA i) (elemsA d)
end.



(*3. Funci\u00f3n que devuelve una lista con los elementos de tipo
 B de un ABTree A B*)
Fixpoint elemsB (A B: Type) (t: ABTree A B): AList B :=
match t with
| RootA _ _ => nil B
| RootB _ _ => nil B
| NodeA _ i d => conc (elemsB i) (elemsB d)
| NodeB e i d => ConsA e (conc (elemsB i) (elemsB d))
end.



(*4. Funci\u00f3n que devuelve el n\u00famero de elementos de un ABTree.*)
Fixpoint numElems (A B:Type) (t: ABTree A B): nat :=
match t with
| RootA _ _ => 1
| RootB _ _ => 1
| NodeA e i d => 1 + (numElems i) + (numElems d)
| NodeB e i d => 1 + (numElems i) + (numElems d)
end.



(*Funci\u00f3n que verifica la paridad.*)
Fixpoint serImpar (n: nat): bool :=
match n with
| 0 => false
| S n' => negb (serImpar n')
end.

Compute serImpar 2385.

(*Lema sobre la suma par de impares.*)
Lemma sumaImpar:
forall (n m: nat), serImpar n = true -> serImpar m = true -> serImpar (n + m) = false.
Proof.
Admitted.

(*5. Demuestre que el numero de elementos de todo arbol es impar.*)
Theorem elemImpar: 
forall (A B:Type) (t: ABTree A B), serImpar(numElems(t)) = true.
Proof.
induction t.
-simpl. trivial.
-simpl. trivial.
-simpl. rewrite sumaImpar.
simpl. trivial.
rewrite IHt1. trivial.
rewrite IHt2. trivial.
-simpl. rewrite sumaImpar.
simpl. trivial.
rewrite IHt1. trivial.
rewrite IHt2. trivial.
Qed.