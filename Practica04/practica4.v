(*/C\u00e9sar Eduardo Jardines Mendoza
*)


Inductive Nat : Type := 
| Z : Nat
| W : Nat -> Nat.

Definition pred (n: Nat): Nat :=
match n with
| Z => Z
| W m => m
end.

Fixpoint plus (n: Nat) (m: Nat): Nat :=
match n with
| Z => m
| W x => W (plus x m)
end.

Inductive Bool: Type :=
| true: Bool
| false: Bool.
Check false.

Definition negb (a: Bool): Bool :=
match a with
| true => false
| false => true
end.

Definition andb (a b: Bool): Bool :=
match a with
| true => b
| false => false
end.

Definition orb (a b: Bool): Bool :=
match a with
| true => b
| false => false
end.

Lemma plus_Z: 
forall (n : Nat), plus n Z = n.
Proof.
induction n.
- simpl. trivial.
- simpl. rewrite IHn. trivial.
Qed.

Lemma plus_W:
forall (n m: Nat), plus n (W m) = W (plus n m).
Proof.
induction n.
- intro. simpl. trivial.
- intro. simpl. rewrite IHn. trivial.
Qed.


Theorem plus_comm: 
forall (n m: Nat), plus n m = plus m n.
Proof.
induction n.
 - intro. (*Sub demostraci\u00f3n*)
  simpl.
  rewrite plus_Z. trivial.
 - intro.
  simpl.
  rewrite plus_W.
  rewrite IHn. trivial.
Qed.


Theorem negb_invol: 
forall (b: Bool), negb (negb b) = b.
Proof.
  intros b. destruct b.
    reflexivity.
    reflexivity.
Qed.


Theorem andb_comm: 
forall b c, andb b c = andb c b.
Proof.
intros.
case b.
simpl.
case c.
simpl.
reflexivity.
reflexivity.
case c.
reflexivity.
reflexivity.
Qed.
