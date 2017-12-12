(*
   The following declaration tells Coq that we are defining
   a new set of data values -- a _type_.
*)

(* Enumerated Types *)
Inductive day : Type :=
  | monday : day
  | sunday : day
  | tuesday : day
  | wednesday : day
  | thursday : day
  | friday : day
  | saturday : day.


(* Defining a function that operates on days *)
Definition next_weekday (d:day) : day :=
  match d with
  | monday => tuesday
  | tuesday => wednesday
  | wednesday => thursday
  | thursday => friday
  | friday => saturday
  | saturday => sunday
  | sunday => monday
  end.

(* Compute (next_weekday friday). *)
(* ==> monday : day *)
(* Compute (next_weekday (next_weekday saturday)). *)
(* ==> tuesday : day *)

Example test_next_weekday:
  (next_weekday friday) = saturday.
Proof . simpl . reflexivity . Qed.

(*--------------
 Our first proof
 ---------------
 essentially this can be read as "The assertion we've just made can be prved by observing that both sides of the equality evaluate to the same thing, after some simplification." The details are not important for now (we'll come back to them in a bit), but essentially this can be read as "The assertion we've just made can be proved by observing that both sides of the equality evaluate to the same thing, after some simplification."
*)

Example text_next_next_weekday:
  (next_weekday (next_weekday friday)) = sunday.
Proof . simpl . reflexivity . Qed.

(*------------------------------------------
   Booleans
  ------------------------------------------*)
Inductive bool : Type :=
  | true : bool
  | false : bool.

(* Defining functions over booleans:
   notb :: bool -> bool
   andb :: bool -> bool -> bool
   orb  :: bool -> bool -> bool
 *)

Definition notb (b1: bool) : bool :=
  match b1 with
  | true => false
  | false => true
  end.

Definition andb (b1: bool) (b2: bool) : bool :=
  match b1 with
  | true => b2
  | false => false
  end.

Definition orb (b1: bool) (b2: bool) : bool :=
  match b1 with
  | true => true
  | false => b2
  end.

Example test_notb1 : (notb true = false).
Proof. simpl. reflexivity. Qed.

Example test_notb2 : (notb false = true).
Proof. simpl. reflexivity. Qed.

Example test_orb1 : (orb true true = true).
Proof. simpl. reflexivity. Qed.

Example test_orb2 : (orb true false = true).
Proof. simpl. reflexivity. Qed.

Example test_orb3 : (orb false true = true).
Proof. simpl. reflexivity. Qed.

Example test_orb4 : (orb false false = false).
Proof. simpl. reflexivity. Qed.

(* The infix command defines a new symbolic notation
   for an existing definition
 *)

Notation "~ x" := (notb x).
Notation "x && y" := (andb x y).
Notation "x || y" := (orb x y).

Example test_andb1 : (true && false) = false.
Proof. simpl. reflexivity. Qed.

(* Exercise: 1 star (nandb)
The function should return true if either or both of its inputs are false. *)

Definition nandb (b1: bool) (b2: bool) : bool :=
  match b1 with
  | true => ~b2
  | false => true
  end.

(* That is, if b1 is true return not b2:
   - If b2 is true, nandb b1 b2 => false
   - If b2 is false, nanb b1 b2 => true
   Else, b1 is already false, so return true. *)

Notation "x '/\' y" := (nandb x y).

Example test_nandb1 : (true /\ false) = true.
Proof. simpl. reflexivity. Qed.

Example test_nandb2 : (false /\ true) = true.
Proof. simpl. reflexivity. Qed.

Example test_nandb3 : (false /\ false) = true.
Proof. simpl. reflexivity. Qed.

Example test_nandb4 : (true /\ true) = false.
Proof. simpl. reflexivity. Qed.

(* Exercise: 1 star (andb3)
Do the same for the andb3 function below. This function should return true when all of its inputs are true, and false otherwise. *)

Definition andb3 (b1:bool) (b2:bool) (b3:bool) : bool :=
  match b1 with
  | true => (b2 && b3)
  | false => false
  end.

Example test_andb31: (andb3 true true true) = true.
Proof. simpl. reflexivity. Qed.

Example test_andb32: (andb3 false true true) = false.
Proof. simpl. reflexivity. Qed.

Example test_andb33: (andb3 true false true) = false.
Proof. simpl. reflexivity. Qed.

Example test_andb34: (andb3 true true false) = false.
Proof. simpl. reflexivity. Qed.

(* Function Types
   Every expression in Coq has a type, describing what sort of thing it computes. The Check command asks Coq to print the type of an expression.

   Check true.
         ===> true : bool
   Check (negb true).
         ===> negb true : bool
 *)

(*--------------
  Compound Types
  --------------
  The types we have defined so far are examples of "enumerated types": the  ir definitions explicitly enumerate a finite set of elements, each of wh  ich is just a bare constructor. Here is a more interesting type definiti  won, where one of the constructors takes an argument: 
 *)

(*
  > reg, green, and blue are the constructors of rgb;
  > black, white, and primary are the constructors of color;
  > the expression red belongs to the set rgb, as do the expressions green    and blue;
  > the expressions black and white belong to the set color;
  > if p is an expression belonging to the set rgb, then primary p (pronou    nced "the constructor primary applied to the argument p") is an expres    sion belonging to the set color; and
  > expressions formed in these ways are the only ones belonging to the se    ts rgb and color.
 *)

Inductive rgb : Type :=
| red : rgb
| green : rgb
| blue : rgb.

Inductive color : Type :=
| black : color
| white : color
| primary : rgb -> color.

(* We can define functions on colors using pattern matching just as we have done for day and bool. *)

Definition monochrome (c : color) : bool :=
  match c with
  | black => true
  | white => true
  | primary p => false
  end.

Definition isred (c: color) : bool :=
  match c with
  | black => false
  | white => false
  | primary red => true
  | primary _ => true
  end.

(* Compute (isred (primary red)). *)

(* ========================================
   Modules
   ========================================
   Coq provides a module system, to aid in organizing large developments. In this course we won't need most of its features, but one is useful: If we enclose a collection of declarations between Module X and End X markers, then, in the remainder of the file after the End, these definitions are referred to by names like X.foo instead of just foo. We will use this feature to introduce the definition of the type nat in an inner module so that it does not interfere with the one from the standard library (which we want to use in the rest because it comes with a tiny bit of convenient special notation).
   ========================================
 *)

Module NatPlayground.

Inductive nat : Type :=
  | O : nat
  | S : nat -> nat.

(* Predecessor Function *)

Definition pred (n : nat) : nat :=
  match n with
  | O => O
  | S n' => n'
  end.

End NatPlayground.

(* Check (S (S (S (S O)))). *)

Definition minustwo (n : nat) : nat :=
  match n with
  | O => O
  | S O => O
  | S (S n') => n'
  end.

(* Compute(minustwo 4). *)

(* -------------------------
   FixPoints
   -------------------------
For most function definitions over numbers, just pattern matching is not enough: we also need recursion. For example, to check that a number n is even, we may need to recursively check whether n-2 is even. To write such functions, we use the keyword Fixpoint.
 *)

Fixpoint evenb (n : nat) : bool :=
  match n with
  | O => true
  | S O => false
  | S (S n') => evenb n'
  end.

Example test_evenb : evenb 4 = true.
Proof. simpl. reflexivity. Qed.

Example test_evenb2 : evenb 5 = false.
Proof. simpl. reflexivity. Qed.

(* We can define oddb by a similar Fixpoint declaration, but here is a simpler definition: *)

Definition oddb (n:nat) : bool := ~(evenb n).

Example test_oddb1: oddb 1 = true.
Proof. simpl. reflexivity. Qed.

Example test_oddb2: oddb 4 = false.
Proof. simpl. reflexivity. Qed.

Module NatPlayground2.

Fixpoint plus (n:nat) (m:nat) : nat :=
  match n with
  | O => m
  | S n' => S (plus n' m)
  end.

(* Compute(plus 3 2). *)

(*  As a notational convenience, if two or more arguments have the same type, they can be written together. In the following definition, (n m : nat) means just the same as if we had written (n : nat) (m : nat). *)

Fixpoint mult (n m : nat) : nat :=
  match n with
  | O => O
  | S n' => plus m (mult n' m)
  end.

Example test_mult1: mult 3 3 = 9.
Proof. simpl. reflexivity. Qed.

Example test_mult2: mult 4 1 = 4.
Proof. simpl. reflexivity. Qed.

(*  You can match two expressions at once by using tuples *)
Fixpoint minus (n m : nat) : nat :=
  match (n, m) with
  | (0, _) => 0
  | (S _, 0) => n
  | (S n', S m') => minus n' m'
  end.

(* Compute(minus 3 1). *)

End NatPlayground2.

(* Exponentiation *)
Fixpoint exp (base power : nat) : nat :=
  match power with
  | O => S O
  | S p => mult base (exp base p)
  end.

Example test_exp1: exp 3 2 = 9.
Proof. simpl. reflexivity. Qed.

(* __________________________________

   Exercises
   __________________________________
 *)

(*----------------------------
  Exercise: 1 star (factorial)
  ----------------------------
Recall the standard mathematical factorial function:

       factorial(0)  =  1
       factorial(n)  =  n * factorial(n-1)     (if n>0)

Translate this into Coq.
 *)

Fixpoint factorial (n: nat) : nat :=
  match n with
  | O => 1
  | S n' => mult n (factorial n')
  end.

Example test_factorial1: (factorial 3) = 6.
Proof. simpl. reflexivity. Qed.
Example test_factorial2: (factorial 5) = (mult 10 12).
Proof. simpl. reflexivity. Qed.

(* We can make numerical expressions a little easier to read and write by introducing notations for addition, multiplication, and subtraction. *)

Notation "x + y" := (plus x y)
                       (at level 50, left associativity)
                       : nat_scope.
Notation "x - y" := (minus x y)
                       (at level 50, left associativity)
                       : nat_scope.
Notation "x * y" := (mult x y)
                       (at level 40, left associativity)
                       : nat_scope.

(* Check ((0 + 1) + 1). *)

(* nat equality *)

Fixpoint beq_nat (n m : nat) : bool :=
  match n with
  | O => match m with
         | O => true
         | S m' => false
         end
  | S n' => match m with
            | O => false
            | S m' => beq_nat n' m'
            end
  end.

(* nat less than or equal to *)
Fixpoint leb (n m : nat) : bool :=
  match n with
  | O => true
  | S n' => match m with
            | O => false
            | S m' => leb n' m'
            end
  end.

Example test_leb1: (leb 2 2) = true.
Proof. simpl. reflexivity. Qed.
Example test_leb2: (leb 2 4) = true.
Proof. simpl. reflexivity. Qed.
Example test_leb3: (leb 4 2) = false.
Proof. simpl. reflexivity. Qed.

(*--------------------------
  Exercise: 1 star (blt_nat)
  --------------------------
The blt_nat function tests natural numbers for less-than, yielding a boolean. Instead of making up a new Fixpoint for this one, define it in terms of a previously defined function.
 *)

Definition blt_nat (n m : nat) : bool :=
  (leb n m) && ~(beq_nat n m).

Example test_blt_nat1: (blt_nat 2 2) = false.
Proof. simpl. reflexivity. Qed.
Example test_blt_nat2: (blt_nat 2 4) = true.
Proof. simpl. reflexivity. Qed.
Example test_blt_nat3: (blt_nat 4 2) = false.
Proof. simpl. reflexivity. Qed.

(* ============================
   Proof by Simplification
   ============================
Now that we've defined a few datatypes and functions, let's turn to stating and proving properties of their behavior. Actually, we've already started doing this: each Example in the previous sections makes a precise claim about the behavior of some function on some particular inputs. The proofs of these claims were always the same: use simpl to simplify both sides of the equation, then use reflexivity to check that both sides contain identical values.
The same sort of "proof by simplification" can be used to prove more interesting properties as well. For example, the fact that 0 is a "neutral element" for + on the left can be proved just by observing that 0 + n reduces to n no matter what n is, a fact that can be read directly off the definition of plus.
 *)

Theorem plus_0_n : forall n : nat, 0 + n = n.
Proof.
  intros n. simpl. reflexivity. Qed.

(*
This is a good place to mention that reflexivity is a bit more powerful than we have admitted. In the examples we have seen, the calls to simpl were actually not needed, because reflexivity can perform some simplification automatically when checking that two sides are equal; simpl was just added so that we could see the intermediate state — after simplification but before finishing the proof. Here is a shorter proof of the theorem:
 *)

Theorem plus_0_n' : forall n : nat, 0 + n = n.
Proof.
  intros n. reflexivity. Qed.

(* Tactics
The quantifier ∀ n:nat is added so that our theorem talks about all natural numbers n. Informally, to prove theorems of this form, we generally start by saying "Suppose n is some number..." Formally, this is achieved in the proof by intros n, which moves n from the quantifier in the goal to a context of current assumptions.

The keywords intros, simpl, and reflexivity are examples of tactics. A tactic is a command that is used between Proof and Qed to guide the process of checking some claim we are making. We will see several more tactics in the rest of this chapter and yet more in future chapters.
*)

Theorem plus_1_l : forall n:nat, 1 + n = S n.
Proof.
  intros n. reflexivity. Qed.

Theorem mult_0_l : forall n:nat, 0 * n = 0.
Proof.
  intros n. reflexivity. Qed.

(* ===================
   Proof by Rewriting
   ===================
 *)

Theorem plus_id_example : forall n m: nat,
    n = m ->
    n+n = m + m.
Proof.
  (* move both quantifiers into the context: *)
  intros n m.
  (* move the hypothesis into the context: *)
  intros H.
  (* rewrite the goal using the hypothesis: *)
  rewrite -> H.
  reflexivity. Qed.

(* ------------------------------------
   Exercise: 1 star (plus_id_exercise)
   ------------------------------------
Remove "Admitted." and fill in the proof.
 *)

Theorem plus_id_exercise : forall n m o : nat,
    n = m -> m = o -> n + m = m + o.
Proof.
  intros
    n m o.
  intros H1 H2.
  rewrite -> H1.
  rewrite -> H2.
  reflexivity. Qed.

(* ----------------------------
   Exercise: 2 stars (mult_S_1)
   ----------------------------
 *)

Theorem mult_S_1 : forall n m : nat,
  m = S n ->
  m * (1 + n) = m * m.
Proof.
  intros n m.
  intros H.
  rewrite -> plus_1_l.
  rewrite -> H.
  reflexivity. Qed.

(* ======================
   Proof by Case Analysis
   ======================

Of course, not everything can be proved by simple calculation and rewriting: In general, unknown, hypothetical values (arbitrary numbers, booleans, lists, etc.) can block simplification. For example, if we try to prove the following fact using the simpl tactic as above, we get stuck. (We then use the Abort command to give up on it for the moment.)
 *)

Theorem plus_1_neq_0_firsttry : forall n : nat,
  beq_nat (n + 1) 0 = false.
Proof.
  intros n.
  simpl. (* does nothing! *)
Abort.

(* The reason for this is that the definitions of both beq_nat and + begin by performing a match on their first argument. But here, the first argument to + is the unknown number n and the argument to beq_nat is the compound expression n + 1; neither can be simplified.

To make progress, we need to consider the possible forms of n separately. If n is O, then we can calculate the final result of beq_nat (n + 1) 0 and check that it is, indeed, false. And if n = S n' for some n', then, although we don't know exactly what number n + 1 yields, we can calculate that, at least, it will begin with one S, and this is enough to calculate that, again, beq_nat (n + 1) 0 will yield false.
The tactic that tells Coq to consider, separately, the cases where n = O and where n = S n' is called destruct. *)

Theorem plus_1_neq_0 : forall n : nat,
  beq_nat (n + 1) 0 = false.
Proof.
  intros n. destruct n as [| n'].
  - reflexivity.
  - reflexivity. Qed.

(* The destruct generates two subgoals, which we must then prove, separately, in order to get Coq to accept the theorem. The annotation "as [| n']" is called an intro pattern. It tells Coq what variable names to introduce in each subgoal. In general, what goes between the square brackets is a list of lists of names, separated by |. In this case, the first component is empty, since the O constructor is nullary (it doesn't have any arguments). The second component gives a single name, n', since S is a unary constructor.
The - signs on the second and third lines are called bullets, and they mark the parts of the proof that correspond to each generated subgoal. The proof script that comes after a bullet is the entire proof for a subgoal. In this example, each of the subgoals is easily proved by a single use of reflexivity, which itself performs some simplification — e.g., the first one simplifies beq_nat (S n' + 1) 0 to false by first rewriting (S n' + 1) to S (n' + 1), then unfolding beq_nat, and then simplifying the match. *)



