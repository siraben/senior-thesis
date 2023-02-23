(* Most direct encoding of graphs *)
Require Import Nat.
Require Import Arith.
From Hammer Require Import Tactics.
From Hammer Require Import Hammer.
Require Import List.
Import ListNotations.
Import Nat.
Open Scope nat_scope.

Module Type Graphs.
(** A graph is a set of vertices V and a relation E. *)
Parameter V : Type.
Parameter E : V -> V -> Prop.
Hint Unfold E V : core.
End Graphs.

Lemma tool2 : forall (X:Type) (l1 l2 : list X) (a b: X),
  l1 ++ [a] = l2 ++ [b] -> a = b /\ l1 = l2.
Proof.
  intros X l1 l2 a b.
  generalize dependent l2.
  generalize dependent a.
  generalize dependent b.
  induction l1; intros b c l2 H; sauto q: on dep: on.
Qed.

(* A list is either empty or an initial segment and the last
   element. *)
Lemma split_last : forall {X} (l : list X), l = [] \/ exists x p, l = p ++ [x].
Proof.
  intros X l.
  rewrite <- (rev_involutive l).
  induction l.
  - left. auto.
  - destruct IHl.
    + hauto use: rev_involutive, app_cons_not_nil, rev_unit, app_nil_r unfold: app, rev.
    + destruct H as [t [p Hl]].
      right. simpl. rewrite rev_app_distr. simpl.
      rewrite Hl. exists t. exists (a :: p). reflexivity.
Qed.

Lemma strong_ind (P : nat -> Prop) :
  (forall m, (forall k : nat, k < m -> P k) -> P m) ->
  forall n, P n.
Proof.
  intros H n; enough (H0: forall p, p <= n -> P p).
    - hauto l: on.
    - induction n.
      + sauto q: on.
      + intros p Hp.
        hfcrush use: le_succ_r, lt_n_Sm_le.
Qed.

(* lemma that firstn n l ++ nth n l = firstn (n + 1) l *)
Lemma firstn_nth : forall {X} (a : X) (l : list X) n, n < length l  ->
                                                 firstn n l ++ [nth n l a] = firstn (S n) l.
Proof.
  intros X a l n H.
  pose proof (nth_split l a ltac:(sauto lq: on rew:off)).
  destruct H0 as [l1 [l2 H0]].
  assert (l1 = firstn n l).
  {
    hauto l: on use: firstn_app, app_nil_r, firstn_O, minus_n_n, firstn_all.
  }
  subst.
  hauto lq: on drew: off use: firstn_cons, add_succ_l, add_comm, firstn_app_2, firstn_O, add_0_l.
Qed.

(* let n be less than length l - 1, then first (S (S n)) l = first n l ++ [nth n l a; nth (S n) l a ] *)
Lemma firstn_nth2 : forall {X} (a : X) (l : list X) n,
    n < length l - 1  -> firstn n l ++ [nth n l a; nth (S n) l a] = firstn (S (S n)) l.
Proof.
  intros X a l n H.
  replace (firstn n l ++ [nth n l a; nth (S n ) l a]) with (firstn n l ++ [nth n l a] ++ [nth (S n) l a]).
  rewrite app_assoc.
  rewrite firstn_nth.
  2: sfirstorder.
  hauto lq: on use: lt_add_lt_sub_l, add_1_l, @firstn_nth.
  hauto lq: on drew: off.
Qed.

Lemma firstn_nth3 : forall {X} (a : X) (l : list X) n,
    n < length l - 1  -> l = firstn n l ++ [nth n l a; nth (S n) l a] ++ skipn (S (S n)) l.
Proof.
  hauto lq: on drew: off use: @firstn_nth2, app_assoc, firstn_skipn.
Qed.


Compute ((fun i l l2 x y => (firstn (i - 1) l ++ [x; y] ++ l2)) 3 [0;1] [4;5] 2 3).
(* for an index i less than the length of a list l, the ith and i+1Th elements
   of l are adjacent and can decompose l into three lists *)
Lemma split_middle : forall {X} (l : list X) (i : nat),
  i < length l - 1 ->
  exists l1 l2 a, l = l1 ++ [nth i l a; nth (S i) l a] ++ l2.
Proof.
  intros X l i H.
  eexists.
  eexists.
  eexists.
  erewrite <- firstn_nth3.
  reflexivity.
  sfirstorder.
  Unshelve.
  destruct l as [|a l].
  - sfirstorder use: length_zero_iff_nil, pred_of_minus, nlt_0_r unfold: Init.Nat.pred.
  - hauto lq: on drew: off.
Qed.    

(* Parametric module instantiated from a given graph structure G. *)
Module SimpleGraphs (G : Graphs).
  Import G.
  (** A graph has no loops if E is irreflexive. *)
  Definition no_loops := forall v, ~ E v v.

  (** A graph is underacted if E is symmetric. *)
  Definition undirected := forall v1 v2, E v1 v2 -> E v2 v1.

  (** A walk is a sequence of vertices such that each pair of
      consecutive vertices is connected by an edge. *)
  Inductive walk : V -> V -> list V -> Prop :=
  | walk_nil : forall v, walk v v [v]
  | walk_edge : forall v1 v2, E v1 v2 -> walk v1 v2 [v1; v2]
  | walk_app : forall v1 v2 v3 l1 l2,
      walk v1 v2 (v1 :: l1 ++ [v2]) -> walk v2 v3 (v2 :: l2 ++ [v3]) -> walk v1 v3 (v1 :: l1 ++ [v2] ++ l2 ++ [v3]).

  (** In an undirected graph, a walk can be reversed. *)
  Lemma walk_rev : forall v1 v2 l, undirected -> walk v1 v2 l -> walk v2 v1 (rev l).
  Proof.
    intros v1 v2 l un H.
    induction H.
    - sfirstorder.
    - simpl. hfcrush.
    - simpl in *.
      rewrite !rev_app_distr in *.
      simpl in *.
      rewrite <- !app_assoc, !rev_app_distr in *.
      simpl in *.
      sauto lq: on.
  Qed.

  (** The first vertex of a walk is the starting vertex. *)
  Lemma walk_first : forall v1 v2 l, walk v1 v2 l -> exists l', l = v1 :: l'.
  Proof.
    intros v1 v2 l H.
    induction H; sfirstorder.
  Qed.

  (** The last vertex of a walk is the ending vertex. *)
  Lemma walk_last : forall v1 v2 l, walk v1 v2 l -> exists l', l = l' ++ [v2].
  Proof.
    intros v1 v2 l H.
    induction H.
    - exists []. reflexivity.
    - exists [v1]. reflexivity.
    - exists (v1 :: l1 ++ [v2] ++ l2).
      simpl.
      hauto use: app_assoc, app_comm_cons.
  Qed.

  (** Inversion lemma for walks. *)
  (** This is an example of how we can have statements that require
      too strong of a hypothesis. *)
  Lemma walk_inv' : forall v1 v2 l, v1 <> v2 -> walk v1 v2 l -> exists l', l = v1 :: l' ++ [v2].
  Proof.
    intros v1 v2 l H1 H2.
    destruct l as [|v l]; [sauto lq: on|].
    assert (v = v1) by sauto lq: on rew: off.
    subst.
    destruct l.
    - apply walk_last in H2.
      destruct H2 as [l' H2].
      destruct l'; hauto q: on.
    - apply walk_last in H2.
      sauto q: on dep: on.
  Qed.

  (** Actual inversion lemma for walks. *)
  (** This statement has the same conclusion as the previous one, but
      it requires a weaker hypothesis. *)
  Lemma walk_inv : forall v1 v2 l, 2 <= length l -> walk v1 v2 l -> exists l', l = v1 :: l' ++ [v2].
  Proof.
    intros v1 v2 l H0 H.
    apply le_lt_or_eq in H0.
    destruct l.
    - sauto lq: on.
    - destruct l.
      + sauto q: on.
      + assert (v1 = v) by sauto lq: on.
        subst.
        destruct H0.
        * apply walk_last in H.
          destruct H as [l' H].
          destruct l.
          ** sauto q: on.
          ** clear H0.
             destruct l'; [scongruence|].
             simpl in H.
             destruct l'; [scongruence|].
             simpl in H.
             hauto lq: on drew: off use: app_comm_cons.
        * hauto q: on dep: on use: walk_last inv: list.
  Qed.

  Lemma walk_edge_iff : forall v1 v2, walk v1 v2 [v1; v2] <-> E v1 v2.
  Proof.
    intros v1 v2.
    split.
    - intros H.
      inversion H.
      + scongruence.
      + hauto lq: on use: app_cons_not_nil, elt_eq_unit unfold: app.
    - intros H.
      sauto lq: on.
  Qed.

  Lemma walk_adj : forall v1 v2, E v1 v2 -> walk v1 v2 [v1;v2].
  Proof.
    sauto lq: on.
  Qed.
  
  Lemma walk_cons : forall v1 v2 v3 l, E v1 v2 -> walk v2 v3 l -> walk v1 v3 (v1 :: l).
  Proof.
    intros v1 v2 v3 l H0 H1.
    inversion H1.
    - sauto lq: on.
    - subst.
      apply walk_adj in H0.
      replace [v1; v2] with ([v1] ++ [] ++ [v2]) in H0 by reflexivity.
      replace [v2; v3] with ([v2] ++ [] ++ [v3]) in H1 by reflexivity.
      pose proof (walk_app v1 v2 v3 [] [] H0 H1).
      sfirstorder.
    - subst.
      apply walk_adj in H0.
      replace [v1; v2] with ([v1] ++ [] ++ [v2]) in H0 by reflexivity.
      (* H1 : walk v2 v3 (v2 :: l1 ++ [v4] ++ l2 ++ [v3]) *)
      (* X : wala v2 v4 (v2 :: l1 ++ [v4]) *)
      (* X0 : walk v4 v3 (v4 :: l2 ++ [v3]) *)
      replace (v2 :: l1 ++ [v4]) with ([v2] ++ l1 ++ [v4]) in H1 by reflexivity.
      replace (v4 :: l2 ++ [v3]) with ([v4] ++ l2 ++ [v3]) in H2 by reflexivity.
      pose proof (walk_app v1 v2 v3 [] (l1 ++ [v4] ++ l2) ltac:(hauto lq: on)).
      replace (v2 :: l1 ++ [v4] ++ l2 ++ [v3]) with (v2 :: (l1 ++ [v4] ++ l2) ++ [v3]) in H1.
      apply H3 in H1.
      enough (v1 :: v2 :: (l1 ++ v4 :: l2) ++ [v3] = (v1 :: v2 :: l1 ++ [v4] ++ l2 ++ [v3])).
      + scongruence.
      + simpl. rewrite <- app_assoc.
        reflexivity.
      + rewrite <- app_assoc. reflexivity.
  Qed.

  Lemma walk_adj3 : forall v1 v2 v3, E v1 v2 /\ E v2 v3 <-> walk v1 v3 [v1;v2;v3].
  Proof.
    intros v1 v2 v3.
    split.
    - intros H.
      sfirstorder use: walk_cons, walk_adj.
    - intros H.
      remember [v1; v2; v3] as l.
      inversion H; subst.
      * scongruence.
      * scongruence.
      * inversion H4.
        assert (l1 ++ v4 :: l2 = [v2]).
        {
          replace [v2;v3] with ([v2] ++ [v3]) in H3 by reflexivity.
          rewrite app_comm_cons, app_assoc in H3.
          eapply app_inv_tail in H3.
          sfirstorder.
        }
        assert (l1 = []) by strivial use: elt_eq_unit.
        assert (l2 = []) by strivial use: elt_eq_unit.
        strivial use: walk_edge_iff.
  Qed.

  Lemma walk_adj4 : forall v1 v2 v3 v4, E v1 v2 /\ E v2 v3 /\ E v3 v4 <-> walk v1 v4 [v1;v2;v3;v4].
  Proof.
    intros v1 v2 v3 v4.
    split.
    - intros H.
      sfirstorder use: walk_adj3, walk_cons.
    - intros H.
      remember [v1; v2; v3; v4] as l.
      inversion H; subst.
      + scongruence.
      + scongruence.
      + replace [v1;v2;v3;v4] with ([v1] ++ [v2;v3] ++ [v4]) in H4 by reflexivity.
        rewrite app_comm_cons in H4.
        rewrite !app_assoc in H4.
        apply (app_inv_tail [v4] (((v1 :: l1) ++ [v5]) ++ l2) ([v1] ++ [v2; v3])) in H4.
        inversion H4.
        assert (l1 = [] \/ l2 = []).
        {
          destruct l1, l2; auto.
          hauto use: app_cons_not_nil, nil_cons, app_eq_unit unfold: app.
        }
        destruct H2.
        * hfcrush use: walk_edge_iff, app_comm_cons, walk_adj3 unfold: app inv: list.
        * subst.
          rewrite app_nil_r in H3, H4.
          assert (l1 = [v2]).
          {
            destruct l1.
            - scongruence.
            - destruct l1.
              scongruence.
              simpl in H3. scongruence use: app_cons_not_nil.
          }
          strivial use: walk_edge_iff, walk_adj3 unfold: app inv: list.
  Qed.

  Lemma walk_split : forall v1 v2 v3 l, walk v1 v3 (v1 :: v2 :: l) -> walk v1 v2 [v1;v2].
  Proof.
    intros v1 v2 v3 l H.
    remember (v1 :: v2 :: l) as l'.
    inversion H; subst.
    - scongruence.
    - scongruence.
    - pose proof (walk_last _ _ _ H).
      destruct H2 as [l'' H2].
      ssimpl.
      destruct l''.
      + hauto lq: on drew: off.
      + inversion H2.
        ssimpl.
        rewrite H6 in H3.
        rewrite app_comm_cons, app_assoc in H3.
        apply app_inv_tail in H3.
        rewrite <- H3 in H6.
  Admitted.
  


  (** Adjacent vertices in a walk are connected by an edge. *)
  (* for every index i, l[i] is adjacent to l[i+1] *)
  Lemma walk_adjacent : forall v1 v2 l, walk v1 v2 l -> forall i, i < length l - 1 -> E (nth i l v1) (nth (S i) l v2).
  Proof.
    intros v1 v2 l X i H.
    epose proof (split_middle l _ H).
    destruct H0 as [l1 [l2 [a X0]]].
  Admitted.

  Inductive path' : V -> V -> Type :=
  | path'_nil : forall v1 v2, E v1 v2 -> path' v1 v2
  | path'_cons : forall v1 v2 v3, E v1 v2 -> path' v2 v3 -> path' v1 v3.

  Lemma path'_trans : forall v1 v2 v3, path' v1 v2 -> path' v2 v3 -> path' v1 v3.
  Proof.
    intros v1 v2 v3 H1 H2.
    induction H1; sauto lq: on.
  Qed.

  Lemma path'_snoc : forall v1 v2 v3, path' v1 v2 -> E v2 v3 -> path' v1 v3.
  Proof.
    intros v1 v2 v3 H1 H2.
    induction H1; sauto lq: on.
  Qed.

  (* (v1 -> v2) ++ (v2 -> v3 -> v4) or is it (v1 -> v2 -> v3) ++ (v3 -> v4) *)
  
End SimpleGraphs.

(**
rough plan:
- walks (no constraints) -> trail (no repeated E) -> path (no repeated V + E)
- connectedness (strong connectedness), 2-connected graphs
- degrees
- subgraphs
- finite graphs
- colorability
 *)


(* Instantiate the graph theory with the natural numbers. *)
Module NatGraphs <: Graphs.
  Definition V := nat.
  Definition E x y := x < y.
End NatGraphs.


Module NatSimpleGraphs := SimpleGraphs NatGraphs.

(* natural number graphs have no loops *)
Theorem nat_graphs_undirected : NatSimpleGraphs.no_loops.
Proof.
  sfirstorder use: lt_irrefl.
Qed.



