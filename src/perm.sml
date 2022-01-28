(* file: perm.sml *)
(* description: permutations for nominal terms *)
(* author: Masaki Haga *)

signature PERM = 
sig
  type atom = Atom.key
  type swap 
  type perm 

  val apply: perm -> atom -> atom
  val supp: perm -> atom -> bool
end

structure Perm : PERM =
struct

  type atom = Atom.key
  type swap = atom * atom
  type perm = swap list

  fun apply [] a_0 = a_0
    | apply ((a,b)::p) a_0 =
        let val a_1 = if a_0 = a then b
                      else if a_0 = b then a
                           else a_0
        in apply p a_1
        end
  fun supp p a = apply p a <> a
end
