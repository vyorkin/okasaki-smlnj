(* Leftist heap / Левоориентированная куча *)

(* Heap = Priority queue *)
(* Supports efficient access only to the minimum element *)

(* Visualization: https://www.cs.usfca.edu/~galles/visualization/LeftistHeap.html *)

(* https://www.youtube.com/watch?v=y2mipTl823k
   https://www.youtube.com/watch?v=ktes_kHGG2Q *)

(* Heaps often implemented as heap-ordered binary trees:
   - Element at each node is <= than the elements at its children nodes
   - Minimum element in a tree is always at the root *)

(* rank(node) - length of the "right spine" -
   rightmost path from node to an empty (leaf) node *)

(* rank(node) - distance to the nearest leaf *)

(* forall (Node l x r) in Heap:
   rank(l) >= rank(r) *)

(* Ex. 3.1 (Informal proof by induction)

   Proof is by induction using hypothesis rank(l) >= rank(r).

      h : Heap
   -> size h = n
   -> rank(l) >= rank(r)
   ------------------------
   -> rank h >= log (n + 1)

   1) h is E:

      size E = 0 => n = 0
      rank E = 0
      log (0 + 1) [ 2^x = 1 => x = 0 ]
      ---
      Qed.

   2) h is (T (l, _, r)):

      size (T (l, _, r)) = 1 + size (l) + size(r)
   -> rank(l) >= rank(r)
   -> ???
   -> Qed.
*)

signature Heap =
sig
  structure Elem: Ordered
  type Heap

  val empty   : Heap
  val isEmpty : Heap -> bool
  val insert  : Elem.T * Heap -> Heap
  val merge   : Heap * Heap -> Heap

  val findMin   : Heap -> Elem.T
  val deleteMin : Heap -> Heap
end

functor LeftistHeap (Element: Ordered) : Heap =
struct
  structure Elem = Element

  datatype Heap = E | T of int * Elem.T * Heap * Heap

  fun rank E = 0
    | rank (T (r, _, _, _)) = r

  val empty = E

  fun isEmpty E = true
    | isEmpty _ = false

  fun makeT (x, l, r) =
      if rank l >= rank r
      then T (rank l + 1, x, l, r)
      else T (rank r + 1, x, r, l)

  fun merge (h, E) = h
    | merge (E, h) = h
    | merge (h1 as T (_, x, l1, r1), h2 as T (_, y, l2, r2)) =
      if Elem.leq (x, y)
      then makeT (x, l1, merge (r1, h2))
      else makeT (y, l2, merge (h1, r2))

  fun insert (x, h) = merge (T (1, x, E, E), h)

  exception EMPTY

  fun findMin E = raise EMPTY
    | findMin (T (_, _, x, _)) = x

  fun deleteMin E = raise EMPTY
    | deleteMin (T (_, l, x, r)) = merge (l, r)
end
