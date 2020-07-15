signature Set =
sig
  type Elem
  type Set

  val empty     : Set

  val insert    : Elem * Set -> Set
  val insert'   : Elem * Set -> Set
  (* val insert''  : Elem * Set -> Set *)

  val member    : Elem * Set -> bool
  val member'   : Elem * Set -> bool

  val complete  : Elem * int -> Set
  val complete' : Elem * int -> Set
end

signature Ordered =
sig
  type T

  val min : T

  val eq  : T * T -> bool
  val lt  : T * T -> bool
  val leq : T * T -> bool
end

structure OrderedInt: Ordered =
struct
  type T = int

  val min = Option.valOf Int.minInt

  fun eq (x, y)  = Int.compare (x, y) = General.EQUAL
  fun lt (x, y)  = x < y
  fun leq (x, y) = x <= y
end

functor UnbalancedSet (Element: Ordered): Set =
struct
  type Elem = Element.T

  datatype Tree = E | T of Tree * Elem * Tree

  type Set = Tree

  val empty = E

  fun member (x, E) = false
    | member (x, T (l, y, r)) =
        if Element.lt (x, y) then member (x, l)
        else if Element.lt (y, x) then member (x, r)
        else true

  fun mem (x, E, z) = Element.eq (x, z)
    | mem (x, T (l, y, r), _) =
        if Element.leq (x, y) then mem (x, l, y)
        else mem (x, r, y)

  (* Ex. 2.2 *)
  fun member' (x, t) = mem (x, t, Element.min)

  fun insert (x, E) = T (E, x, E)
    | insert (x, s as T (l, y, r)) =
        if Element.lt (x, y) then T (insert (x, l), y, r)
        else if Element.lt (y, x) then T (l, y, insert (x, r))
        else s

  exception SAMEVALUE

  (* Ex. 2.3 *)
  fun insert' (x, s) =
    let
      fun ins E = T (E, x, E)
        | ins (T (l, v, r)) =
            if Element.lt (v, x) then T (ins l, v, r)
            else if Element.lt (x, v) then T (l, v, ins r)
            else raise SAMEVALUE
    in ins s handle SAMEVALUE => s end

  fun complete' (x, 0) = raise Empty
    | complete' (x, 1) = T (E, x, E)
    | complete' (x, d) = T (complete' (x, d - 1), x, E)

  fun complete (x, 0) = T (E, x, T (E, x, E))
    | complete (x, 1) = T (T (E, x, E), x, complete (x, 0))
    | complete (x, m) = T (complete (x, m - 2), x, complete (x, m - 1))
end

structure IntUS = UnbalancedSet(OrderedInt);
