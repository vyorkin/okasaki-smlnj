signature Stack =
sig
  type 'a Stack
  val empty   : 'a Stack
  val isEmpty : 'a Stack -> bool
  val cons    : 'a * 'a Stack -> 'a Stack
  val head    : 'a Stack -> 'a
  val tail    : 'a Stack -> 'a Stack
end

structure List: Stack =
struct
  type 'a Stack = 'a list

  val empty = []

  fun isEmpty s = null s
  fun cons (x, s) = x :: s
  fun head s = hd s
  fun tail s = tl s
end

structure CustomStack: Stack =
struct
  datatype 'a Stack = Nil | Cons of 'a * 'a Stack

  val empty = Nil

  fun isEmpty Nil = true
    | isEmpty _   = false
  fun cons (x, s) = Cons (x, s)
  fun head Nil = raise Empty
    | head (Cons (x, _)) = x
  fun tail Nil = raise Empty
    | tail (Cons (_, s)) = s
end
