fun concat [] ys = ys
  | concat (x :: xs) ys = x :: (concat xs ys)

fun update ([], i, y) = raise Subscript
  | update (x :: xs, 0, y) = y :: xs
  | update (x :: xs, i, y) = x :: update (xs, i - 1, y)

(* Ex. 2.1 *)
fun suffixes [] = [[]]
  | suffixes (x :: xs) = (x :: xs) :: suffixes xs
