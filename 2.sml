(*fun printf str = TextIO.stdOut str;*)

(* reverse : int list -> int list*)
(* REQUIRES: true*)
(* ENSURES: reverse L. *)
fun reverse([]) = []
  | reverse(x::L) = reverse(L) @ [x];

fun reverse'(L : int list) = rev L;

fun interleave([], R : int list) = R
  | interleave(L : int list, []) = L
  | interleave(a::L, b::R) = [a] @ [b] @ interleave(L, R);

datatype tree = Empty | Node of tree * int * tree;

(*fun split [ ]  = ([ ],  [ ]) *)
  (*| split [x] = ([x], [ ])*)
  (*| split (x::y::L) =*)
      (*let val (A, B) = split L *)
      (*in (x::A, y::B) *)
    (*end;*)

fun listToTree([]) = Empty
  | listToTree(x::L) =
      let val k = length L div 2
      in Node(listToTree(List.take(L, k)), x, listToTree(List.drop(L, k)))
    end;

(*fun listToTree'([]) = Empty*)
  (*| listToTree'(x::L) = *)
        (*let val (A, B) = split L*)
        (*in Node(listToTree'(A), x, listToTree'(B))*)
    (*end;*)


fun trav Empty = [ ]
    | trav (Node(t1, x, t2)) = trav t1 @ (x :: trav t2);

fun revT Empty = Empty
  | revT(Node(t1, x, t2)) = Node(revT(t2), x, revT(t1));

fun binarySearch (Empty, k) = false
| binarySearch(Node(t1, x, t2), k) = 
    case Int.compare(k, x) of
         LESS => binarySearch(t1, k)
       | EQUAL => true
       | GREATER => binarySearch(t2, k);

print("test:\n");

print("\n reverse [1,2,3,4,5]: ");
reverse([1, 2, 3, 4, 5]);

print("\n reverse' [1,2,3,4,5,6]: ");
reverse'([1, 2, 3, 4, 5, 6]);

print("\n interleave([1, 3, 5, 7, 9], [2, 4, 6, 8, 10]): ");
interleave([1, 3, 5, 7, 9], [2, 4, 6, 8, 10]);

print("\n interleave([1, 3], [2, 4, 6, 8, 10]): ");
interleave([1, 3], [2, 4, 6, 8, 10]);

print("\n listToTree'([1,2,3,4]): ");
listToTree([1,2,3,4]);

print("\n val T = listToTree([3,1,2,4,5]): ");
val T = listToTree([3,1,2,4,5]);

print("\n trav(T): ");
trav(T);

print("\n trav(revT(T)): ");
trav(revT(T));

print("\n binarySearch(T, 3): ");
binarySearch(T, 3);

print("\n binarySearch(T, 1): ");
binarySearch(T, 1);

print("\n binarySearch(T, 5): ");
binarySearch(T, 5);

print("\n binarySearch(T, 6): ");
binarySearch(T, 6);

print("\n binarySearch(T, 0): ");
binarySearch(T, 0);
