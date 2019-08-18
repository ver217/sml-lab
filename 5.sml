datatype tree = Empty | Node of tree * int * tree;

fun treecmp (Empty, Empty) = EQUAL
  | treecmp (Empty, t) = LESS
  | treecmp (t, Empty) = GREATER
  | treecmp (Node(t1, x, t2), Node(t3, y, t4)) = Int.compare(x, y);

treecmp (Node(Empty, 1, Empty), Node(Empty, 2, Empty));
treecmp (Empty, Node(Empty, 2, Empty));
treecmp (Empty, Empty);
treecmp (Node(Empty, 1, Empty), Empty);
treecmp (Node(Empty, 2, Empty), Node(Empty, 2, Empty));
treecmp (Node(Empty, 2, Empty), Node(Empty, 1, Empty));


fun swapDown Empty = Empty
  | swapDown (Node(Empty, x, Empty)) = Node(Empty, x, Empty)
  | swapDown (Node(Node(t1, y, t2), x, Empty)) = if y < x then
                                                Node(swapDown(Node(t1, x, t2)), y, Empty)
                                               else Node(Node(t1, y, t2), x, Empty)
  | swapDown (Node(Node(t1, y, t2), x, Node(t3, z, t4))) = if y < z then
                                                            if y < x then
                                                              Node(swapDown(Node(t1, x, t2)), y, Node(t3, z, t4))
                                                            else Node(Node(t1, y, t2), x, Node(t3, z, t4))
                                                          else
                                                            if z < x then 
                                                              Node(Node(t1, y, t2), z, swapDown(Node(t3, x, t4)))
                                                            else Node(Node(t1, y, t2), x, Node(t3, z, t4));
