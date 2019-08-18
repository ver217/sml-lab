fun factorial (0) = 1
  | factorial (x) = x * factorial(x - 1);

fun thenAddOne(func, 0) = 2
  | thenAddOne(func, x) = func(x) + 1;

fun mapList(func, []) = []
  | mapList(func, x::L) = func(x)::mapList(func, L);

fun mapList'(func) = fn L => case L of
                    []=>[]
                    |x::R =>(factorial x)::(mapList' factorial R);

fun exist f [] = false
    |exist f(x::L) =
        case  f(x) of
        true => true
        |false => exist f(L)

fun forall f [] = true
    |forall f(x::L) = 
        case f(x) of
        true => forall f(L)
        |false => false


datatype 'a tree = Empty | Node of 'a tree * 'a * 'a tree;

fun treeFliter f = fn T=>
    case T of
        Empty => Empty
        |Node(l,x,r) => if (f x) then
            Node(treeFliter f l,SOME x,treeFliter f r)
        else 
            Node(treeFliter f l,NONE,treeFliter f r);
