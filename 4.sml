fun next(xlist, y::ys) = 
  if hd xlist <= y
    then next(y::xlist, ys)
    else let fun swap [x] = y::x::ys
               | swap (x::xk::xs) = if xk > y then x::swap(xk::xs) else
                 (y::xk::xs)@(x::xs)
            in swap(xlist)
         end;

fun nextp(y::ys) = next([y], ys);

nextp [2,3,1,4];
