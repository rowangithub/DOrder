fun('a)
    append ([], ys) = ys
  | append (x :: xs, ys) = x :: append(xs, ys)
withtype {m:nat, n:nat} 'a list(m)*'a list(n)->'a list(m+n)
