
type 'a mlist = {
  mutable value : 'a;
  mutable next  : 'a mlist;
}

let new_node x = 
  let rec n = { value = x; next = n } in
  n

let rec nth node n =
  if n <= 0 then node else nth node.next n

let rec insert node node' k =
  if k > 0 then insert node.next node' (k-1) else 
    (node'.next <- node.next ; node.next <- node')

let rec printff f node =
    let _ = print_string (" -> "^(f (node.value))) in
    if not (node == node.next) then printff f node.next 

let rec mkml xs = 
  match xs with 
  | [] -> 
      assert false 
  | [x] -> 
      new_node x
  | x::xs' -> 
      let n = new_node x in 
      let _ = n.next <- (mkml xs') in
      n

let m1 = mkml [1;2;3]

let _ = printff string_of_int m1 

