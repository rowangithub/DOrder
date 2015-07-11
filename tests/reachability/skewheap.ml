type 'a tree = 
	| Empty 
	| Node of 'a tree * 'a * 'a tree


let rec meld a b =
	match (a,b) with 
		| (Empty, b) -> b 
		| (a, Empty) -> a
 		| (Node(al, ak, ar), Node(bl, bk, br)) ->
 			if (ak <= bk) then 
				Node((meld ar b), ak, al)
 			else 
				Node((meld a br), bk, bl)

let insert k a =
	let n = Node(Empty, k, Empty) in
	match a with
		| Empty -> n
		| Node (x,y,z) -> meld a n

let findmin (phantom:bool) a : int =
	match a with 
		(*| Empty -> failwith "Findmin_on_empty_heap"*)
 		| Node(al, ak, ar) -> ak

let deletemin a =
	match a with 
		(*| Empty -> failwith "Deletemin_on_empty_heap"*)
 		| Node(al, ak, ar) -> meld al ar

let harness () = (insert 0 Empty; findmin true Empty; deletemin Empty)