
let dotprod v1 v2 = 
begin
  let sum = ref 0 in
		let i = ref 0 in
			let sz = size v1 in
				let rec loop = 
					if !i < sz then (i := !i + 1; 
      			sum := (get v1 i) * (get v2 i) + !sum; loop) else
							() 
    in loop; !sum
end
;;
