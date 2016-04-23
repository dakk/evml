open Scanf;;
open Instr;;

module Parser = struct

let contract_of_bytes bs =
	let rec parse_chunks s c =
		match String.length s with
		| 0 -> c
		| 1 -> c
		| n -> 
			let chunk = String.sub s 0 2 in
			let s' = String.sub s 2 (n-2) in
			let op = Instr.hex_to_op (Scanf.sscanf chunk "%x" (fun x -> x)) in
				let rec get_data sd d n =
					match n with
					| 0 -> d
					| n -> 
						let dchunk = Scanf.sscanf (String.sub sd 0 2) "%x" (fun x -> x) in
						let s'' = String.sub sd 2 (n-2) in
						get_data s'' (d @ [dchunk]) (n-1)						
				in
				let isize = Instr.dsize_of_op op in
				parse_chunks s' (Instr.create (op,(get_data s' [] isize))::c)
	in
		parse_chunks bs []

let rec contract_to_asm il =
	match il with
	| i::il' -> (Instr.to_asm i) ^ "\n" ^ (contract_to_asm il')
	| [] -> "\n"

let rec contract_to_bytes il =
	match il with
	| i::il' -> 
		(Instr.to_hex i) ^ (contract_to_bytes il')
	| [] -> ""

end