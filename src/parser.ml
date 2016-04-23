open Scanf;;
open Instr;;

module Parser = struct

let contract_of_bytes bs =
	let rec parse_chunks s c =
		match String.length s with
		| 0 -> c
		| 1 -> c
		| n -> 
			let inn = Instr.of_bytes s in
			parse_chunks (snd inn) (c @ [fst inn])
	in
		parse_chunks bs []
;;

let rec contract_to_asm il =
	match il with
	| i::il' -> (Instr.to_asm i) ^ "\n" ^ (contract_to_asm il')
	| [] -> "\n"
;;

let rec contract_to_bytes il =
	match il with
	| i::il' -> 
		(Instr.to_hex i) ^ (contract_to_bytes il')
	| [] -> ""
;;

end