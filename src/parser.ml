open Sscanf;;

let contract_of_bytes bs =
	let rec parse_chunks s c =
		match String.length s with
		| 0 -> c
		| 1 -> c
		| n -> 
			let chunk = String.sub s 0 2 in
			let s' = String.sub s 2 n-2 in
			let i = hex_to_instr (Scanf.sscanf chunk "%x" (fun x -> x)) in
				parse_chunks s' (i::c)
	in
		prase_chunks bs []
;;


let rec contract_to_asm il =
	match il with
	| i::il' -> (instr_to_string i) ^ "\n" ^ (contract_to_asm il')
	| [] -> "\n"
;;

