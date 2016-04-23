module Parser =
	let contract_of_bytes -> string -> Instr.t list;;
	let contract_to_asm -> Instr.t list -> string;;
	let contract_to_bytes -> Instr.t list -> string;;
end