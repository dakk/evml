module Parser =
	let contract_of_bytes -> string -> Instr list;;
	let contract_to_asm -> Instr list -> string;;
	let contract_to_bytes -> Instr list -> string;;
end