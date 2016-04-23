open Instr;;

module Parser : sig
	val contract_of_bytes : string -> Instr.t list
	val contract_to_asm   : Instr.t list -> string
	val contract_to_bytes : Instr.t list -> string
end