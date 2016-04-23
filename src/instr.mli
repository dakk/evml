module Instr : sig
	type op
	type t
	type contract
	
	val of_bytes: string -> t * string
	val to_hex 	: t -> string
	val to_asm 	: t -> string
end
