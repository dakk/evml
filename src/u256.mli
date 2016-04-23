open Unsigned;;

module U256 : sig
	type t = Unsigned.UInt64.t * Unsigned.UInt64.t * Unsigned.UInt64.t * Unsigned.UInt64.t 

	val u256_add	:	t -> t -> t
end
