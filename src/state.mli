module Memory : sig 
	type t = int list;;

	val mem_set : int -> int option -> unit
	val mem_get : int -> int option
with struct =
	let mem_set i el = ();;	
	let mem_get i = None;;
end


module Stack = 
	type t = int list;

	let stack_push stack element = element :: stack;;

	let stack_pop stack = 
		match stack with
			| []    -> (None, [])
			| x::xl -> (Some (x), xl)
	;; 	 
	
	let stack_new () = [];;
end


module State : sig
	type Fail = OUTOFGAS | BADJUMPDEST | BADINSTR | STACKUNDERFLOW | OUTOFSTACK | NONE ;;

	type t = Stack.t * Memory.t * Fail
end



end