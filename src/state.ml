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

module Memory = 
	type t = ;;
	
	let mem_set i el = ();;
	
	let mem_get i = None;;
end

type FailCode = OUTOFGAS | BADJUMPDEST | BADINSTR | STACKUNDERFLOW | OUTOFSTACK | NONE ;;

module State =
	type t = struct {
		mem		: Memory.t		;
		stack	: Stack.t		;
		fail	: FailCode
	}
end
