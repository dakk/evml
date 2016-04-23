open U256;;

module Address : sig
	type t
end

module Message : sig
	type t = {
		origin		: Address.t		;	
		code_address: Address.t		;
		sender		: Address.t		;
		code		: int option	;
		data		: int option	;
		value		: U256.t		;
		gas			: U256.t		;
		gas_price	: U256.t		;
	}
end