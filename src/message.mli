module Message : sig =
	type t = struct {
		origin		: Address.t	
		code_address: Address.t	
		sender		: Address.it
		code		: option
		data		: option
		value		: u256
		gas			: u256
		gas_price	: u256
	}
with struct =
end