module Message = 
	type t = struct {
		gas		: int		;
		to		: string	;
		sender	: string	;
		value	: int		;
		data	: string	
	}
end
