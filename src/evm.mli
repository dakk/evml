module EVM =
	(* Create the initial state of a given programm *)
	let init -> Contract -> State;;
	
	(* Apply code to a dapp state *)
	(* TODO need to pass a caller ENV with address / balance / gas *)
	let apply -> State -> Message -> State;;
	
	(* Evalute the gas cost for a call *)
	let estimate_gas -> State -> Message -> int;;
end