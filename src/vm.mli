open Instr;;

module Vm : sig
	(* Create the initial state of a given programm *)
	(*val init 			: Instr.contract -> State*)
	
	(* Apply code to a dapp state *)
	(*val apply			: State -> Message -> State*)
	
	(* Evalute the gas cost for a call *)
	(*val estimate_gas 	: State -> Message -> int*)
end