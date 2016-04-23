open Instr;;

module Vm = struct 


let rec push (stack,mem,fail) data n =
	match data with 
	| [] when n = 0 -> (stack, mem, fail)
	| d::dl when n > 0 -> push  (d::stack, mem, fail) dl (n-1)
	| [] when n > 0 -> failwith "Not done"
	| d::dl when n = 0 -> failwith "Not done"
;;


let apply_instr state msg i =
	match fst i with 
	| Instr.STOP -> state
	| Instr.ADD -> state
	| Instr.SUB -> state
	| Instr.MUL -> state
	| Instr.DIV -> state
	| Instr.SDIV -> state
	| Instr.MOD -> state
	| Instr.SMOD -> state
	| Instr.EXP -> state
	| Instr.NOT -> state
	| Instr.LT -> state
	| Instr.GT -> state
	| Instr.SLT -> state
	| Instr.SGT -> state
	| Instr.EQ -> state
	| Instr.ISZERO -> state
	| Instr.AND -> state
	| Instr.OR -> state
	| Instr.XOR -> state
	| Instr.BYTE -> state
	| Instr.ADDMOD -> state
	| Instr.MULMOD -> state
	| Instr.SIGNEXTEND -> state
	| Instr.SHA3 -> state
	| Instr.ADDRESS -> state
	| Instr.BALANCE -> state
	| Instr.ORIGIN -> state
	| Instr.CALLER -> state
	| Instr.CALLVALUE -> state
	| Instr.CALLDATALOAD -> state
	| Instr.CALLDATASIZE -> state
	| Instr.CALLDATACOPY -> state
	| Instr.CODESIZE -> state
	| Instr.CODECOPY -> state
	| Instr.GASPRICE -> state
	| Instr.EXTCODESIZE -> state
	| Instr.EXTCODECOPY -> state
	| Instr.BLOCKHASH -> state
	| Instr.COINBASE -> state
	| Instr.TIMESTAMP -> state
	| Instr.NUMBER -> state
	| Instr.DIFFICULTY -> state
	| Instr.GASLIMIT -> state
	| Instr.POP -> state
	| Instr.MLOAD -> state
	| Instr.MSTORE -> state
	| Instr.MSTORE8 -> state
	| Instr.MSIZE -> state
	| Instr.SLOAD -> state
	| Instr.SSTORE -> state
	| Instr.JUMP -> state
	| Instr.JUMPI -> state
	| Instr.PC -> state
	| Instr.GAS -> state
	| Instr.JUMPDEST -> state
	
	| Instr.PUSH1 | Instr.PUSH2 | Instr.PUSH3 | Instr.PUSH4 | Instr.PUSH5 | Instr.PUSH6 | Instr.PUSH7 | Instr.PUSH8 | Instr.PUSH9 | Instr.PUSH10 | Instr.PUSH11
	| Instr.PUSH12 | Instr.PUSH13 | Instr.PUSH14 | Instr.PUSH15 | Instr.PUSH16 | Instr.PUSH17 | Instr.PUSH18 | Instr.PUSH19 | Instr.PUSH20 | Instr.PUSH21
	| Instr.PUSH22 | Instr.PUSH23 | Instr.PUSH24 | Instr.PUSH25 | Instr.PUSH26 | Instr.PUSH27 | Instr.PUSH28 | Instr.PUSH29 | Instr.PUSH30 | Instr.PUSH31 
	| Instr.PUSH32
		->  let dsize = Instr.dsize_of_op (fst i) 
			in push state (snd i) dsize
		
	| Instr.DUP1 -> state
	| Instr.DUP2 -> state
	| Instr.DUP3 -> state
	| Instr.DUP4 -> state
	| Instr.DUP5 -> state
	| Instr.DUP6 -> state
	| Instr.DUP7 -> state
	| Instr.DUP8 -> state
	| Instr.DUP9 -> state
	| Instr.DUP10 -> state
	| Instr.DUP11 -> state
	| Instr.DUP12 -> state
	| Instr.DUP13 -> state
	| Instr.DUP14 -> state
	| Instr.DUP15 -> state
	| Instr.DUP16 -> state
	| Instr.SWAP1 -> state
	| Instr.SWAP2 -> state
	| Instr.SWAP3 -> state
	| Instr.SWAP4 -> state
	| Instr.SWAP5 -> state
	| Instr.SWAP6 -> state
	| Instr.SWAP7 -> state
	| Instr.SWAP8 -> state
	| Instr.SWAP9 -> state
	| Instr.SWAP10 -> state
	| Instr.SWAP11 -> state
	| Instr.SWAP12 -> state
	| Instr.SWAP13 -> state
	| Instr.SWAP14 -> state
	| Instr.SWAP15 -> state
	| Instr.SWAP16 -> state
	| Instr.LOG0 -> state
	| Instr.LOG1 -> state
	| Instr.LOG2 -> state
	| Instr.LOG3 -> state
	| Instr.LOG4 -> state
	| Instr.CREATE -> state
	| Instr.CALL -> state
	| Instr.CALLCODE -> state
	| Instr.RETURN -> state
	| Instr.DELEGATECALL -> state
	| Instr.SUICIDE -> state
	| Instr.INVALID -> state
;;

let init contract =
	0
;;

let apply state msg = 0
;;

end