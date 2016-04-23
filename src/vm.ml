module Vm = struct 

let apply_instr state msg i =
	match fst i with 
	| STOP -> state
	| ADD -> state
	| SUB -> state
	| MUL -> state
	| DIV -> state
	| SDIV -> state
	| MOD -> state
	| SMOD -> state
	| EXP -> state
	| NOT -> state
	| LT -> state
	| GT -> state
	| SLT -> state
	| SGT -> state
	| EQ -> state
	| ISZERO -> state
	| AND -> state
	| OR -> state
	| XOR -> state
	| BYTE -> state
	| ADDMOD -> state
	| MULMOD -> state
	| SIGNEXTEND -> state
	| SHA3 -> state
	| ADDRESS -> state
	| BALANCE -> state
	| ORIGIN -> state
	| CALLER -> state
	| CALLVALUE -> state
	| CALLDATALOAD -> state
	| CALLDATASIZE -> state
	| CALLDATACOPY -> state
	| CODESIZE -> state
	| CODECOPY -> state
	| GASPRICE -> state
	| EXTCODESIZE -> state
	| EXTCODECOPY -> state
	| BLOCKHASH -> state
	| COINBASE -> state
	| TIMESTAMP -> state
	| NUMBER -> state
	| DIFFICULTY -> state
	| GASLIMIT -> state
	| POP -> state
	| MLOAD -> state
	| MSTORE -> state
	| MSTORE8 -> state
	| MSIZE -> state
	| SLOAD -> state
	| SSTORE -> state
	| JUMP -> state
	| JUMPI -> state
	| PC -> state
	| GAS -> state
	| JUMPDEST -> state
	| PUSH1 | PUSH2 | PUSH3 | PUSH4 | PUSH5 | PUSH6 | PUSH7 | PUSH8 | PUSH9 | PUSH10 | PUSH11
	| PUSH12 | PUSH13 | PUSH14 | PUSH15 | PUSH16 | PUSH17 | PUSH18 | PUSH19 | PUSH20 | PUSH21
	| PUSH22 | PUSH23 | PUSH24 | PUSH25 | PUSH26 | PUSH27 | PUSH28 | PUSH29 | PUSH30 | PUSH31 
	| PUSH32
		-> 
			let dsize = dsize_of_op i.op in
			state
	| DUP1 -> state
	| DUP2 -> state
	| DUP3 -> state
	| DUP4 -> state
	| DUP5 -> state
	| DUP6 -> state
	| DUP7 -> state
	| DUP8 -> state
	| DUP9 -> state
	| DUP10 -> state
	| DUP11 -> state
	| DUP12 -> state
	| DUP13 -> state
	| DUP14 -> state
	| DUP15 -> state
	| DUP16 -> state
	| SWAP1 -> state
	| SWAP2 -> state
	| SWAP3 -> state
	| SWAP4 -> state
	| SWAP5 -> state
	| SWAP6 -> state
	| SWAP7 -> state
	| SWAP8 -> state
	| SWAP9 -> state
	| SWAP10 -> state
	| SWAP11 -> state
	| SWAP12 -> state
	| SWAP13 -> state
	| SWAP14 -> state
	| SWAP15 -> state
	| SWAP16 -> state
	| LOG0 -> state
	| LOG1 -> state
	| LOG2 -> state
	| LOG3 -> state
	| LOG4 -> state
	| CREATE -> state
	| CALL -> state
	| CALLCODE -> state
	| RETURN -> state
	| DELEGATECALL -> state
	| SUICIDE -> state
	| INVALID -> state
;;

let init contract =
	0
;;

let apply state msg = 0
;;

end