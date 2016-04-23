let apply_instr state msg i =
	match i.op with 
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

	| DUP1
	| DUP2
	| DUP3
	| DUP4
	| DUP5
	| DUP6
	| DUP7
	| DUP8
	| DUP9
	| DUP10
	| DUP11
	| DUP12
	| DUP13
	| DUP14
	| DUP15
	| DUP16
	| SWAP1
	| SWAP2
	| SWAP3
	| SWAP4
	| SWAP5
	| SWAP6
	| SWAP7
	| SWAP8
	| SWAP9
	| SWAP10
	| SWAP11
	| SWAP12
	| SWAP13
	| SWAP14
	| SWAP15
	| SWAP16
	| LOG0
	| LOG1
	| LOG2
	| LOG3
	| LOG4
	| CREATE
	| CALL
	| CALLCODE
	| RETURN
	| DELEGATECALL
	| SUICIDE
	| INVALID
;;

let init contract =
	0
;;

let apply state msg = 
;;
