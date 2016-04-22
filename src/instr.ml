type Instr = 
	  STOP
	| ADD
	| SUB
	| MUL
	| DIV
	| SDIV
	| MOD
	| SMOD
	| EXP
	| NOT
	| LT
	| GT
	| SLT
	| SGT
	| EQ
	| ISZERO
	| AND
	| OR
	| XOR
	| BYTE
	| ADDMOD
	| MULMOD
	| SIGNEXTEND
	| SHA3
	| ADDRESS
	| BALANCE
	| ORIGIN
	| CALLER
	| CALLVALUE
	| CALLDATALOAD
	| CALLDATASIZE
	| CALLDATACOPY
	| CODESIZE
	| CODECOPY
	| GASPRICE
	| EXTCODESIZE
	| EXTCODECOPY
	| BLOCKHASH
	| COINBASE
	| TIMESTAMP
	| NUMBER
	| DIFFICULTY
	| GASLIMIT
	| POP
	| MLOAD
	| MSTORE
	| MSTORE8
	| MSIZE
	| SLOAD
	| SSTORE
	| JUMP
	| JUMPI
	| PC
	| GAS
	| JUMPDEST
	| PUSH1
	| PUSH2
	| PUSH3
	| PUSH4
	| PUSH5
	| PUSH6
	| PUSH7
	| PUSH8
	| PUSH9
	| PUSH10
	| PUSH11
	| PUSH12
	| PUSH13
	| PUSH14
	| PUSH15
	| PUSH16
	| PUSH17
	| PUSH18
	| PUSH19
	| PUSH20
	| PUSH21
	| PUSH22
	| PUSH23
	| PUSH24
	| PUSH25
	| PUSH26
	| PUSH27
	| PUSH28
	| PUSH29
	| PUSH30
	| PUSH31
	| PUSH32
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



type GasTier = ZERO | BASE | VERY_LOW | LOW | MID | HIGH | EXT | SPECIAL | INVALID;;

let gas_price t = 
	| ZERO -> 0
	| BASE -> 1
	| VERY_LOW -> 2
	| LOW -> 3
	| MID -> 4
	| HIGH -> 5
	| EXT -> 6
	| SPECIAL -> 7
	| INVALID -> 8
;;

let gas_of_instr i =
	match i with
	| STOP | RETURN | SUICIDE ->
		gas_price ZERO

	| COINBASE | TIMESTAMP | NUMBER | DIFFICULTY | GASLIMIT | POP | MSIZE | PC | GAS
	| ADDRESS | ORIGIN | CALLER | CALLVALUE | CODESIZE | GASPRICE | CALLDATASIZE
		-> gas_price BASE
	
	| PUSH1 | PUSH2 | PUSH3 | PUSH4 | PUSH5 | PUSH6 | PUSH7 | PUSH8 | PUSH9 | PUSH10 | PUSH11
	| PUSH12 | PUSH13 | PUSH14 | PUSH15 | PUSH16 | PUSH17 | PUSH18 | PUSH19 | PUSH20 | PUSH21
	| PUSH22 | PUSH23 | PUSH24 | PUSH25 | PUSH26 | PUSH27 | PUSH28 | PUSH29 | PUSH30 | PUSH31 
	| PUSH32 | DUP1 | DUP2 | DUP3 | DUP4 | DUP5 | DUP6 | DUP7 | DUP8 | DUP9 | DUP10 | DUP11
	| DUP12 | DUP13 | DUP14 | DUP15 | DUP16 | SWAP1 | SWAP2 | SWAP3 | SWAP4 | SWAP5 | SWAP6
	| SWAP7 | SWAP8 | SWAP9 | SWAP10 | SWAP11 | SWAP12 | SWAP13 | SWAP14 | SWAP15 | SWAP16 
	| MSTORE | MSTORE8 | NOT | LT | GT | SLT | SGT | EQ | ISZERO | AND | OR | XOR | BYTE
	| ADD | SUB | CALLDATALOAD | CALLDATACOPY | CODECOPY
		-> gas VERYLOW 

	| MUL | DIV | SDIV | MOD | SMOD | SIGNEXTEND
		-> gas_price LOW	

	| ADDMOD | MULMOD | JUMP
		-> gas_price MID

	| JUMPI
		-> gas_price HIGH

	| EXTCODESIZE | EXTCODECOPY | BLOCKHASH | BALANCE
		-> gas_price EXT	

	| MLOAD | SLOAD | SSTORE | JUMPDEST | LOG0 | LOG1 | LOG2 | LOG3 | LOG4 | CREATE | CALL 
	| CALLCODE | DELEGATECALL | EXP | SHA3
		-> gas_price SPECIAL
		
	| INVALID 
		-> gas_price INVALID
;;


let hex_of_instr i =
	match i with
	| STOP -> 0x00
	| ADD -> 0x01
	| SUB -> 0x03
	| MUL -> 0x02
	| DIV -> 0x04
	| SDIV -> 0x05
	| MOD -> 0x06
	| SMOD -> 0x07
	| EXP -> 0x0a
	| NOT -> 0x19
	| LT -> 0x10
	| GT -> 0x11
	| SLT -> 0x12
	| SGT -> 0x13
	| EQ -> 0x14
	| ISZERO -> 0x15
	| AND -> 0x16
	| OR -> 0x17
	| XOR -> 0x18
	| BYTE -> 0x1a
	| ADDMOD -> 0x08
	| MULMOD -> 0x09
	| SIGNEXTEND -> 0x0b
	| SHA3 -> 0x20
	| ADDRESS -> 0x30
	| BALANCE -> 0x31
	| ORIGIN -> 0x32
	| CALLER -> 0x33
	| CALLVALUE -> 0x34
	| CALLDATALOAD -> 0x35
	| CALLDATASIZE -> 0x36
	| CALLDATACOPY -> 0x37
	| CODESIZE -> 0x38
	| CODECOPY -> 0x39
	| GASPRICE -> 0x3a
	| EXTCODESIZE -> 0x3b
	| EXTCODECOPY -> 0x3c
	| BLOCKHASH -> 0x40
	| COINBASE -> 0x41
	| TIMESTAMP -> 0x42
	| NUMBER -> 0x43
	| DIFFICULTY -> 0x44
	| GASLIMIT -> 0x45
	| POP -> 0x50
	| MLOAD -> 0x51
	| MSTORE -> 0x52
	| MSIZE -> 0x59
	| MSTORE8 -> 0x53
	| SLOAD -> 0x54
	| SSTORE -> 0x55
	| JUMP -> 0x56
	| JUMPI -> 0x57
	| PC -> 0x58
	| GAS -> 0x5a
	| JUMPDEST -> 0x5b
	| PUSH1 -> 0x60
	| PUSH2 -> 0x61
	| PUSH3 -> 0x62
	| PUSH4 -> 0x63
	| PUSH5 -> 0x64
	| PUSH6 -> 0x65
	| PUSH7 -> 0x66
	| PUSH8 -> 0x67
	| PUSH9 -> 0x68
	| PUSH10 -> 0x69
	| PUSH11 -> 0x6a
	| PUSH12 -> 0x6b
	| PUSH13 -> 0x6c
	| PUSH14 -> 0x6d
	| PUSH15 -> 0x6e
	| PUSH16 -> 0x6f
	| PUSH17 -> 0x70
	| PUSH18 -> 0x71
	| PUSH19 -> 0x72
	| PUSH20 -> 0x73
	| PUSH21 -> 0x74
	| PUSH22 -> 0x75
	| PUSH23 -> 0x76
	| PUSH24 -> 0x77
	| PUSH25 -> 0x78
	| PUSH26 -> 0x79
	| PUSH27 -> 0x7a
	| PUSH28 -> 0x7b
	| PUSH29 -> 0x7c
	| PUSH30 -> 0x7d
	| PUSH31 -> 0x7e
	| PUSH32 -> 0x7f
	| DUP1 -> 0x80
	| DUP2 -> 0x81
	| DUP3 -> 0x82
	| DUP4 -> 0x83
	| DUP5 -> 0x84
	| DUP6 -> 0x85
	| DUP7 -> 0x86
	| DUP8 -> 0x87
	| DUP9 -> 0x88
	| DUP10 -> 0x89
	| DUP11 -> 0x8a
	| DUP12 -> 0x8b
	| DUP13 -> 0x8c
	| DUP14 -> 0x8d
	| DUP15 -> 0x8e
	| DUP16 -> 0x8f
	| SWAP1 -> 0x90
	| SWAP2 -> 0x91
	| SWAP3 -> 0x92
	| SWAP4 -> 0x93
	| SWAP5 -> 0x94
	| SWAP6 -> 0x95
	| SWAP7 -> 0x96
	| SWAP8 -> 0x97
	| SWAP9 -> 0x98
	| SWAP10 -> 0x99
	| SWAP11 -> 0x9a
	| SWAP12 -> 0x9b
	| SWAP13 -> 0x9c
	| SWAP14 -> 0x9d
	| SWAP15 -> 0x9e
	| SWAP16 -> 0x9f
	| LOG0 -> 0xa0
	| LOG1 -> 0xa1
	| LOG2 -> 0xa2
	| LOG3 -> 0xa3
	| LOG4 -> 0xa4
	| CREATE -> 0xf0
	| CALL -> 0xf1
	| CALLCODE -> 0xf2
	| RETURN -> 0xf3
	| DELEGATECALL -> 0xf4
	| SUICIDE -> 0xff
;;


let instr_of_hex h = 
	match h with
	| 0x00 -> STOP
	| 0x01 -> ADD
	| 0x03 -> SUB
	| 0x02 -> MUL
	| 0x04 -> DIV
	| 0x05 -> SDIV
	| 0x06 -> MOD
	| 0x07 -> SMOD
	| 0x0a -> EXP
	| 0x19 -> NOT
	| 0x10 -> LT
	| 0x11 -> GT
	| 0x12 -> SLT
	| 0x13 -> SGT
	| 0x14 -> EQ
	| 0x15 -> ISZERO
	| 0x16 -> AND
	| 0x17 -> OR
	| 0x18 -> XOR
	| 0x1a -> BYTE
	| 0x08 -> ADDMOD
	| 0x09 -> MULMOD
	| 0x0b -> SIGNEXTEND
	| 0x20 -> SHA3
	| 0x30 -> ADDRESS
	| 0x31 -> BALANCE
	| 0x32 -> ORIGIN
	| 0x33 -> CALLER
	| 0x34 -> CALLVALUE
	| 0x35 -> CALLDATALOAD
	| 0x36 -> CALLDATASIZE
	| 0x37 -> CALLDATACOPY
	| 0x38 -> CODESIZE
	| 0x39 -> CODECOPY
	| 0x3a -> GASPRICE
	| 0x3b -> EXTCODESIZE
	| 0x3c -> EXTCODECOPY
	| 0x40 -> BLOCKHASH
	| 0x41 -> COINBASE
	| 0x42 -> TIMESTAMP
	| 0x43 -> NUMBER
	| 0x44 -> DIFFICULTY
	| 0x45 -> GASLIMIT
	| 0x50 -> POP
	| 0x51 -> MLOAD
	| 0x52 -> MSTORE
	| 0x59 -> MSIZE
	| 0x53 -> MSTORE8
	| 0x54 -> SLOAD
	| 0x55 -> SSTORE
	| 0x56 -> JUMP
	| 0x57 -> JUMPI
	| 0x58 -> PC
	| 0x5a -> GAS
	| 0x5b -> JUMPDEST
	| 0x60 -> PUSH1
	| 0x61 -> PUSH2
	| 0x62 -> PUSH3
	| 0x63 -> PUSH4
	| 0x64 -> PUSH5
	| 0x65 -> PUSH6
	| 0x66 -> PUSH7
	| 0x67 -> PUSH8
	| 0x68 -> PUSH9
	| 0x69 -> PUSH10
	| 0x6a -> PUSH11
	| 0x6b -> PUSH12
	| 0x6c -> PUSH13
	| 0x6d -> PUSH14
	| 0x6e -> PUSH15
	| 0x6f -> PUSH16
	| 0x70 -> PUSH17
	| 0x71 -> PUSH18
	| 0x72 -> PUSH19
	| 0x73 -> PUSH20
	| 0x74 -> PUSH21
	| 0x75 -> PUSH22
	| 0x76 -> PUSH23
	| 0x77 -> PUSH24
	| 0x78 -> PUSH25
	| 0x79 -> PUSH26
	| 0x7a -> PUSH27
	| 0x7b -> PUSH28
	| 0x7c -> PUSH29
	| 0x7d -> PUSH30
	| 0x7e -> PUSH31
	| 0x7f -> PUSH32
	| 0x80 -> DUP1
	| 0x81 -> DUP2
	| 0x82 -> DUP3
	| 0x83 -> DUP4
	| 0x84 -> DUP5
	| 0x85 -> DUP6
	| 0x86 -> DUP7
	| 0x87 -> DUP8
	| 0x88 -> DUP9
	| 0x89 -> DUP10
	| 0x8a -> DUP11
	| 0x8b -> DUP12
	| 0x8c -> DUP13
	| 0x8d -> DUP14
	| 0x8e -> DUP15
	| 0x8f -> DUP16
	| 0x90 -> SWAP1
	| 0x91 -> SWAP2
	| 0x92 -> SWAP3
	| 0x93 -> SWAP4
	| 0x94 -> SWAP5
	| 0x95 -> SWAP6
	| 0x96 -> SWAP7
	| 0x97 -> SWAP8
	| 0x98 -> SWAP9
	| 0x99 -> SWAP10
	| 0x9a -> SWAP11
	| 0x9b -> SWAP12
	| 0x9c -> SWAP13
	| 0x9d -> SWAP14
	| 0x9e -> SWAP15
	| 0x9f -> SWAP16
	| 0xa0 -> LOG0
	| 0xa1 -> LOG1
	| 0xa2 -> LOG2
	| 0xa3 -> LOG3
	| 0xa4 -> LOG4
	| 0xf0 -> CREATE
	| 0xf1 -> CALL
	| 0xf2 -> CALLCODE
	| 0xf3 -> RETURN
	| 0xf4 -> DELEGATECALL
	| 0xff -> SUICIDE
;;


