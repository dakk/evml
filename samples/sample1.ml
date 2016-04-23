let test1 = "000f" in
let c = contract_of_bytes test1 in
let istate = EVM.init c in
let rstate = EVM.apply istate m

;;