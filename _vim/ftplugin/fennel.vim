call which_key#register(',', "b:which_key_local_map")
let b:which_key_local_map =  {}

let b:which_key_local_map.e = { 'name' : '+Eval' ,
			\ '!' : 'Eval & Replace',
			\ 'b' : 'Eval buffer',
			\ 'e' : 'Eval current form',
			\ 'E' : 'Eval selection',
			\ 'r' : 'Eval top level form',
			\ 'm' : 'Eval at mark',
			\ 'w' : 'Eval word',
			\}

let b:which_key_local_map.l = { 'name' : '+log' ,
			\ 'r' : 'Soft Reset',
			\ 'R' : 'Hard Reset',
			\ 's' : 'in split',
			\ 't' : 'in tab',
			\ 'v' : 'in vertical split',
			\ 'q' : 'quit',
			\}

