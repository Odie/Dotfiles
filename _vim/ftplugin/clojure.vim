" let b:which_key_local_map =  {}
" call which_key#register(',', "b:which_key_local_map")
"
" "" Evaluating (<LocalLeader>e)
" "" ------------------------------------------------------------------------
" let b:which_key_local_map.e = { 'name' : '+Eval' ,
" 			\ 'q' : 'Interrupt',
" 			\ 'Q' : 'Interrupt all',
" 			\ 'i' : 'Eval inner',
" 			\ 'e' : 'Eval outer',
" 			\ 'f' : 'Eval top level form',
" 			\}
"
"
" "" Testing (<LocalLeader>t)
" "" ------------------------------------------------------------------------
" let b:which_key_local_map.t = { 'name' : '+Test' }
"
" "" Stdout buffer (<LocalLeader>s)
" "" ------------------------------------------------------------------------
" let b:which_key_local_map.s = { 'name' : '+Stdout' }
"
" "" Refactoring (<LocalLeader>r)
" "" ------------------------------------------------------------------------
" let b:which_key_local_map.r = { 'name' : '+Refactor',
" 			\ 'a' : {'name' : '+add'},
" 			\ 'c' : {'name' : '+clean'},
" 			\ 't' : {'name' : '+thread'},
" 			\}
"
"
" "" Help/Document (<LocalLeader>h)
" "" ------------------------------------------------------------------------
"
" " silent! nmap <buffer> K <Plug>(iced_document_popup_open)
"
" let b:which_key_local_map.h = { 'name' : '+Help' }
"
"
" "" Browsing (<LocalLeader>b)
" "" ------------------------------------------------------------------------
" let b:which_key_local_map.b = { 'name' : '+Browse',
" 	\ 'v' : {'name' : '+var'},
" 	\}
"
" "" Jumping cursor (<LocalLeader>j)
" "" ------------------------------------------------------------------------
" let b:which_key_local_map.j = { 'name' : '+Jump' }
"
" "" Debugging (<LocalLeader>d)
" "" ------------------------------------------------------------------------
" let b:which_key_local_map.d = { 'name' : '+Debug' }
"
