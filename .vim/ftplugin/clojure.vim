silent! nmap <buffer> gd <Plug>(iced_def_jump)

silent! nmap <buffer> <LocalLeader>' <Plug>(iced_connect)
silent! nmap <buffer> <LocalLeader>" <Plug>(iced_jack_in)

let b:which_key_local_map =  {}
call which_key#register(',', "b:which_key_local_map")

"" Evaluating (<LocalLeader>e)
"" ------------------------------------------------------------------------
let b:which_key_local_map.e = { 'name' : '+Eval' ,
			\ 'q' : 'Interrupt',
			\ 'Q' : 'Interrupt all',
			\ 'i' : 'Eval inner',
			\ 'e' : 'Eval outer',
			\ 'f' : 'Eval top level form',
			\}

silent! nmap <buffer> <LocalLeader>eq <Plug>(iced_interrupt)
silent! nmap <buffer> <LocalLeader>eQ <Plug>(iced_interrupt_all)
silent! nmap <buffer> <LocalLeader>ei <Plug>(iced_eval_and_print)<Plug>(sexp_inner_element)``
silent! nmap <buffer> <LocalLeader>ee <Plug>(iced_eval_and_print)<Plug>(sexp_outer_list)``
silent! nmap <buffer> <LocalLeader>ef <Plug>(iced_eval_outer_top_list)
silent! vmap <buffer> <LocalLeader>ee <Plug>(iced_eval_visual)
silent! nmap <buffer> <LocalLeader>en <Plug>(iced_eval_ns)
silent! nmap <buffer> <LocalLeader>ep <Plug>(iced_print_last)
silent! nmap <buffer> <LocalLeader>eb <Plug>(iced_require)
silent! nmap <buffer> <LocalLeader>eB <Plug>(iced_require_all)
silent! nmap <buffer> <LocalLeader>eu <Plug>(iced_undef)
silent! nmap <buffer> <LocalLeader>eU <Plug>(iced_undef_all_in_ns)
silent! nmap <buffer> <LocalLeader>eM <Plug>(iced_macroexpand_outer_list)
silent! nmap <buffer> <LocalLeader>em <Plug>(iced_macroexpand_1_outer_list)

"" Testing (<LocalLeader>t)
"" ------------------------------------------------------------------------
let b:which_key_local_map.t = { 'name' : '+Test' }
silent! nmap <buffer> <LocalLeader>tt <Plug>(iced_test_under_cursor)
silent! nmap <buffer> <LocalLeader>tl <Plug>(iced_test_rerun_last)
silent! nmap <buffer> <LocalLeader>ts <Plug>(iced_test_spec_check)
silent! nmap <buffer> <LocalLeader>to <Plug>(iced_test_buffer_open)
silent! nmap <buffer> <LocalLeader>tn <Plug>(iced_test_ns)
silent! nmap <buffer> <LocalLeader>tp <Plug>(iced_test_all)
silent! nmap <buffer> <LocalLeader>tr <Plug>(iced_test_redo)

"" Stdout buffer (<LocalLeader>s)
"" ------------------------------------------------------------------------
let b:which_key_local_map.s = { 'name' : '+Stdout' }
silent! nmap <buffer> <LocalLeader>ss <Plug>(iced_stdout_buffer_open)
silent! nmap <buffer> <LocalLeader>sc <Plug>(iced_stdout_buffer_clear)
silent! nmap <buffer> <LocalLeader>sq <Plug>(iced_stdout_buffer_close)
silent! nmap <buffer> <LocalLeader>sn <Plug>(iced_eval_ns)

"" Refactoring (<LocalLeader>r)
"" ------------------------------------------------------------------------
let b:which_key_local_map.r = { 'name' : '+Refactor',
			\ 'a' : {'name' : '+add'},
			\ 'c' : {'name' : '+clean'},
			\ 't' : {'name' : '+thread'},
			\}

silent! nmap <buffer> <LocalLeader>rcn <Plug>(iced_clean_ns)
silent! nmap <buffer> <LocalLeader>rca <Plug>(iced_clean_all)
silent! nmap <buffer> <LocalLeader>ram <Plug>(iced_add_missing)
silent! nmap <buffer> <LocalLeader>ran <Plug>(iced_add_ns)
silent! nmap <buffer> <LocalLeader>raa <Plug>(iced_add_arity)
silent! nmap <buffer> <LocalLeader>rtf <Plug>(iced_thread_first)
silent! nmap <buffer> <LocalLeader>rtl <Plug>(iced_thread_last)
silent! nmap <buffer> <LocalLeader>ref <Plug>(iced_extract_function)
silent! nmap <buffer> <LocalLeader>rml <Plug>(iced_move_to_let)

"" Help/Document (<LocalLeader>h)
"" ------------------------------------------------------------------------

silent! nmap <buffer> K <Plug>(iced_document_popup_open)

let b:which_key_local_map.h = { 'name' : '+Help' }
silent! nmap <buffer> <LocalLeader>hb <Plug>(iced_document_open)
silent! nmap <buffer> <LocalLeader>hu <Plug>(iced_use_case_open)
silent! nmap <buffer> <LocalLeader>hn <Plug>(iced_next_use_case)
silent! nmap <buffer> <LocalLeader>hN <Plug>(iced_prev_use_case)
silent! nmap <buffer> <LocalLeader>hq <Plug>(iced_document_close)
silent! nmap <buffer> <LocalLeader>hS <Plug>(iced_source_show)
silent! nmap <buffer> <LocalLeader>hs <Plug>(iced_source_popup_show)
silent! nmap <buffer> <LocalLeader>hc <Plug>(iced_clojuredocs_open)
silent! nmap <buffer> <LocalLeader>hh <Plug>(iced_command_palette)


"" Browsing (<LocalLeader>b)
"" ------------------------------------------------------------------------
let b:which_key_local_map.b = { 'name' : '+Browse',
	\ 'v' : {'name' : '+var'},
	\}
silent! nmap <buffer> <LocalLeader>bn <Plug>(iced_browse_related_namespace)
silent! nmap <buffer> <LocalLeader>bs <Plug>(iced_browse_spec)
silent! nmap <buffer> <LocalLeader>bt <Plug>(iced_browse_test_under_cursor)
silent! nmap <buffer> <LocalLeader>br <Plug>(iced_browse_references)
silent! nmap <buffer> <LocalLeader>bd <Plug>(iced_browse_dependencies)
silent! nmap <buffer> <LocalLeader>bvr <Plug>(iced_browse_var_references)
silent! nmap <buffer> <LocalLeader>bvd <Plug>(iced_browse_var_dependencies)

"" Jumping cursor (<LocalLeader>j)
"" ------------------------------------------------------------------------
let b:which_key_local_map.j = { 'name' : '+Jump' }
silent! nmap <buffer> <C-]> <Plug>(iced_def_jump)
silent! nmap <buffer> <LocalLeader>jn <Plug>(iced_jump_to_next_sign)
silent! nmap <buffer> <LocalLeader>jN <Plug>(iced_jump_to_prev_sign)
silent! nmap <buffer> <LocalLeader>jl <Plug>(iced_jump_to_let)

"" Debugging (<LocalLeader>d)
"" ------------------------------------------------------------------------
let b:which_key_local_map.d = { 'name' : '+Debug' }
silent! nmap <buffer> <LocalLeader>dbt <Plug>(iced_browse_tapped)
silent! nmap <buffer> <LocalLeader>dlt <Plug>(iced_clear_tapped)

"" Misc
"" ------------------------------------------------------------------------
silent! nmap <buffer> == <Plug>(iced_format)
silent! nmap <buffer> =G <Plug>(iced_format_all)

silent! nmap <buffer> <LocalLeader>* <Plug>(iced_grep)
silent! nmap <buffer> <LocalLeader>/ :<C-u>IcedGrep<Space>
