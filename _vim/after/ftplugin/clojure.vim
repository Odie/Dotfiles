" after/ftplugin/clojure.vim

augroup clojure_user_settings
	autocmd!
	" For unknown reasons, this does not work if we hook into the
	" FileType event
	autocmd BufEnter <buffer> RainbowToggleOn
augroup END

" nnoremap <buffer> ,' :IcedConnect<cr>
" nnoremap <buffer> ,sq :IcedDisconnect<cr>
" nnoremap <buffer> ,sn :IcedEvalNs<cr>
" nnoremap <buffer> ,ef :IcedEvalOuterTopList<cr>
" nnoremap <buffer> ,ee <Plug>(iced_eval_and_tap)<Plug>(sexp_outer_list)``
"
" nmap <buffer> ,sc <Plug>(iced_stdout_buffer_clear)
