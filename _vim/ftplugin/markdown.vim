let b:trim_trailing_spaces=0    " Do not trim trailing spaces because they are significant
setlocal nolist                 " Hide hidden characters for visual clarity
setlocal tabstop=2 softtabstop=0 expandtab shiftwidth=2 smarttab
setlocal textwidth=79           " Wrap lines automatically
setlocal foldmethod=expr        " Fold at major headings
setlocal foldexpr=OrderedListFold(v:lnum)

" Search backwards for a line that starts with a number
function! NextNumberHeading(lnum)
	let numlines = line('$')
	let current = a:lnum + 1

	while current <= numlines
		if getline(current) =~? '\v^\d+\.'
			return current
		endif

		let current += 1
	endwhile

	return -2
endfunction

function! IsNumberHeading(lnum)
	if getline(a:lnum) =~? '\v^\d+\.'
		return 1
	else
		return 0
	endif
endfunction

function! OrderedListFold(lnum)
	let current = a:lnum

	" We're interested in folding markdown that's being explicitly or
	" auto-numbered.
	" This means we're only interested in classifying lines into the
	" following categories:
	" 1. A number heading
	" 2. Not a number heading
	if IsNumberHeading(current+1)
		return 0
	else
		return 1
	endif
endfunction

" Unfold everything on load
normal! zR
