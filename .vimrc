runtime! archlinux.vim "TODO: Is this necessary if we're gonna source defaults.vim?

unlet! skip_defaults_vim " defaults.vim has an 'abort early' check if this flag is set. Unset it to be extra sure, I guess. Maybe this is for some sort of idempotency thing?
source $VIMRUNTIME/defaults.vim

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" # Here are the settings which I 'understand' from default.vim and which I
" agree with:

" set nocomptabile                " Don't care about compatibility with vi
" set backspace=indent,eol,start  " backspace behaviour. `indent` means we can backspace over autoindents. `eol` means we can join lines together. `start` means we can backspace before the point where we entered insert mode.
" set history=200                 " How many commands and searches to remember in the history
" set ruler                       " Show cursor position in bottom right corner.
" set showcmd                     " Shows incomplete commands (i.e. situations where vim is waiting for you to type more before it does something) in the bottom right corner.
" set wildmenu                    " When using tab-completion, and there are multiple results, shows a little menu that lets you pick the one you want.
" set incsearch                   " incremental search; i.e. jump to the best match as you provide each letter of your search string, instead of waiting for you to submit a full string before search.
" syntax on                       " Enables syntax highlighting.
" filetype plugin indent on       " Allows vim to guess the syntax of the file, and then apply different plugins depending on the detect type, and to apply different indentation rules depending on the detected type.

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" # Here are the custom configurations I like.

" ## Visual Options

set number                          " Show line numbers
set linebreak                       " By default, visual word wrapping is enabled. However, the wrapping can happen in the middle of words. With linebreak on (default off), we only break at 'word boundaries'. What is or isn't a word-boundary isdefined by the 'breakat' setting, which here we leave at the default.
set cursorline                      " highlight current the cursor is on.
set list                            " Enables showing whitespace
set listchars=tab:→→,trail:␣,nbsp:␣ " Controls what characters are used to display the whitespaces.

" ## Movement Options

" By default, if you're at the start of the line, and you hit the left cursor
" key, vim just beeps at you. You'd have to use the backspace to move back to
" the previous line, and the space key to move beyond the end of the current
" line and into the next line. That's what the `b` and `s` values (enabled by
" default) do. The `<` and `>` allow the left and right cursor keys to also
" wrap across lines in normal mode. The `[` and `]` allow the cursor keys to
" wrap across lines in insert mode.
set whichwrap=b,s,<,>,[,]

" If the required features are available (syntax parsing, I guess; not sure
" about the other one), then add the `matchit` package. The `%` command lets you
" jump to 'matching elements'. For example, if your cursor is on a `(`, then
" `%` will cause you to jump to the matching `)`. The `matchit` package makes
" this smarter, allowing to jump to the matching HTML tags, matching if/else
" branches, etc.
if has('syntax') && has('eval')
	packadd! matchit
endif

" ## Searching Options

set hlsearch   " After a search is complete and you're scrolling around, keep the search results highlighted.
set ignorecase " Do case insentive searches by default.
set smartcase  " However, if the search query contains an upper case character, switch back to case-sensitive searches.

" ## Editing Options

set showmatch   " When inserting a bracket, briefly highlight the matching bracket. Brackets here is defined by the `matchpairs` setting which this file leaves as default.

" ### Editing Indentation.

set autoindent  " When hitting enter on a line, automatically intend the newly created line to have the same indent as the line you just left. If you move away from the newly indented line without inserting any characters, those indentation are deleted so that you end up with an empty line (i.e. no trailing whitespace).
set smartindent " Makes autoident smarter; uses language-specific knowledge (e.g. checking if a new scope is being introduced) to indent the newly created line one level higher or lower than the previous line.


" The combination of the next three settings setting basically mean:
" * Use tab for indentiation
" * Tabs have a width of 2.
set tabstop=2    " When a file contains a tab, vim will now interpret this to mean to visually move to the right to the nearest multiple column.
set shiftwidth=2 " When vim wants to indent something (e.g. a FilePlugin says that we're introducing a new block scope), this is visually how many columns to the right it should try to make the next line start at. Use 'something' (might be tabs, might be spaces) to make that happen.
set noexpandtab  " When the user presses the tab key, insert a tab characer (i.e. don't expand the tab into spaces instead.

" Use ocaml's indenting hints.
" set rtp^="/home/nebu/.opam/default/share/ocp-indent/vim"

