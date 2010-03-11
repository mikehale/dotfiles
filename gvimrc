set background=dark
colorscheme macvim

highlight Normal guibg=#080808     " darker background
highlight CursorLine guibg=#0f0f0f " subtler highlighting

set columns=152
set cursorline        " highlight current line
set fuoptions=maxvert,maxhorz
set guifont=Monaco:h12
set guioptions=egmt   " disable scrollbars
set number            " enable line numbering
set transp=1          " slightly transparent

function! <SID>ShowNERDTreeOnlyIfWiderThan(threshold)
  if &columns > a:threshold
    NERDTree
  else
    NERDTreeClose
  endif
endfunction

autocmd VimResized * call <SID>ShowNERDTreeOnlyIfWiderThan(160)
