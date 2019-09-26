" -------------------------------------------------------
" tux config for external vim add-ons
" -------------------------------------------------------

" ------------------------------------
" --- NERDTree ---

" auto-run it
"if !&diff | if has("gui_running") | au GUIEnter * NERDTree | else | au VimEnter * NERDTree | endif | endif

" map it
map <C-a> :NERDTreeToggle<CR>

" re-enable netrw
let NERDTreeHijackNetrw=0 

" -------------------------------------
" --- netrw ---
let g:netrw_liststyle=3

" -------------------------------------
" --- .wiki syntax ---
autocmd BufRead,BufNewFile *.wikipedia.org* setfiletype Wikipedia
autocmd FileType Wikipedia setlocal encoding=utf-8

" -------------------------------------
" --- AutoFenc ---
let g:autofenc_autodetect_ext_prog=0

" -------------------------------------
" --- VCSCommand ---
let VCSCommandSVNExec='C:\Program Files\SVN\bin\svn.exe'
let VCSCommandGitExec='C:\Program Files\Git\bin\git.exe'

" -------------------------------------
" --- mru.vim ---
let MRU_Max_Entries=25
let MRU_Exclude_Files='\\temp\\'
let MRU_Current_Window=1

" -------------------------------------
" --- TwitVim ---
let twitvim_count=75
let twitvim_show_header=0
" ... for OAuth: ...
let twitvim_enable_perl=1
let twitvim_enable_python=1
let twitvim_browser_cmd='C:\Program Files\Mozilla Firefox\firefox.exe'
" ... mappings: ...
nnoremap <F8>   :FriendsTwitter<cr>:exe bufwinnr('^Twitter_').'wincmd w'<cr>:only!<cr>
nnoremap <S-F8> :UserTwitter<cr>:exe bufwinnr('^Twitter_').'wincmd w'<cr>:only!<cr>
nnoremap <A-F8> :RepliesTwitter<cr>:exe bufwinnr('^Twitter_').'wincmd w'<cr>:only!<cr>
nnoremap <C-F8> :DMTwitter<cr>:exe bufwinnr('^Twitter_').'wincmd w'<cr>:only!<cr>

" -------------------------------------
" --- vimcommander ---
noremap <silent> <F11> :call VimCommanderToggle()<CR><C-w><Down><C-w>c

" -------------------------------------
" --- ZenCoding.vim ---
let g:user_zen_expandabbr_key = '<C-d>'
let g:user_zen_settings = { 'indentation' : '  ' }

" -------------------------------------
" --- HTML Autoclosetag ---
"let b:mapped_auto_closetag = 1

" -------------------------------------
" --- yaifa ---
let yaifa_indentation = 0 " default indentation: spaces

" -------------------------------------
" --- delimitMate ---
let loaded_delimitMate = 0 " turn it off

" -------------------------------------
" --- neocomplcache ---

" ... disable AutoComplPopup ...
let g:acp_enableAtStartup = 0

let g:neocomplcache_enable_at_startup = 1
let g:neocomplcache_enable_smart_case = 1
let g:neocomplcache_enable_camel_case_completion = 1
let g:neocomplcache_enable_underbar_completion = 1
let g:neocomplcache_min_syntax_length = 3
let g:neocomplcache_lock_buffer_name_pattern = '\*ku\*'

" ... define keyword ...
if !exists('g:neocomplcache_keyword_patterns')
    let g:neocomplcache_keyword_patterns = {}
endif
let g:neocomplcache_keyword_patterns['default'] = '\h\w*'

imap <C-k>     <Plug>(neocomplcache_snippets_expand)
smap <C-k>     <Plug>(neocomplcache_snippets_expand)
inoremap <expr><C-g>     neocomplcache#undo_completion()
inoremap <expr><C-l>     neocomplcache#complete_common_string()

" <TAB>: completion.
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
" <C-h>, <BS>: close popup and delete backword char.
inoremap <expr><C-h> pumvisible() ? neocomplcache#close_popup()."\<C-h>" : "\<C-h>"
inoremap <expr><BS> pumvisible() ? neocomplcache#close_popup()."\<C-h>" : "\<C-h>"
inoremap <expr><C-y>  neocomplcache#close_popup()
inoremap <expr><C-e>  neocomplcache#cancel_popup()

" ... emulate AutoComplPopup ...
let g:neocomplcache_enable_auto_select = 1

" -------------------------------------
" --- OrgMode ---

let g:org_agenda_dirs=["c:/users/admin/Documents"]
let g:org_todo_setup= 'TODO STARTED | DONE'
let g:org_tag_setup='{@blog(b) @study(s) @work(w)} \n {easy(e) hard(d)} \n {computer(c) phone(p)}'

" leave these as is:
au! BufRead,BufWrite,BufWritePost,BufNewFile *.org 
au BufRead,BufNewFile *.org            call org#SetOrgFileType()
au BufRead *.org :PreLoadTags
au BufWrite *.org :PreWriteTags
au BufWritePost *.org :PostWriteTags

" These two hooks are currently the only ones enabled in 
" the VimOrganizer codebase, but they are easy to add so if
" there's a particular hook you want go ahead and request it
" or look for where these hooks are implemented in 
" /ftplugin/org.vim and use them as example for placing your
" own hooks in VimOrganizer:
function! Org_property_changed_functions(line,key, val)
        "call confirm("prop changed: ".a:line."--key:".a:key." val:".a:val)
endfunction
function! Org_after_todo_state_change_hook(line,state1, state2)
        "call ConfirmDrawer("LOGBOOK")
        "let str = ": - State: " . Pad(a:state2,10) . "   from: " . Pad(a:state1,10) .
        "            \ '    [' . Timestamp() . ']'
        "call append(line("."), repeat(' ',len(matchstr(getline(line(".")),'^\s*'))) . str)
        
endfunction