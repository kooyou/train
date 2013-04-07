execute pathogen#infect()

set nocompatible
source $VIMRUNTIME/vimrc_example.vim
source $VIMRUNTIME/mswin.vim
behave mswin

set diffexpr=MyDiff()
function MyDiff()
  let opt = '-a --binary '
  if &diffopt =~ 'icase' | let opt = opt . '-i ' | endif
  if &diffopt =~ 'iwhite' | let opt = opt . '-b ' | endif
  let arg1 = v:fname_in
  if arg1 =~ ' ' | let arg1 = '"' . arg1 . '"' | endif
  let arg2 = v:fname_new
  if arg2 =~ ' ' | let arg2 = '"' . arg2 . '"' | endif
  let arg3 = v:fname_out
  if arg3 =~ ' ' | let arg3 = '"' . arg3 . '"' | endif
  let eq = ''
  if $VIMRUNTIME =~ ' '
    if &sh =~ '\<cmd'
      let cmd = '""' . $VIMRUNTIME . '\diff"'
      let eq = '"'
    else
      let cmd = substitute($VIMRUNTIME, ' ', '" ', '') . '\diff"'
    endif
  else
    let cmd = $VIMRUNTIME . '\diff'
  endif
  silent execute '!' . cmd . ' ' . opt . arg1 . ' ' . arg2 . ' > ' . arg3 . eq
endfunction

set nu!
set ts=4  "ts��tabstop����д����TAB��4���ո�
set expandtab "��TAB�滻Ϊ�ո�
set sw=4
set noai
set ru
set nocin
set nobackup
set paste
set backspace=indent,eol,start

"���windows��������
"set encoding=utf-8      "�ڲ�����
"set langmenu=zh_CN.UTF-8
"language message zh_CN.UTF-8
"set termencoding=utf-8  " �ն˱��룬Windows����
" �Զ�ʶ������б� fileencodings
"set fencs=ucs-bom,gb18030,gbk,gb2312,big5,utf-8,euc-jp,euc-kr,latin1,cp936
"�ļ�����
"if has("win32")
"set fileencoding=chinese
"else
"set fileencoding=utf-8
"endif

set fenc=utf-8
set fencs=utf-8
set tenc=utf-8
set statusline=%F%m%r,%Y,%{&fileformat}\ \ \ ASCII=\%b,HEX=\%B\ \ \ %l,%c%V\ %p%%\ \ \ [\ %L\ lines\ in\ all\ ]
let &termencoding=&encoding
set ff=unix
set ffs=unix,dos
colorscheme koehler

"NERDTree plugin
let NERDTreeWinPos = "left" "where NERD tree window is placed on the screen
let NERDTreeWinSize = 25 "size of the NERD tree
nmap <F8> <ESC>:NERDTreeToggle<RETURN>	"Open and close the NERD_tree.vim separately
let NERDTreeMouseMode = 1 "ָ�����ģʽ��1.˫���򿪣�2.��Ŀ¼˫�ļ���3.�����򿪣�

"taglist plugin
let Tlist_Show_One_File = 1 "taglist���ֻ��ʾ��ǰ�ļ���tag
let Tlist_Use_Right_Window = 1 "��taglist������ʾ���ұߣ�Ĭ�������
let Tlist_Exit_OnlyWindow = 1 "�˳�vimʱ���˳�taglist
let Tlist_Use_SingleClick = 2 "˫��tag����
let Tlist_Auto_Open = 0	"���Զ���Tlist����
let Tlist_WinWidth = 25	"Ĭ�Ͽ��

nmap <F9> :Tlist <cr>	"��������Tlist�Ŀ�ݼ�Ϊ<F9>

"NERD_commenter plugin
let NERDShutUp=0

set lbr	"�������У�lbr��linebreak��

"���������Զ���ȫ
:inoremap ( ()<ESC>i
:inoremap ) <c-r>=ClosePair(')')<CR>
:inoremap { {}<ESC>i
:inoremap } <c-r>=ClosePair('}')<CR>
:inoremap [ []<ESC>i
:inoremap ] <c-r>=ClosePair(']')<CR>
:inoremap < <><ESC>i
:inoremap > <c-r>=ClosePair('>')<CR>
"�Լ�����ȥ��
:inoremap " ""<ESC>i


function ClosePair(char)
    if getline('.')[col('.') - 1] == a:char
        return "\<Right>"
    else
        return a:char
    endif
endfunction

"lcd d:\ �Զ��л�Ŀ¼

"�������
au GUIEnter * simalt ~x

"from .vimrc"
"----------------------------------------+
" vim7�����ļ�
"----------------------------------------+

set nocompatible "����Ҫ����VI


"�������
set fileformats=unix,dos,mac "�ļ���ʽѡ��˳��
set fileencodings=ucs-bom,utf-8,chinese
set termencoding=utf-8
set helplang=cn
set history=100
set backspace=indent,eol,start "�˸������
set expandtab "��TABתΪ�ո�
set tabstop=4
set softtabstop=4
set shiftwidth=4
set smarttab
set wrap
set hidden
set autoindent
set smartindent
set cindent
set magic
set ignorecase  "����ʱ�����ִ�Сд
set incsearch
set hlsearch
set number
set scrolloff=3
set grepprg=grep\ -nri\ --include=*.{erl,php,js,as,html,py,pyw}

"�ر��Զ�����
set nobackup
set nowritebackup
set noswapfile

"�ر�����
set noerrorbells
set novisualbell

"�����ļ��Զ�ʶ��
syntax on
filetype on
filetype plugin on
filetype indent on

"״̬������ʾ��ʽ
set laststatus=2
set statusline=%f\ \%h%m%r%r%=%-35(%l,%c\ [0x%B]\ (%L\ lines)\ [%{&ff}:%{&fenc}]\ %y%)\ %P
"set statusline=%<%f%m\ \[%{&ff}:%{&fenc}:%Y]\ %{getcwd()}\ \ \[%{strftime('%Y/%b/%d\ %a\ %I:%M\ %p')}\]\ %=\ Line:%l\/%L\ Column:%c%V\ %P
"set statusline=\ %f%m%r%h%w\ %=%({%{&ff}\|%{(&fenc==\"\"?&enc:&fenc).((exists(\"+bomb\")\ &&\ &bomb)?\",B\":\"\")}%k\|%Y}%)\ %([%l,%v][%p%%]\ %)

"ɫ������
colo vividchalk
"colo af
"colo moria
"colo neon
"colo dark2

"����leader��
let mapleader = ","
let g:mapleader = ","

"�ļ�������
cd E:/erlang_project/mail/trunk
" autocmd BufEnter * lcd %:p:h "�Զ�cd����ǰ�ļ����ڵ�Ŀ¼
"������Ŀ¼
map <f1> :NERDTreeToggle<cr>
"��pwd()Ϊ��ʼĿ¼�����ļ�
map <leader>ff :FufFile<cr>
nmap <c-x><c-f> :FufFile<cr>
"���ҵ�ǰĿ¼�µ��ļ�
map <leader>fc :FufFileWithCurrentBufferDir<cr>
"�����Ѵ򿪵��ļ�
map <leader>fb :FufBuffer<cr>
nmap <c-x><c-b> :FufBuffer<cr>
"����Ŀ¼[��pwd()Ϊ��ʼĿ¼]
map <leader>fd :FufDir<cr>
nmap <c-x><c-d> :FufDir<cr>
"����quickfix�е���Ϣ
map <leader>fq :FufQuickfix<cr>
"���ҵ�ǰĿ¼������Ŀ¼�µ������ļ�(ע��:����ļ��ܶ࣬��ʱ�Ῠ�þ�)
map <leader>fr :FufCoverageFile<cr>
"����tags��Ϣ(��Ҫ����ctags����tags�ļ�)
map <leader>ft :FufTag<cr>

"��ݼ�
map <m-space> :simalt ~<cr> "�������ڵ����˵�
map <leader>v :e $vim/.vimrc<cr> "�༭vimrc
map <leader>n :enew<cr> "��һ����Buffer
nmap <silent> <m-h> :bp<cr> "�л�����һ��buffer
nmap <silent> <m-l> :bn<cr> "�л�����һ��buffer
nmap <silent> <m-n> :noh<cr> "ȡ��������ʾ

"��ĳЩ�����ļ��Ŀ�ݼ�
nmap <leader>fl :e e:\dev\xiuxian\www\game\assets\config\server.xml<cr>
nmap <leader>ho :e c:/windows/system32/drivers/etc/hosts<cr>
nmap <leader>ap :e d:/Program Files/Apache Software Foundation/Apache2.2/conf/httpd.conf<cr>
nmap <leader>cyg :ConqueTerm d:/cygwin/Cygwin.bat<cr>

"�����ⲿӦ�ó���
nmap <leader>aba :silent !start "D:/Program Files/AbaReplace/AbaReplace.exe"<cr>
nmap <F11> :silent !start c:/progra~1/intern~1/iexplore.exe file://%:p<cr>
nmap <F12> :silent !start "D:/Program Files/Firefox3/firefox.exe" -p yeahoo %<cr>

"�𶨱�ǩ��صĿ�ݼ�
map <leader>tn :tabnew<cr>
map <leader>tc :tabclose<cr>
map <c-tab> :tabnext<cr>
map <c-s-tab> :tabNext<cr>
map <m-1> 1gt
map <m-2> 2gt
map <m-3> 3gt
map <m-4> 4gt
map <m-5> 5gt

"ʹ��undo��redoʱ�����л�ģʽ
map  <c-u> :undo<cr>
map  <c-/> :undo<cr>
map! <c-/> <c-o>:undo<cr>
map! <m-u> <c-o>:undo<cr>
map! <c-u> <c-o>:undo<cr>
map! <c-r> <c-o>:redo<cr>

"����eamcs�Ĳ�����ʽ(��vim�ܺõĲ���)
" map  <c-k> dd
" map! <c-k> <c-o>dd
map  <c-a> 0
map! <c-a> <c-o>0
map  <c-e> $
map! <c-e> <c-o>$
map  <c-d> x
map! <c-d> <c-o>x
map  <c-x><c-s> :w<cr>
map! <c-x><c-s> <c-o>:w<cr>
map  <c-g> <esc>
map! <c-g> <esc>

"�ڱ༭ģʽ��ʹ����������
map! <m-,> <c-o>V<
map! <m-.> <c-o>V>

"�ڱ༭ģʽ���ƶ����
map! <m-h> <c-o>h
map! <m-j> <c-o>j
map! <m-k> <c-o>k
map! <m-l> <c-o>l
map! <m-a> <c-o>0
map! <m-e> <c-o>$
map! <m-w> <c-o>w


" ����
map! <m-d> <c-o>dw

"BufCloseִ�к�ĸ��Ӳ���,��Ϊ�رյ�ǰbuffer��������һ��buffer
let g:BufClose_AltBuffer = 'bNext'
map <leader>d :BufClose<cr> "�ر��ļ���������

"��quickfix��ݼ�
nmap <leader>cc :cc<cr>     "��ת����ǰ
nmap <leader>cn :cn<cr>     "��ת����һ��
nmap <leader>cp :cp<cr>     "��ת����һ��
nmap <leader>co :copen<cr>  "��quickfix����
nmap <leader>cq :cclose<cr> "�ر�quickfix����
nmap <leader>cw :cw<cr>     "
nmap <leader>cr :cr<cr>
nmap <leader>cl :cl<cr>
" quickfix���ؼ�
map! <m-q> <c-o> :call ToggleQuickfix()<cr>
map <m-q> :call ToggleQuickfix()<cr>
function! ToggleQuickfix()
    if !exists("g:quickfix_is_show")
        let g:quickfix_is_show = 0
    endif
    if g:quickfix_is_show
        let g:quickfix_is_show = 0
        cclose
    else
        let g:quickfix_is_show = 1
        copen
    endif
endfunction

"�����﷨��鹤��
let g:syntastic_enable_signs=1
" let g:syntastic_auto_loc_list=1
" set statusline+=%#warningmsg#
" set statusline+=%{SyntasticStatuslineFlag()}
" set statusline+=%*

"Errors��ʾ�л�
map! <m-e> <c-o>:call ToggleErrors()<cr>
map <m-e> :call ToggleErrors()<cr>
function! ToggleErrors()
    if !exists("g:errors_is_show")
        let g:errors_is_show = 0
    endif
    if g:errors_is_show
        let g:errors_is_show = 0
        lclose
    else
        let g:errors_is_show = 1
        " Errors ��ҪSyntastic���֧��
        Errors
    endif
endfunction

"���ع�긨����
hi Search guibg=#ff6600 guifg=#ffffff
hi IncSearch guibg=#ff6600 guifg=#ffffff
hi cursorline guibg=#222222
hi cursorcolumn guibg=#222222
function! ToggleCursorLine()
    if !exists("g:CursorLineStatu")
        let g:CursorLineStatu = 0
    endif
    if g:CursorLineStatu == 1
        set nocursorcolumn
        set nocursorline
        let g:CursorLineStatu = 0
    else
        set cursorcolumn
        set cursorline
        let g:CursorLineStatu = 1
    endif
endfunction

" call ToggleCursorLine() "Ĭ�ϴ򿪹�긨����,��Ҫ�ر���ע������
map <m-m> :call ToggleCursorLine()<cr> "�𶨵�Alt+m����

if has("gui_running")
    "set guioptions=

    set guifont=ProFontWindows:h11:w5.8
    set guifont=inconsolata:h10
    set guifont=YaHei\ Consolas\ Hybrid:h12
    "set guifont=courier\ new:h12:w5.8

    " ��������λ��
    set columns=134
    set lines=34
    map <c-a-left> :call ChWinpos("left")<cr>
    map <c-a-right> :call ChWinpos("right")<cr>
    map <c-a-up> :call ChWinpos("up")<cr>
    map <c-a-down> :call ChWinpos("down")<cr>
    function! ChWinpos(dir)
        if "left" == a:dir
            exec ':winpos '.(getwinposx() - 44).' '.(getwinposy())
        endif
        if "right" == a:dir
            exec ':winpos '.(getwinposx() + 44).' '.(getwinposy())
        endif
        if "up" == a:dir
            exec ':winpos '.(getwinposx()).' '.(getwinposy() - 44)
        endif
        if "down" == a:dir
            exec ':winpos '.(getwinposx()).' '.(getwinposy() + 44)
        endif
    endfunction
endif



"Session���
set sessionoptions+=resize
map <F3> :mksession! ~/worksess.vim<cr> "���浱ǰ�༭����״̬
map <leader>load :source ~/worksess.vim<cr> "�ָ��༭�����ϴα����״̬

" flash�������
nmap <F9> :call MakeMxml()<cr> "��flex������
nmap <F10> :!d:/software/flashplayer.exe %<.swf<cr> "ִ�б�����swf
function! MakeMxml()
    setl makeprg=mxmlc\ --static-link-runtime-shared-libraries=true\ --show-actionscript-warnings=true\ --strict=true\ -debug=true\ %
    setl errorformat=\ %f(%l):\ Error:\ %m
    :make
endfunction

" ��app�ļ�����erlang�ļ�����
autocmd BufRead,BufNewFile *.app set filetype=erlang
" ActionScript,flex,air
autocmd BufRead,BufNewFile *.as set filetype=actionscript
"��heXa�ļ�Ҳ����actionscript�ļ���ʾ
autocmd BufRead,BufNewFile *.hx set filetype=actionscript
"��mxml���XML�ļ���ʾ
autocmd BufRead,BufNewFile *.mxml set filetype=mxml

"�����ܲ�ȫ
autocmd FileType python set omnifunc=pythoncomplete#Complete
autocmd FileType javascript set omnifunc=javascriptcomplete#CompleteJS
autocmd FileType html set omnifunc=htmlcomplete#CompleteTags
autocmd FileType css set omnifunc=csscomplete#CompleteCSS
autocmd FileType xml set omnifunc=xmlcomplete#CompleteTags
autocmd FileType php set omnifunc=phpcomplete#CompletePHP
autocmd FileType c set omnifunc=ccomplete#Complete
autocmd FileType erlang set omnifunc=erlang_complete#Complete

" neocomplcache����������
let g:neocomplcache_enable_at_startup = 1
" ��ĳЩ����½���neocomplcache
let g:neocomplcache_lock_buffer_name_pattern = '[fuf]'
inoremap <expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"

" inoremap <expr><cr> neocomplcache#smart_close_popup() . "\<cr>"
" inoremap <expr><c-h> neocomplcache#smart_close_popup()."\<c-h>" 
" inoremap <expr><bs> neocomplcache#smart_close_popup()."\<c-h>" 
" inoremap <expr><c-y> neocomplcache#close_popup() 
" inoremap <expr><c-e> neocomplcache#cancel_popup() 

let g:neocomplcache_dictionary_filetype_lists = {
            \ 'default' : '',
            \ 'erlang' : $vim.'/dict/erlang.dict',
            \ }
let g:neocomplcache_keyword_patterns = {
            \ 'default' : '\h\w*',
            \ 'erlang' : '\v\h\w*(:\h\w*)*',
            \ }
let g:neocomplcache_omni_patterns = {
            \ 'ruby' : '[^. *\t]\.\w*\|\h\w*::',
            \ 'php' : '[^. \t]->\h\w*\|\h\w*::',
            \ } 

"""""""""""""""""""""""""""""""""
" ��д
"""""""""""""""""""""""""""""""""
"Html������д
iab <xhtml>  <esc>:set filetype=xhtml<cr>:set fileencoding=utf-8<cr>i<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"><cr><html xmlns="http://www.w3.org/1999/xhtml"><cr><head><cr><meta http-equiv="Content-Type" content="text/html; charset=utf-8" /><cr><meta name="keywords" content="" /><cr><title></title><cr></head><cr><body><cr></body><cr></html><esc>4k
iab <refresh> <meta http-equiv="refresh" content="0;url="><esc>hh
iab <css> <style type="text/css"><cr></style><esc>O
iab <cssm> <style type="text/css"><cr>*{margin:0; padding:0;}<cr>ul{list-style:none;}<cr></style><esc>k
iab <ci> <link rel="stylesheet" type="text/css" href="" /><esc>2b1l
iab <js> <script type="text/javascript"><cr></script><esc>O
iab <ji> <script type="text/javascript" src=""></script><esc>2b1l
iab <swf> <object type="application/x-shockwave-flash" data="0000" width="0000" height="0000"><param name="movie" value="0000" /></object><esc>/0000<cr>h
iab <form> <form method="post"><cr></form><esc>1k1h
iab <input> <input type="text" name="0000" value="0000" /><esc>/0000<cr>h
iab <radio> <input type="radio" name="0000" value="0000" />0000<esc>/0000<cr>h
iab <select> <select name="0000"><cr><option value="0000">0000</option><cr><option value="0000">0000</option><cr></select><esc>/0000<cr>h
iab <cb> <input type="checkbox" name="0000" value="0000" />0000<esc>/0000<cr>h
iab <submit> <input type="submit" name="submit" value="submit" /><esc>h
iab <btn> <input type="button" name="0000" value="0000" /><esc>/0000<cr>h
iab <img> <img src="0000" alt="0000" /><esc>/0000<cr>h
iab <u> <ul><cr><li>0000</li><cr><li>0000</li><cr></ul><esc>/0000<cr>h
iab <a> <a href="0000">0000</a><esc>/0000<cr>h
iab id> id=""<esc>hh
iab <tb> <table><cr><tr><td>0000</td></tr><cr></table><esc>/0000<cr>h
iab <tr> <tr><td>0000</td></tr><esc>/0000<cr>h
iab <td> <td>0000</td><esc>/0000<cr>h
iab f> function (){<cr>}<esc>1k$3h

"erlang������д
iab <erl> <esc>:set filetype=erlang<cr>:set fileencoding=utf-8<cr>i%%----------------------------------------------------<cr>%% <cr>%% @author yeahoo2000@gmail.com<cr>%% @end<cr>%%----------------------------------------------------<cr>-module().<cr>-export([]).<esc>4kl
iab <gs> <esc>:set filetype=erlang<cr>:set fileencoding=utf-8<cr>i%%----------------------------------------------------<cr>%%<cr>%% <cr>%% @author yeahoo2000@gmail.com<cr>%% @end<cr>%%----------------------------------------------------<cr>-module().<cr>-behaviour(gen_server).<cr>-export([start_link/0]).<cr>-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).<cr>-record(state, {}).<cr><cr>start_link() -><cr>gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).<cr><cr>init([]) -><cr>State = #state{},<cr>{ok, State}.<cr><cr>handle_call(_Request, _From, State) -><cr>{noreply, State}.<cr><cr>handle_cast(_Msg, State) -><cr>{noreply, State}.<cr><cr>handle_info(_Info, State) -><cr>{noreply, State}.<cr><cr>terminate(_Reason, _State) -><cr>ok.<cr><cr>code_change(_OldVsn, State, _Extra) -><cr>{ok, State}.<esc>2GA
iab <gf> <esc>:set filetype=erlang<cr>:set fileencoding=utf-8<cr>i%%----------------------------------------------------<cr>%%<cr>%%<cr>%% @author yeahoo2000@gmail.com<cr>%% @end<cr>%%----------------------------------------------------<cr>-module().<cr>-behaviour(gen_fsm).<cr>-export(<cr>[<cr>start_link/1<cr>]<cr>).<cr>-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).<cr>-record(state, {}).<cr><cr>start_link([])-><cr>gen_fsm:start_link(?MODULE, [], []).<cr><cr>init([])-><cr>{ok, xxxx, State}.<cr><cr>handle_event(_Event, StateName, State) -><cr>{next_state, StateName, State}.<cr><cr>handle_sync_event(_Event, _From, StateName, State) -><cr>Reply = ok,<cr>{reply, Reply, StateName, State}.<cr><cr>handle_info(_Info, StateName, State) -><cr>{next_state, StateName, State}.<cr><cr>terminate(_Reason, _StateName, _State) -><cr>ok.<cr><cr>code_change(_OldVsn, StateName, State, _Extra) -><cr>{ok, StateName, State}.<esc>2GA

"PHP������д
iab <php> <esc>:set filetype=php<cr>:set fileencoding=utf-8<cr>i<?php<cr>/**----------------------------------------------------+<cr> * <cr>* @author yeahoo2000@gmail.com<cr>+-----------------------------------------------------*/<esc>2kl
iab ifel> if(0000){<cr>0000<cr>}else{<cr>0000<cr>}<cr>0000<esc>/0000<cr>h
iab func> function 0000(0000){<cr>0000<cr>}<cr>0000<esc>/0000<cr>h

"Flex��һЩ��д����
iab <mx> xmlns:mx="http://www.adobe.com/2006/mxml"
iab <xml> <?xml version="1.0" encoding="utf-8"?><esc>set fileencoding=utf8<cr>
iab <flex>  <esc>:set filetype=xml<cr>:set fileencoding=utf-8<cr>i<?xml version="1.0" encoding="utf-8"?><cr><mx:Application xmlns="http://www.adobe.com/2006/mxml"><cr></mx:Application><esc>O
iab <air>  <esc>:set filetype=xml<cr>:set fileencoding=utf-8<cr>i<?xml version="1.0" encoding="utf-8"?><cr><mx:WindowedApplication title="0000" width="0000" height="0000" creationComplete="0000" xmlns="http://www.adobe.com/2006/mxml"><cr></mx:WindowedApplication><esc>/0000<cr>
iab <aircfg> <esc>:set filetype=xml<cr>:set fileencoding=utf-8<cr>i<?xml version="1.0" encoding="UTF-8"?><cr><application xmlns="http://ns.adobe.com/air/application/1.0"><cr><id>0000</id><cr><version>0000</version><cr><filename>0000</filename><cr><initialWindow><cr><content>0000.swf</content><cr><visible>true</visible><cr><systemChrome>none</systemChrome><cr><transparent>true</transparent><cr><width>0000</width><cr><height>0000</height><cr></initialWindow><cr></application><esc>/0000<cr>

"taglist������������
let Tlist_Use_Right_Window = 1
nmap <leader>tl :Tlist<cr>

"�Զ��л�tags·��
autocmd BufEnter d:/mhfx/server/* call ChProject('d:/mhfx/server')
autocmd BufEnter d:/mhfx/web/* call ChProject('d:/mhfx/web')
autocmd BufEnter d:/mhfx/client/* call ChProject('d:/mhfx/client')
function! ChProject(path)
    if a:path == ""
        return
    else
        let g:PjRoot=a:path
        "�л���Ŀ¼
        execute 'cd '.g:PjRoot
        "����tags�ļ�
        execute 'set tags='.a:path.'/tags'
    endif
endfunction
"����tags�ļ�
map <f9> :call TagsMake()<cr>
function! TagsMake()
    if exists("g:PjRoot")
        execute '!ctags -R -f '.g:PjRoot.'/tags '.g:PjRoot.'/*'
    endif
endfunction




""""fengzhenlin ���
map <F5> :call CompileRun()<CR>
map <F6> :call Run()<CR>
map <F7> :call ClientRun()<CR>
"����CompileRun�������������ý��б�������� 
func CompileRun() 
exec "w" 
"erlang���� 
if &filetype == 'erlang' 
exec "!erl -make" 
endif 
endfunc 
"��������CompileRun

func Run()
exec "!erl -s chat_supervisor start"
endfunc
"��������Run

func ClientRun()
exec "!erl"
endfunc
"��������ClientRun


"�ļ�ģ��"
autocmd BufNewFile *.erl 0 r D:/vim/vim73/mytemplate/erl.tpl




"from _vimrc"






"lcd d:\ �Զ��л�Ŀ¼

"�������
au GUIEnter * simalt ~x


"--------------------------->>>һЩtips<<<------------------------------
":global /^/ move 0 |
"                   +���ݵ���
":g/^/m0            |

"from _vimrc"






"lcd d:\ �Զ��л�Ŀ¼

"�������
au GUIEnter * simalt ~x


"--------------------------->>>һЩtips<<<------------------------------
":global /^/ move 0 |
"                   +���ݵ���
":g/^/m0            |

