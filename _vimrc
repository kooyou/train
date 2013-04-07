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
set ts=4  "ts是tabstop的缩写，设TAB宽4个空格
set expandtab "将TAB替换为空格
set sw=4
set noai
set ru
set nocin
set nobackup
set paste
set backspace=indent,eol,start

"解决windows乱码问题
"set encoding=utf-8      "内部编码
"set langmenu=zh_CN.UTF-8
"language message zh_CN.UTF-8
"set termencoding=utf-8  " 终端编码，Windows忽略
" 自动识别编码列表 fileencodings
"set fencs=ucs-bom,gb18030,gbk,gb2312,big5,utf-8,euc-jp,euc-kr,latin1,cp936
"文件编码
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
let NERDTreeMouseMode = 1 "指定鼠标模式（1.双击打开；2.单目录双文件；3.单击打开）

"taglist plugin
let Tlist_Show_One_File = 1 "taglist插件只显示当前文件的tag
let Tlist_Use_Right_Window = 1 "让taglist窗口显示在右边，默认在左边
let Tlist_Exit_OnlyWindow = 1 "退出vim时候退出taglist
let Tlist_Use_SingleClick = 2 "双击tag跳动
let Tlist_Auto_Open = 0	"不自动打开Tlist窗口
let Tlist_WinWidth = 25	"默认宽度

nmap <F9> :Tlist <cr>	"设置命令Tlist的快捷键为<F9>

"NERD_commenter plugin
let NERDShutUp=0

set lbr	"整词折行（lbr即linebreak）

"设置括号自动补全
:inoremap ( ()<ESC>i
:inoremap ) <c-r>=ClosePair(')')<CR>
:inoremap { {}<ESC>i
:inoremap } <c-r>=ClosePair('}')<CR>
:inoremap [ []<ESC>i
:inoremap ] <c-r>=ClosePair(']')<CR>
:inoremap < <><ESC>i
:inoremap > <c-r>=ClosePair('>')<CR>
"自己加上去的
:inoremap " ""<ESC>i


function ClosePair(char)
    if getline('.')[col('.') - 1] == a:char
        return "\<Right>"
    else
        return a:char
    endif
endfunction

"lcd d:\ 自动切换目录

"启动最大化
au GUIEnter * simalt ~x

"from .vimrc"
"----------------------------------------+
" vim7配置文件
"----------------------------------------+

set nocompatible "不需要兼容VI


"编码相关
set fileformats=unix,dos,mac "文件格式选择顺序
set fileencodings=ucs-bom,utf-8,chinese
set termencoding=utf-8
set helplang=cn
set history=100
set backspace=indent,eol,start "退格键设置
set expandtab "将TAB转为空格
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
set ignorecase  "搜索时不区分大小写
set incsearch
set hlsearch
set number
set scrolloff=3
set grepprg=grep\ -nri\ --include=*.{erl,php,js,as,html,py,pyw}

"关闭自动备份
set nobackup
set nowritebackup
set noswapfile

"关闭声音
set noerrorbells
set novisualbell

"开启文件自动识别
syntax on
filetype on
filetype plugin on
filetype indent on

"状态栏的显示形式
set laststatus=2
set statusline=%f\ \%h%m%r%r%=%-35(%l,%c\ [0x%B]\ (%L\ lines)\ [%{&ff}:%{&fenc}]\ %y%)\ %P
"set statusline=%<%f%m\ \[%{&ff}:%{&fenc}:%Y]\ %{getcwd()}\ \ \[%{strftime('%Y/%b/%d\ %a\ %I:%M\ %p')}\]\ %=\ Line:%l\/%L\ Column:%c%V\ %P
"set statusline=\ %f%m%r%h%w\ %=%({%{&ff}\|%{(&fenc==\"\"?&enc:&fenc).((exists(\"+bomb\")\ &&\ &bomb)?\",B\":\"\")}%k\|%Y}%)\ %([%l,%v][%p%%]\ %)

"色彩主题
colo vividchalk
"colo af
"colo moria
"colo neon
"colo dark2

"设置leader键
let mapleader = ","
let g:mapleader = ","

"文件浏览相关
cd E:/erlang_project/mail/trunk
" autocmd BufEnter * lcd %:p:h "自动cd到当前文件所在的目录
"打开树型目录
map <f1> :NERDTreeToggle<cr>
"以pwd()为起始目录查找文件
map <leader>ff :FufFile<cr>
nmap <c-x><c-f> :FufFile<cr>
"查找当前目录下的文件
map <leader>fc :FufFileWithCurrentBufferDir<cr>
"查找已打开的文件
map <leader>fb :FufBuffer<cr>
nmap <c-x><c-b> :FufBuffer<cr>
"查找目录[以pwd()为起始目录]
map <leader>fd :FufDir<cr>
nmap <c-x><c-d> :FufDir<cr>
"查找quickfix中的信息
map <leader>fq :FufQuickfix<cr>
"查找当前目录包括子目录下的所有文件(注意:如果文件很多，打开时会卡好久)
map <leader>fr :FufCoverageFile<cr>
"查找tags信息(需要先由ctags生成tags文件)
map <leader>ft :FufTag<cr>

"快捷键
map <m-space> :simalt ~<cr> "弹出窗口调整菜单
map <leader>v :e $vim/.vimrc<cr> "编辑vimrc
map <leader>n :enew<cr> "打开一个空Buffer
nmap <silent> <m-h> :bp<cr> "切换到上一个buffer
nmap <silent> <m-l> :bn<cr> "切换到下一个buffer
nmap <silent> <m-n> :noh<cr> "取消高亮显示

"打开某些常用文件的快捷键
nmap <leader>fl :e e:\dev\xiuxian\www\game\assets\config\server.xml<cr>
nmap <leader>ho :e c:/windows/system32/drivers/etc/hosts<cr>
nmap <leader>ap :e d:/Program Files/Apache Software Foundation/Apache2.2/conf/httpd.conf<cr>
nmap <leader>cyg :ConqueTerm d:/cygwin/Cygwin.bat<cr>

"调用外部应用程序
nmap <leader>aba :silent !start "D:/Program Files/AbaReplace/AbaReplace.exe"<cr>
nmap <F11> :silent !start c:/progra~1/intern~1/iexplore.exe file://%:p<cr>
nmap <F12> :silent !start "D:/Program Files/Firefox3/firefox.exe" -p yeahoo %<cr>

"梆定标签相关的快捷键
map <leader>tn :tabnew<cr>
map <leader>tc :tabclose<cr>
map <c-tab> :tabnext<cr>
map <c-s-tab> :tabNext<cr>
map <m-1> 1gt
map <m-2> 2gt
map <m-3> 3gt
map <m-4> 4gt
map <m-5> 5gt

"使用undo和redo时无需切换模式
map  <c-u> :undo<cr>
map  <c-/> :undo<cr>
map! <c-/> <c-o>:undo<cr>
map! <m-u> <c-o>:undo<cr>
map! <c-u> <c-o>:undo<cr>
map! <c-r> <c-o>:redo<cr>

"兼容eamcs的操作方式(是vim很好的补充)
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

"在编辑模式下使用缩进功能
map! <m-,> <c-o>V<
map! <m-.> <c-o>V>

"在编辑模式下移动光标
map! <m-h> <c-o>h
map! <m-j> <c-o>j
map! <m-k> <c-o>k
map! <m-l> <c-o>l
map! <m-a> <c-o>0
map! <m-e> <c-o>$
map! <m-w> <c-o>w


" 其它
map! <m-d> <c-o>dw

"BufClose执行后的附加操作,设为关闭当前buffer后跳到下一个buffer
let g:BufClose_AltBuffer = 'bNext'
map <leader>d :BufClose<cr> "关闭文件后保留窗口

"梆定quickfix快捷键
nmap <leader>cc :cc<cr>     "跳转到当前
nmap <leader>cn :cn<cr>     "跳转到下一个
nmap <leader>cp :cp<cr>     "跳转到上一个
nmap <leader>co :copen<cr>  "打开quickfix窗口
nmap <leader>cq :cclose<cr> "关闭quickfix窗口
nmap <leader>cw :cw<cr>     "
nmap <leader>cr :cr<cr>
nmap <leader>cl :cl<cr>
" quickfix开关键
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

"设置语法检查工具
let g:syntastic_enable_signs=1
" let g:syntastic_auto_loc_list=1
" set statusline+=%#warningmsg#
" set statusline+=%{SyntasticStatuslineFlag()}
" set statusline+=%*

"Errors显示切换
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
        " Errors 需要Syntastic插件支持
        Errors
    endif
endfunction

"开关光标辅助线
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

" call ToggleCursorLine() "默认打开光标辅助线,想要关闭请注释这行
map <m-m> :call ToggleCursorLine()<cr> "梆定到Alt+m键上

if has("gui_running")
    "set guioptions=

    set guifont=ProFontWindows:h11:w5.8
    set guifont=inconsolata:h10
    set guifont=YaHei\ Consolas\ Hybrid:h12
    "set guifont=courier\ new:h12:w5.8

    " 调整窗口位置
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



"Session相关
set sessionoptions+=resize
map <F3> :mksession! ~/worksess.vim<cr> "保存当前编辑器的状态
map <leader>load :source ~/worksess.vim<cr> "恢复编辑器到上次保存的状态

" flash相关设置
nmap <F9> :call MakeMxml()<cr> "梆定flex编译器
nmap <F10> :!d:/software/flashplayer.exe %<.swf<cr> "执行编译后的swf
function! MakeMxml()
    setl makeprg=mxmlc\ --static-link-runtime-shared-libraries=true\ --show-actionscript-warnings=true\ --strict=true\ -debug=true\ %
    setl errorformat=\ %f(%l):\ Error:\ %m
    :make
endfunction

" 将app文件当成erlang文件处理
autocmd BufRead,BufNewFile *.app set filetype=erlang
" ActionScript,flex,air
autocmd BufRead,BufNewFile *.as set filetype=actionscript
"将heXa文件也当成actionscript文件显示
autocmd BufRead,BufNewFile *.hx set filetype=actionscript
"将mxml设成XML文件显示
autocmd BufRead,BufNewFile *.mxml set filetype=mxml

"打开智能补全
autocmd FileType python set omnifunc=pythoncomplete#Complete
autocmd FileType javascript set omnifunc=javascriptcomplete#CompleteJS
autocmd FileType html set omnifunc=htmlcomplete#CompleteTags
autocmd FileType css set omnifunc=csscomplete#CompleteCSS
autocmd FileType xml set omnifunc=xmlcomplete#CompleteTags
autocmd FileType php set omnifunc=phpcomplete#CompletePHP
autocmd FileType c set omnifunc=ccomplete#Complete
autocmd FileType erlang set omnifunc=erlang_complete#Complete

" neocomplcache插件相关设置
let g:neocomplcache_enable_at_startup = 1
" 在某些情况下禁用neocomplcache
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
" 缩写
"""""""""""""""""""""""""""""""""
"Html代码缩写
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

"erlang代码缩写
iab <erl> <esc>:set filetype=erlang<cr>:set fileencoding=utf-8<cr>i%%----------------------------------------------------<cr>%% <cr>%% @author yeahoo2000@gmail.com<cr>%% @end<cr>%%----------------------------------------------------<cr>-module().<cr>-export([]).<esc>4kl
iab <gs> <esc>:set filetype=erlang<cr>:set fileencoding=utf-8<cr>i%%----------------------------------------------------<cr>%%<cr>%% <cr>%% @author yeahoo2000@gmail.com<cr>%% @end<cr>%%----------------------------------------------------<cr>-module().<cr>-behaviour(gen_server).<cr>-export([start_link/0]).<cr>-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).<cr>-record(state, {}).<cr><cr>start_link() -><cr>gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).<cr><cr>init([]) -><cr>State = #state{},<cr>{ok, State}.<cr><cr>handle_call(_Request, _From, State) -><cr>{noreply, State}.<cr><cr>handle_cast(_Msg, State) -><cr>{noreply, State}.<cr><cr>handle_info(_Info, State) -><cr>{noreply, State}.<cr><cr>terminate(_Reason, _State) -><cr>ok.<cr><cr>code_change(_OldVsn, State, _Extra) -><cr>{ok, State}.<esc>2GA
iab <gf> <esc>:set filetype=erlang<cr>:set fileencoding=utf-8<cr>i%%----------------------------------------------------<cr>%%<cr>%%<cr>%% @author yeahoo2000@gmail.com<cr>%% @end<cr>%%----------------------------------------------------<cr>-module().<cr>-behaviour(gen_fsm).<cr>-export(<cr>[<cr>start_link/1<cr>]<cr>).<cr>-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).<cr>-record(state, {}).<cr><cr>start_link([])-><cr>gen_fsm:start_link(?MODULE, [], []).<cr><cr>init([])-><cr>{ok, xxxx, State}.<cr><cr>handle_event(_Event, StateName, State) -><cr>{next_state, StateName, State}.<cr><cr>handle_sync_event(_Event, _From, StateName, State) -><cr>Reply = ok,<cr>{reply, Reply, StateName, State}.<cr><cr>handle_info(_Info, StateName, State) -><cr>{next_state, StateName, State}.<cr><cr>terminate(_Reason, _StateName, _State) -><cr>ok.<cr><cr>code_change(_OldVsn, StateName, State, _Extra) -><cr>{ok, StateName, State}.<esc>2GA

"PHP代码缩写
iab <php> <esc>:set filetype=php<cr>:set fileencoding=utf-8<cr>i<?php<cr>/**----------------------------------------------------+<cr> * <cr>* @author yeahoo2000@gmail.com<cr>+-----------------------------------------------------*/<esc>2kl
iab ifel> if(0000){<cr>0000<cr>}else{<cr>0000<cr>}<cr>0000<esc>/0000<cr>h
iab func> function 0000(0000){<cr>0000<cr>}<cr>0000<esc>/0000<cr>h

"Flex的一些缩写定义
iab <mx> xmlns:mx="http://www.adobe.com/2006/mxml"
iab <xml> <?xml version="1.0" encoding="utf-8"?><esc>set fileencoding=utf8<cr>
iab <flex>  <esc>:set filetype=xml<cr>:set fileencoding=utf-8<cr>i<?xml version="1.0" encoding="utf-8"?><cr><mx:Application xmlns="http://www.adobe.com/2006/mxml"><cr></mx:Application><esc>O
iab <air>  <esc>:set filetype=xml<cr>:set fileencoding=utf-8<cr>i<?xml version="1.0" encoding="utf-8"?><cr><mx:WindowedApplication title="0000" width="0000" height="0000" creationComplete="0000" xmlns="http://www.adobe.com/2006/mxml"><cr></mx:WindowedApplication><esc>/0000<cr>
iab <aircfg> <esc>:set filetype=xml<cr>:set fileencoding=utf-8<cr>i<?xml version="1.0" encoding="UTF-8"?><cr><application xmlns="http://ns.adobe.com/air/application/1.0"><cr><id>0000</id><cr><version>0000</version><cr><filename>0000</filename><cr><initialWindow><cr><content>0000.swf</content><cr><visible>true</visible><cr><systemChrome>none</systemChrome><cr><transparent>true</transparent><cr><width>0000</width><cr><height>0000</height><cr></initialWindow><cr></application><esc>/0000<cr>

"taglist插件的相关设置
let Tlist_Use_Right_Window = 1
nmap <leader>tl :Tlist<cr>

"自动切换tags路径
autocmd BufEnter d:/mhfx/server/* call ChProject('d:/mhfx/server')
autocmd BufEnter d:/mhfx/web/* call ChProject('d:/mhfx/web')
autocmd BufEnter d:/mhfx/client/* call ChProject('d:/mhfx/client')
function! ChProject(path)
    if a:path == ""
        return
    else
        let g:PjRoot=a:path
        "切换根目录
        execute 'cd '.g:PjRoot
        "设置tags文件
        execute 'set tags='.a:path.'/tags'
    endif
endfunction
"生成tags文件
map <f9> :call TagsMake()<cr>
function! TagsMake()
    if exists("g:PjRoot")
        execute '!ctags -R -f '.g:PjRoot.'/tags '.g:PjRoot.'/*'
    endif
endfunction




""""fengzhenlin 添加
map <F5> :call CompileRun()<CR>
map <F6> :call Run()<CR>
map <F7> :call ClientRun()<CR>
"定义CompileRun函数，用来调用进行编译和运行 
func CompileRun() 
exec "w" 
"erlang程序 
if &filetype == 'erlang' 
exec "!erl -make" 
endif 
endfunc 
"结束定义CompileRun

func Run()
exec "!erl -s chat_supervisor start"
endfunc
"结束定义Run

func ClientRun()
exec "!erl"
endfunc
"结束定义ClientRun


"文件模板"
autocmd BufNewFile *.erl 0 r D:/vim/vim73/mytemplate/erl.tpl




"from _vimrc"






"lcd d:\ 自动切换目录

"启动最大化
au GUIEnter * simalt ~x


"--------------------------->>>一些tips<<<------------------------------
":global /^/ move 0 |
"                   +内容倒排
":g/^/m0            |

"from _vimrc"






"lcd d:\ 自动切换目录

"启动最大化
au GUIEnter * simalt ~x


"--------------------------->>>一些tips<<<------------------------------
":global /^/ move 0 |
"                   +内容倒排
":g/^/m0            |

