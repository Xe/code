" Copyright 2011 The Go Authors. All rights reserved.
" Use of this source code is governed by a BSD-style
" license that can be found in the LICENSE file.
"
" fmt.vim: Vim command to format Markdown files with markdownfmt.
"
" This filetype plugin add a new commands for markdown buffers:
"
"   :Fmt
"
"       Filter the current Markdown buffer through markdownfmt.
"       It tries to preserve cursor position and avoids
"       replacing the buffer with stderr output.
"
" Options:
"
"   g:markdown_fmt_commands [default=1]
"
"       Flag to indicate whether to enable the commands listed above.
"
"   g:markdown_fmt_command [default="markdownfmt"]
"
"       Flag naming the markdownfmt executable to use.
"
"   g:markdown_fmt_autosave [default=1]
"
"       Flag to auto call :Fmt when saved file
"
if exists("b:did_ftplugin_markdown_fmt")
    finish
endif

if !exists("g:markdown_fmt_commands")
    let g:markdown_fmt_commands = 1
endif

if !exists("g:markdown_fmt_command")
    let g:markdown_fmt_command = "markdownfmt"
endif

if !exists('g:markdown_fmt_autosave')
    let g:markdown_fmt_autosave = 1
endif

if !exists('g:markdown_fmt_fail_silently')
    let g:markdown_fmt_fail_silently = 0
endif

if !exists('g:markdown_fmt_options')
    let g:markdown_fmt_options = ''
endif

if g:markdown_fmt_autosave
    autocmd BufWritePre <buffer> :MarkdownFmt
endif

if g:markdown_fmt_commands
    command! -buffer MarkdownFmt call s:MarkdownFormat(-1)
    command! -buffer MarkdownImports call s:MarkdownFormat(1)
endif

let s:markdownt_fmt_error = 0

"  we have those problems : 
"  http://stackoverflow.com/questions/12741977/prevent-vim-from-updating-its-undo-tree
"  http://stackoverflow.com/questions/18532692/markdownlang-formatter-and-vim-how-to-destroy-history-record?rq=1
"
"  The below function is an improved version that aims to fix all problems.
"  it doesn't undo changes and break undo history.  If you are here reading
"  this and have VimL experience, please look at the function for
"  improvements, patches are welcome :)
function! s:MarkdownFormat(withMarkdownimport)
    " save cursor position and many other things
    let l:curw=winsaveview()

    " needed for testing if markdownfmt fails or not
    let l:tmpname=tempname()
    call writefile(getline(1,'$'), l:tmpname)

    " save our undo file to be restored after we are done. This is needed to
    " prevent an additional undo jump due to BufWritePre auto command and also
    " restore 'redo' history because it's getting being destroyed every
    " BufWritePre
    let tmpundofile=tempname()
    exe 'wundo! ' . tmpundofile

    " get the command first so we can test it
    let fmt_command = g:markdown_fmt_commands

    " populate the final command with user based fmt options
    let command = fmt_command . ' ' . g:markdown_fmt_options

    " execute our command...
    let out = system(command . " " . l:tmpname)

    "if there is no error on the temp file, markdownfmt again our original file
    if v:shell_error == 0
        " remove undo point caused via BufWritePre
        try | silent undojoin | catch | endtry

        " do not include stderr to the buffer, this is due to markdownimports/markdownfmt
        " tha fails with a zero exit return value (sad yeah).
        let default_srr = &srr
        set srr=>%s 

        " execufe markdownfmt on the current buffer and replace it
        silent execute "%!" . command

        " only clear quickfix if it was previously set, this prevents closing
        " other quickfixes
        if s:markdownt_fmt_error 
            let s:markdownt_fmt_error = 0
            call setqflist([])
            cwindow
        endif

        " put back the users srr setting
        let &srr = default_srr
    endif

    " restore our undo history
    silent! exe 'rundo ' . tmpundofile
    call delete(tmpundofile)

    " restore our cursor/windows positions
    call delete(l:tmpname)
    call winrestview(l:curw)
endfunction

let b:did_ftplugin_markdown_fmt = 1

" vim:ts=4:sw=4:et
