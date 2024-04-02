hi! link rubyCapitalizedMethod Type
hi! link rubyInclude Operator
hi! link rubyInterpolation Operator
hi! link rubyInterpolationDelimiter Identifier
hi! link rubyPseudoVariable Special
hi! link rubyStringDelimiter String
hi! link rubySymbol Special

unlet b:current_syntax
syn include @SQL syntax/sql.vim
syn region sqlHeredoc start=/\v\<\<[-~]SQL/ end=/\vSQL/ keepend contains=@SQL
let b:current_syntax = "ruby"
