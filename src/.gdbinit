set listsize 30
#handle SIGINT nostop pass noprint
#handle SIGSEGV nostop pass print

define showinput
p ctx->input.buffer+ctx->input.offset
end

define showds
p ctx->ds.top + 1 - ctx->ds.base
p ctx->ds
x/8gx ctx->ds.base
end

define showrs
p ctx->rs.top + 1 - ctx->rs.base
p ctx->rs
x/8gx ctx->rs.base
end

define showword
p *ctx->words
x/8gx ctx->words->data
end

b p4Bp
commands
finish
end

b p4Repl:_repl
disable 2

show user
