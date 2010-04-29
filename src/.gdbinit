set listsize 24
handle SIGINT nostop pass noprint

define showinput
p ctx->input.buffer+ctx->input.offset
end

define showds
p ctx->ds
x/8xw ctx->ds.base
end

define showrs
p ctx->rs
x/8xw ctx->rs.base
end

define shownoname
p ctx->noname
x/12xw ctx->noname.base
end

define showdata
p *ctx->xt->data
x/12xw ctx->xt->data.base
end

search ^p4Interpret
search PARSE_WORD
b $_
comm 1
showinput
end

search ^P4_WORD_DEFINE(MAIN)
search p4Interpret
b $_

b p4_do_NOOP
