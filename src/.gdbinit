set listsize 20

define showinput
p ctx->input.buffer+ctx->input.offset
end

define showds
p ctx->ds
x/8x ctx->ds.base
end

define showrs
p ctx->rs
x/8x ctx->rs.base
end

define shownoname
p ctx->noname
x/12x ctx->noname.base
end

define showdata
p *ctx->xt->data
x/12x ctx->xt->data.base
end

search ^P4_WORD_DEFINE(EVALUATE)
search PARSE_WORD
b $_
comm 1
showinput
end

search ^P4_WORD_DEFINE(MAIN)
search EVALUATE
b $_
