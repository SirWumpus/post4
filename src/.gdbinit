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

define showfs
p ctx->fs.top + 1 - ctx->fs.base
p ctx->fs
x/8gx ctx->fs.base
end

define showrs
p ctx->rs.top + 1 - ctx->rs.base
p ctx->rs
x/8gx ctx->rs.base
end

define showword
p ctx->words
p *ctx->words
x/8gx ctx->words->data
end

define showwords
  set var $p = ctx->words
  while $p != 0
    printf "%#lx %s\n", $p, $p->name.string
    set var $p = $p->prev
  end
end

set can-use-hw-watchpoints 0

b p4Bp
commands
finish
end

b p4Repl:_repl
disable 2

show user
