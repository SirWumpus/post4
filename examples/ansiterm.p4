\ ANSI terminal highlighting.
\
\ See post4.p4 for AT-XY (cursor postion) and PAGE (clear screen).

: ansi_normal S\" \e[0m" TYPE ;
: ansi_bold S\" \e[1m" TYPE ;
: ansi_dim S\" \e[2m" TYPE ;
: ansi_standout S\" \e[3m" TYPE ;
: ansi_underline S\" \e[4m" TYPE ;
: ansi_blink S\" \e[5m" TYPE ;
: ansi_reverse S\" \e[6m" TYPE ;
: ansi_hide S\" \e[7m" TYPE ;

: ansi_black S\" \e[30m" TYPE ;
: ansi_red S\" \e[31m" TYPE ;
: ansi_green S\" \e[32m" TYPE ;
: ansi_yellow S\" \e[33m" TYPE ;
: ansi_blue S\" \e[34m" TYPE ;
: ansi_magenta S\" \e[35m" TYPE ;
: ansi_cyan S\" \e[36m" TYPE ;
: ansi_white S\" \e[37m" TYPE ;

: ansi_bg_black S\" \e[40m" TYPE ;
: ansi_bg_red S\" \e[41m" TYPE ;
: ansi_bg_green S\" \e[42m" TYPE ;
: ansi_bg_yellow S\" \e[43m" TYPE ;
: ansi_bg_blue S\" \e[44m" TYPE ;
: ansi_bg_magenta S\" \e[45m" TYPE ;
: ansi_bg_cyan S\" \e[46m" TYPE ;
: ansi_bg_white S\" \e[47m" TYPE ;
