#!/module/for/bash

# This needs to be "sourced", rather than run as a stand-alone script, as it
# needs to modify the completion bindings of the user's interactive shell.
#
################################################################################

#
# NAMESPACE: parameters and functions with __zc_ prefix need to remain set
# between invocations; ones with _zc_ prefix are used but do not need to
# persist; they are normally localized but there might be leaks.  All other
# items are localized.
#
# Functions starting with __zcwrap_ are the wrapper completion functions, and
# must remain set as long as the corresponding completion binding is set.
#

################################################################################
#
# Check for supported shells
# BASH_VERSION can be avoided, because BASH_VERSINFO was added to Bash v2.0
#

(( __zc_BASH_VERSION = BASH_VERSINFO[0] * 1000000 + BASH_VERSINFO[1] * 1000 + BASH_VERSINFO[2] ))

#
# Give up if Bash is too old (currently Bash < 3.1).
#
# Bash v3.1 added « VAR+=TEXT » and « ARRAY+=(ITEM) » which we use copiously.
# With some effort these could be avoided, but incrementally building a list
# using « array=("${array[@]}" item) » (or using « set -- "$@" item ») takes
# quadratic time, causing noticeable delays even for lists as small as 100
# items, and intolerable delays on lists of 1000 items.
#
(( 3001000 <= __zc_BASH_VERSION )) || return

#
# Compensate for shortcomings of earlier versions of Bash
#

__zc_can_localize_flags=1           # can do « local - »
__zc_has_maps=1                     # has associative arrays
__zc_has_read_alarm_status=1        # « read -t$seconds » returns SIGALRM status on timeout
__zc_has_varredir=1                 # can do « {var}> ... » redirection
__zc_has_xtracefd=1                 # output triggered by « set -x » can go somewhere other than stderr
__zc_read_n1=-N1                    # « read -N$bytes » is understood
__zc_read_t01=-t0.1                 # « read -t$seconds » can understand fractions

__zc_ts() { printf "%($*)T" -1 ; }  # timestamp (without trailing newline)

if (( __zc_BASH_VERSION < 4000000 ))
then
    # Note [4.0]
    #
    # Bash v4.0 added the ability to separate xtrace output from stderr, by
    # redirecting it to fd « $BASH_XTRACEFD ». (It's harmless to set this
    # variable in earlier versions, but then xtrace output will still be
    # comingled with stderr; so we only use this setting for debugging)
    __zc_has_xtracefd=0
    #
    # Bash v4.0 added support for associative arrays (maps).
    # For numeric variables in older versions use « ((mapname_$x)) » instead of
    # « ((mapname[$x])) ».
    __zc_has_maps=0
    #
    # Bash v4.0 added support for fractional timeouts with « read -t$seconds »,
    # and also sets the return code as if read had been killed by SIGALRM.
    # For older versions, use « read -t1 » instead, which will cause
    # user-observable delays when pressing ESC, and will increase the
    # likelihood of oddities if an incomplete sequence is received followed by
    # another key within the second.
    __zc_has_read_alarm_status=0
    __zc_read_t01=-t1
fi

if (( __zc_BASH_VERSION < 4001000 ))
then
    # Note [4.1]
    #
    # Bash v4.1 added support for « read -N$num ».
    # For older versions, use « read -n$num » instead, which may cause issues
    # if the tty's eol or eol2 is set to some character that's embedded within
    # a key's escape sequence. (In particular this may apply when eol2 is set
    # to ESC by readline in vi mode.)
    __zc_read_n1=-n1
    #
    # Bash v4.1 added support for « {var}> » redirection.
    # For older versions, use fixed numbers for filedescriptors.
    #
    # (Currently we don't make any use of this feature.)
    __zc_has_varredir=0
fi

if (( __zc_BASH_VERSION < 4002000 ))
then
    # Note [4.2]
    #
    # Bash v4.2 added support for printf format specifier « %(...)T ».
    # For older versions, replace __zc_ts with a version that calls the
    # external « date » command instead, but at most once per __zc_DateTicks
    # seconds (based on when SECONDS changes), or whenever the format («$*»)
    # changes.
    __zc_ts() {
        if (( __zcpsec + __zc_DateTicks <= SECONDS || __zcpsec > SECONDS )) || [[ "$*" != "$__zcpfmt" ]]
        then
            (( __zcpsec = SECONDS ))
            __zcpfmt="$*"
            __zcdate=$( date +"$__zcpfmt" )
        fi
        printf '%s' "$__zcdate"
    }
fi

if (( __zc_BASH_VERSION < 4004000 ))
then
    # Note [4.4]
    #
    # Bash v4.4 added support for « local - »
    # For older versions of bash, use « local _zc_savedash=$- » and then
    # « set ${-:++${-//[iloprs]}} ${_zc_savedash:+-$_zc_savedash} » to unwind any changes
    #
    # (Currently we don't make any use of this feature.)
    __zc_can_localize_flags=0
fi

################################################################################
#
# User-configurable behaviour
#

__zc_DateTicks=1        # invoke "date" no more than once every second or so (before Bash version 4.2)
__zc_FastAnsi=0         # use hard-coded ANSI escape sequences, rather than tput
__zc_ForceCols=0        # set non-zero to override terminal height
__zc_ForceRows=0        # set non-zero to override terminal width
__zc_LeftMargin=1       # indent menu
__zc_MaxCols=223        # } limited user preference; note that mouse tracking can't
__zc_MaxRows=223        # } report rows or columns higher than 223=0xff-0x20
__zc_MinCols=24         # skip menu if terminal narrower than this
__zc_MinRows=3          # skip menu if terminal shorter than this
__zc_MouseTrack=1       # arbitrary user preference (numeric true/false)
__zc_PaddingCols=2      # leave gaps between columns
__zc_RightMargin=1      # don't use rightmost column in terminal, to avoid auto-right-margin causing problems
#__zc_Resizeable=1      # terminal is in a window that can be resized

#   The following is commented out because it's not valid syntax before Bash
#   v4.0, and it's only a template for you to define your own map.
#
#   Although it's possible to map from the byte sequence for a key to any other
#   byte sequence, thus mimicking another key, it's better to use these
#   symbolic replacements:
#       ACCEPT      accept the current selection    CR, NL, Space
#       CANCEL      don't select anything           ESC, ctrl-C, Menu
#       IGNORE      do nothing
#       NEXT        move selection down one         ctrl-N  tput-kcuu1
#       PREV        move selection up one           ctrl-P  tput-kcud1
#       REDRAW      re-display menu                 ctrl-L
#       REDRAWNOW   re-display menu immediately
#
#       SIGALRM SIGINT SIGQUIT SIGWINCH
#
if (( __zc_has_maps ))
then
    declare -A __zc_KeyMap
    __zc_KeyMap=(
#       [$'\t']=ACCEPT
#       [' ']=NEXT
    )
fi

################################################################################
#
# Note [ANSI]
#
# This script only works with ANSI-style terminals and terminal emulators, such
# as Xterm; it can cope with resizable terminal windows.
#
# It also requires either "tput -S" (and a version that understands "rows" &
# "lines", so probably any POSIX compliant version), or "stty size" (which
# probably means only GNU's "stty").
#
# TODO: add support for non-ANSI terminals, maybe, someday.
#

[[ "$( tput cup 9 6 ; tput clear )" = $'\e[10;7H\e['*'J' ]] || return   # doesn't seem to be an ANSI terminal

case $TERM in
(ansi*\
|cons*\
|linux*\
|vt???*\
)               __zc_FastAnsi=1
                __zc_MouseTrack=0
              # __zc_Resizeable=0
                ;;    # console

([Pp][Uu][Tt][Tt][Yy]*\
|screen*\
)               __zc_FastAnsi=1
                __zc_MouseTrack=0 ;;    # window terminal, but don't trust mouse tracking

(*[vwxy]term*\
|cygwin*\
|rxvt*\
|wsvt*\
)               __zc_FastAnsi=1 ;;      # resizable terminal, with mouse

(*)             __zc_MouseTrack=0
                ;;    # assume fixed-size terminal, without mouse
esac

if ((__zc_FastAnsi))
then

    __zc_cKeyUp=$'\e[A'     # or $'\eOA'
    __zc_cKeyDn=$'\e[B'     # or $'\eOB'
    __zc_cKeyRt=$'\e[C'     # or $'\eOC'
    __zc_cKeyLf=$'\e[D'     # or $'\eOD'
    __zc_cKeyHm=$'\e[H'     # or $'\eOH'
    __zc_cKeyEn=$'\e[F'     # or $'\eOF'
    __zc_cKeyPU=$'\e[5~'
    __zc_cKeyPD=$'\e[6~'

    __zc_cMoveU() { local rows=$(($1)) ; ((rows)) && printf '\e[%uA' "$rows" ; }
    __zc_cMoveD() { local rows=$(($1)) ; ((rows)) && printf '\e[%uB' "$rows" ; }
    __zc_cMoveR() { local cols=$(($1)) ; ((cols)) && printf '\e[%uC' "$cols" ; }
    __zc_cMoveL() { local cols=$(($1)) ; ((cols)) && printf '\e[%uD' "$cols" ; }

    __zc_cSaveCursor=$'\e7'
    __zc_cRestCursor=$'\e8'
    __zc_cClearEoL=$'\e[K'

    __zc_cNormal=$'\e[22;33;40m'    # show list in dim yellow-on-black
    __zc_cSelect=$'\e[1;37;40m'     # show highlit item in bright-white-on-black
    __zc_cEnd=$'\e[39;49;21m'       # go back to "normal" colours

    __zc_cReportCursor=$'\e[?6n'

else

    __zc_cKeyUp=$( tput kcuu1 )     # or '\e[A' or '\eOA'
    __zc_cKeyDn=$( tput kcud1 )     # or '\e[B' or '\eOB'
    __zc_cKeyRt=$( tput kcuf1 )     # or '\e[C' or '\eOC'
    __zc_cKeyLf=$( tput kcub1 )     # or '\e[D' or '\eOD'
    __zc_cKeyHm=$( tput khome )     # or '\e[H' or '\eOH'
    __zc_cKeyEn=$( tput kend )      # or '\e[F' or '\eOF'
    __zc_cKeyPU=$( tput kpp )       # or '\e[5~'
    __zc_cKeyPD=$( tput knp )       # or '\e[6~'

    declare -a __zc_cMoveU ; __zc_cMoveU() { local rows=$(($1)) ; printf %s "${__zc_cMoveU[rows]=$( ((rows)) && tput cuu "$rows" )}" ; }   # '\e[%uA'
    declare -a __zc_cMoveD ; __zc_cMoveD() { local rows=$(($1)) ; printf %s "${__zc_cMoveD[rows]=$( ((rows)) && tput cud "$rows" )}" ; }   # '\e[%uB'
    declare -a __zc_cMoveR ; __zc_cMoveR() { local cols=$(($1)) ; printf %s "${__zc_cMoveR[cols]=$( ((cols)) && tput cuf "$cols" )}" ; }   # '\e[%uC'
    declare -a __zc_cMoveL ; __zc_cMoveL() { local cols=$(($1)) ; printf %s "${__zc_cMoveL[cols]=$( ((cols)) && tput cub "$cols" )}" ; }   # '\e[%uD'

    __zc_cSaveCursor=$( tput sc )   # or '\e7'
    __zc_cRestCursor=$( tput rc )   # or '\e8'
    __zc_cClearEoL=$( tput el )     # or '\e[K'

    __zc_cNormal=$( tput dim        # show list in dim yellow-on-black $'\e[22;33;40m'
                    tput setaf 3
                    tput setab 0 )
    __zc_cSelect=$( tput bold       # show highlit item in bright-white-on-black $'\e[1;37;40m'
                    tput setaf 3
                    tput setab 0 )
    __zc_cEnd=$( tput sgr0 )        # go back to "normal" colours $'\e[39;49;21m'

    # Compact CSI...m CSI...m CSI...m into CSI...;...;...m
    __zc_cNormal=${__zc_cNormal//$'m\e['/';'}
    __zc_cSelect=${__zc_cSelect//$'m\e['/';'}

    __zc_cReportCursor=$( tput u7 ) # '\e[?6n'

fi

# no tput equivalents
__zc_cStartTrackingMouse=$'\e[?1003h'
__zc_cStopTrackingMouse=$'\e[?1003l'

################################################################################
#
# stackable at-exit
#

declare -a _zc_atexit=()

################################################################################
#
# logging & debugging output
#
# We reserve fd#7 for debug output; discard until we decide what to do with it.
#
# __zclog does not affect the current $? exit status, so you can write
#
#   fubar || __zclog ... || exit
#
# If the first args is with «-@» or «-@num», then the following arg is a format
# string, and «num» denotes the number of arguments used by that format string;
# «num» may be an expression, or may be empty, in which case all remaining args
# are used.
# There may be multiple «-@num fmt args...» groups.
#
# * If the format string contains a «%» then it is used directly with printf;
#   this will implicitly repeat the format string as many times as necessary to
#   consume all the args.
# * If the format string is «[]» then a special list enumeration format is
#   used, producing «[arg,arg,arg...]», or «-» if the list of args is empty.
# * Otherwise the format string is still used directly with printf, but with
#   «=arg» appended as many times as necessary.
#
# If the first arg is not «-@num», or if the «-@num fmt args...» groups do not
# consume all the args, then any remaining args are printed on separate lines.
# Output finishes with a newline.

exec 7>/dev/null

__zclog() {
    local _zc_r=$?      # be transparent for exit code
    [[ -e /dev/fd/7 || ! -d /dev/fd ]] && { # avoid complaints about bad filedescriptors
    __zc_ts %F,%T
    printf ' [%u] ' $$
    local arg argc fmt pre post sep empty
    while [[ $1 = '-@'* ]] ; do
        arg=${1#-?}
        shift
        argc=$# fmt= pre= post= sep=, empty=

        case $arg in
        '') ;;
        *[!0-9]*)   break ;;
        *)          argc=$((arg+1)) ;;
        esac

        fmt=$1
        ((--argc))
        shift

        case $fmt in
        (*\%*)  empty=- sep= ;;
        ([])    pre=\[ post=\] empty=- fmt=%q ;;
        (*)     empty=$fmt fmt+==%q ;;
        esac

        if ((argc==0))
        then
            # shellcheck disable=SC2059
            printf "$empty"
        else
            printf %s "$pre"
            if [[ $sep ]]
            then
                # shellcheck disable=SC2059
                printf -- "$fmt" "${@:1:1}"
                # shellcheck disable=SC2059
                ((argc>1)) &&
                printf -- "$sep$fmt" "${@:2:argc-1}"
            else
                # shellcheck disable=SC2059
                printf -- "$fmt" "${@:1:argc}"
            fi
            printf %s "$post"
        fi

        # discard
        shift $(( 0<=argc && argc<=$# ? argc : $# ))
    done
    (($#)) && printf '\n\t%s' "$@"
    printf '\n'
  } >&7 2>&1
    return $((_zc_r))
}

#
# Note [4.1]
#   {var}> redirections were added in version 4.1 - but avoid them.
#
#   git blame parse.y | grep REDIR_
#   ⇒ 0001803 2011-11-21 20:51:19 -0500 Bash-4.1 distribution source
#

declare -i __zc_nextmask=0 __zc_mask=0
declare -i __zc_loglevel=3 __zc_xtrace_mode=-1 __zc_xtrace_to_log=0 __zc_log_to_xtrace=0
declare __zc_log_to_fd=2 __zc_log_to_file=

# shellcheck disable=SC2034 disable=SC2088
declare -i __zc_maskname_COMPLETE=$(( 1 << __zc_nextmask++ )) \
           __zc_maskname_GEN=$(( 1 << __zc_nextmask++ )) \
           __zc_maskname_SORTMAIN=$(( 1 << __zc_nextmask++ )) \
           __zc_maskname_WRAP=$(( 1 << __zc_nextmask++ )) \
           __zc_maskname_ZCOMP=$(( 1 << __zc_nextmask++ )) \
           __zc_maskname_ALL='~0'

__zc_set_debug() {
    local j k
    local -a _zc_level_names=( silent errors warnings info verbose tracing debug everything )
    local -a _zc_xtrace_modes=( off inherit on )

    for j do
        case $j in
        help|--help|-h) cat <<EndOfHelp
__zc_set_debug [ level=LEVEL ] [ xtrace=XMODE ] [ file=FILENAME | nofile ] [ OPTIONS ... ]

Control what happens to debug & xtrace output while in the menu functions.

level=LEVEL
  0   silent
  1   enable errors
  2   enable warnings & errors
  3   enable info, warnings & errors (info = report all user requests)
  4   enable verbose, info, warnings & errors (verbose = report all mutating actions)
  5   enable tracing, verbose, info, warnings & errors (tracing = explain choices)
  6   enable debug as per mask, tracing, verbose, info, warnings & errors
  7   enable everything (all debugging, overriding the selection mask, and all other options)

(The xtrace output from the completion menu code is insanely long, and
usually not useful to the rest of what's happening in the shell, so suppress
it by default, but allow it to be turned on so that the completion menu code
itself can be debugged.)

xtrace=XMODE
  off         turn off xtrace inside completion (and restore on return; default)
  inherit     don't turn off xtrace inside completion
  on          turn on xtrace inside completion (and restore on return)

log-to-fd
log-to-file
log-to-xtrace     merge xtrace & __zclog output

xtrace-to-log     merge xtrace & __zclog output inside completion menu
xtrace-split      don't merge xtrace & __zclog

If an alternative logfile is given, __zclog output will go there when inside a
completion function; $(
if ((__zc_has_xtracefd)) ; then
    echo            'xtrace output will also go there, since this version of
Bash supports BASH_XTRACEFD'
else
    echo            'however xtrace output will go to stderr instead, since
this version of Bash does not support BASH_XTRACEFD'
fi                  ).

Unspecified options are treated as flags to enable or disable debugging in
particular modules.
EndOfHelp
                        return 0 ;;

        level=*)        __zc_loglevel=${j#*=} ;;
        xtrace=inherit) __zc_xtrace_mode=0 ;;
        xtrace=on)      __zc_xtrace_mode=1 ;;
        xtrace=off)     __zc_xtrace_mode=-1 ;;
        xtrace-to-log)  __zc_log_to_xtrace=0
                        __zc_xtrace_to_log=1
                        ;;
        xtrace-split)   __zc_log_to_xtrace=0
                        __zc_xtrace_to_log=0
                        ((__zc_has_xtracefd)) ||
                            __zcwarn "xtrace-split won't work on this version of Bash"
                        ;;
        log-to-xtrace)  __zc_log_to_xtrace=1
                        __zc_log_to_fd=
                        __zc_log_to_file=
                        __zc_xtrace_to_log=0
                        ;;
        log-to-fd=*)    __zc_log_to_xtrace=0
                        __zc_log_to_fd=${j#*=}
                        __zc_log_to_file=
                        ;;
        log-to-file=-|log-to-stderr)
                        __zc_log_to_xtrace=0
                        __zc_log_to_fd=2
                        __zc_log_to_file=
                        ;;
        log-to-file=*)  __zc_log_to_xtrace=0
                        __zc_log_to_fd=
                        __zc_log_to_file=${j#*=}
                        ;;
        xtrace*)        printf >&2 '__zc_set_debug: invalid "xtrace" option "%s"\n' "$j" ;;
        log-to*)        printf >&2 '__zc_set_debug: invalid "log-to" option "%s"\n' "$j" ;;
        [!-+=_A-Za-z0-9]*|?*[!_A-Za-z0-9]*|[-+=])
            printf >&2 '__zc_set_debug: invalid option "%s"\n' "$j" ;;
        *)
            k=${j#[-+=]} j=${j%"$k"}
            if ! [[ ( $k = 0*     && $k != *[!0-7]* ) ||        # Zero or Octal
                    ( $k = [1-9]* && $k != *[!0-9]* ) ||        # Decimal
                    ( $k = 0x*    && $k != ??*[!0-9a-f]* ) ]]   # Hexadecimal
            then
                k=__zc_maskname_$k  # see version note [4.0]
                # shellcheck disable=SC2004
                (( k || ( $k = 1 << __zc_nextmask++ ) ))
            fi
            (( k = k ))
            case $j in
            +)      (( __zc_mask |=  k )) ;;
            -)      (( __zc_mask &=~ k )) ;;
            =|'')   (( __zc_mask  =  k )) ;;
            esac ;;
        esac
    done

    if [[ -n $__zc_log_to_fd ]]
    then
        # Use a log fd, if specified
        exec 7>&$((__zc_log_to_fd))
    elif [[ -n $__zc_log_to_file ]]
    then
        # Use a logfile, if specified
        exec 7>> "$__zc_log_to_file" || return
        {
        __zc_ts %F,%T
        printf ' [%u] ' $$
        printf ' LOGGING STARTED\n'
        printf '\tlevel=%d (%s)\n' "$__zc_loglevel" "${_zc_level_names[__zc_loglevel]:-unnamed}"
        printf '\txtrace=%d (%s)\n' "$__zc_xtrace_mode" "${_zc_xtrace_modes[__zc_xtrace_mode+1]:-unnamed}"
        printf '\tlog-to-'
        [[ $__zc_log_to_fd ]] && printf 'fd=%d ' "$__zc_log_to_fd"
        [[ $__zc_log_to_file ]] && printf 'file=%s ' "$__zc_log_to_file"
        (( __zc_log_to_xtrace )) && printf 'xtrace '
        printf '\n'
        printf '\tmask=%#x\n' "$__zc_mask"
        } >&7
    elif (( __zc_log_to_xtrace ))
    then
        # Use current BASH_XTRACEFD, or failing that stderr.
        exec 7>&"${BASH_XTRACEFD:-2}"
    elif (( __zc_loglevel > 0 || __zc_xtrace_to_log ))
    then
        # Fall back to stderr if either non-silent or xtrace-to-log
        exec 7>&2
    else
        # No logging expected; discard any that occurs
        exec 7>/dev/null
    fi

    if (( __zc_loglevel > 0 )) ; then __zcerror()   { __zclog "$@" ; } ; else __zcerror()   { return $? ; } ; fi
#   if (( __zc_loglevel > 1 )) ; then __zcwarn()    { __zclog "$@" ; } ; else __zcwarn()    { return $? ; } ; fi
    if (( __zc_loglevel > 2 )) ; then __zcinfo()    { __zclog "$@" ; } ; else __zcinfo()    { return $? ; } ; fi
#   if (( __zc_loglevel > 3 )) ; then __zcverbose() { __zclog "$@" ; } ; else __zcverbose() { return $? ; } ; fi
#   if (( __zc_loglevel > 4 )) ; then __zctrace()   { __zclog "$@" ; } ; else __zctrace()   { return $? ; } ; fi
    if (( __zc_loglevel > 5 )) ; then
        __zcdebug() {
            local -i k=__zc_maskname_$1  # see version note [4.0]
            (( __zc_mask & k )) || return 1
            shift
            __zclog "$@"
            return 0
        }
        if (( __zc_loglevel > 6 )) ; then ((__zc_mask=~0)) ; fi
                                                                         else __zcdebug()   { return 1 ; } ; fi
}

#
# If bash was invoked with "-x" (so xtrace is on when this file is loaded),
# turn on all possible debugging; otherwise turn all debugging off.
#

if [[ $- = *x* ]]
then
    # Assume that if set -x is on when you load zcomp, it's because you want to
    # debug zcomp itself.
    __zc_set_debug log-to-file="$HOME/tmp/_zcomp.$$.log" xtrace-to-log level=7 +ALL
    __zclog -@2 'Starting zcomp loading, pid=%u tty=%s\n' $$ "${TTY:=$( tty )}"
    _zc_loaderfinish() {
        __zclog -@3 'Finished loading zcomp, pid=%u tty=%s ex=%#x' $$ "${TTY:=$( tty )}" $?
        set -x
    }
    _zc_atexit+=( _zc_loaderfinish )
else
    __zc_set_debug level=0 log-to-xtrace -ALL
fi

################################################################################

    # sort COMPREPLY[] using a bottom-up heapsort and simultaneously remove duplicates

    __zc_sort() {
        local i j k n=${#COMPREPLY[@]} o t u
        __zcdebug sortmain -@1 'sort.start %u items' "$n" \
                           -@ '\n %q' "${COMPREPLY[@]}"
        for (( i=2 ; i<=n ; ++i )) do
            t=${COMPREPLY[n-i]}
            for (( j=i ; (k=j>>1)>=1 ; j=k )) do
                [[ $t < ${COMPREPLY[n-k]} ]] || break
                COMPREPLY[n-j]=${COMPREPLY[n-k]}
            done
            if (( j!=i ))
            then
                COMPREPLY[n-j]=$t
            fi
        done
        __zcdebug sortmain -@1 'sort.heaped  %u:' ${#COMPREPLY[@]} \
                           -@ '\n %q' "${COMPREPLY[@]}"
        for (( i=n, o=0 ; i>0 ;)) do
            t=${COMPREPLY[n-i]}
            u=${COMPREPLY[n-1]}
            ((--i))
            for (( j=1 ; (k=j<<1)<=i ; j=k )) do
                if  ((k<i)) &&
                    [[ ${COMPREPLY[n-1-k]} < $t ]]
                then
                    [[ ${COMPREPLY[n-1-k]} < ${COMPREPLY[n-k]} ]] && ((++k))
                else
                    [[ ${COMPREPLY[n-k]} < $t ]] || break
                fi
                COMPREPLY[n-j]=${COMPREPLY[n-k]}
            done
            COMPREPLY[n-j]=$t
            if ((o==0)) || [[ $u != "${COMPREPLY[o-1]}" ]]
            then
                COMPREPLY[o++]=$u
            fi
        done
        for ((; o<n ; ++o )) do
            unset "COMPREPLY[o]"
        done
        __zcdebug sortmain -@1 'sort.done  %u:' ${#COMPREPLY[@]} \
                           -@ '\n %q' "${COMPREPLY[@]}"
    }

    __zc_eval() {
        # Handle the command provided by the -C option.
        # Take the first arg as the command string to be eval'ed, and leave the
        # remaining positional args so that they're available to that command if
        # wanted.
        # (Completion will provide 3 arguments: the command name, the word to
        # complete, and the preceding word; these same arguments are also provided
        # to functions nominated with '-F'.)
        local __zce=$1
        shift
        eval "$__zce"
    }

    #
    # Add newline-delimited items from a string to the COMPREPLY array
    #
    __zc_genadd() {
        local _zc_savedash=${-//[!f]} # save globbing state
        set -f              # disable globbing
        local IFS=$'\n'
        COMPREPLY+=( $1 )   # intentionally unquoted; only splits on newlines
        # restore globbing, if it was previously on
        set +f ${_zc_savedash:+"-$_zc_savedash"}
    }

    #
    # Emulate the normal sequence for completion.
    # Mostly it can be handled by compgen, however -C & -F don't work as
    # advertised when invoked via compgen, so do them directly.
    #
    # This is called from several places in _zcomp, which is called from the
    # wrapper function for each bound completion.
    #
    # The args passed to the wrapper function are provided here as _zc_genargs;
    # these must be provided in turn to any -Ffunction or -Ccommand.
    #
    __zc_gen() {
        local _zc_list
        __zcdebug gen -@0 'ZCGEN START'
        COMPREPLY=()
        if (( ${#_zc_genfunc[@]} ))
        then
            "${_zc_genfunc[@]}" "${_zc_genargs[@]}"
            __zcdebug gen -@0 'ZCGEN FUNC ' \
                          -@${#_zc_genfunc[@]}+${#_zc_genargs[@]} "${_zc_genfunc[@]}" "${_zc_genargs[@]}" \
                          -@1 ' returned %x, filled COMPREPLY with ' $? \
                          -@  [] "${COMPREPLY[@]}"
        fi
        if (( ${#_zc_gencmd[@]} ))
        then
            _zc_list=$( __zc_eval "${_zc_gencmd[@]}" "${_zc_genargs[@]}" )
            __zcdebug gen -@1 'ZCGEN CMD' \
                          -@${#_zc_gencmd[@]}+${#_zc_genargs[@]} [] "${_zc_gencmd[@]}" "${_zc_genargs[@]}" \
                          -@1 ' returned %x, replied ' $? \
                          -@1 [] "$_zc_list"
            __zc_genadd "$_zc_list"
        fi
        if (( ${#_zc_compgen_args[@]} ))
        then
            _zc_list=$( compgen -o nospace "${_zc_compgen_args[@]}" -- "${COMP_WORDS[COMP_CWORD]}" )
            __zcdebug gen -@0 'ZCGEN COMPGEN' \
                          -@3+${#_zc_compgen_args[@]} [] compgen -o nospace "${_zc_compgen_args[@]}" \
                          -@1 ' returned %x, replied ' $? \
                          -@  [] "$_zc_list"
            __zc_genadd "$_zc_list"
        fi

        __zc_sort       # sort COMPREPLY[] and remove duplicates

        # count of items & last item, used in lots of places
        (( _zc_num_items = ${#COMPREPLY[@]}, _zc_last_item = _zc_num_items-1 ))
        # find maximum column width, in _zc_max_item_width
        for (( _zc_max_item_width = 0, _zcj = 0 ; _zcj < _zc_num_items ; ++_zcj )) do
            (( _zc_max_item_width > ${#COMPREPLY[_zcj]} || ( _zc_max_item_width = ${#COMPREPLY[_zcj]} ) ))
        done
        __zcdebug gen -@0 'ZCGEN END ' \
                      -@ [] "${COMPREPLY[*]}"
        # Set return status so that _zcomp bails out if fewer than two options remain
        (( _zc_num_items > 1 )) && _zc_resize=1
    }

################################################################################

    __zc_get_term_size() {
        # Override for debugging
        (( ( _zc_scrn_rows = __zc_ForceRows ) > 0 &&
           ( _zc_scrn_cols = __zc_ForceCols ) > 0 )) && return

        # Hopefully Bash has set LINES & COLUMNS for us...
        (( ( _zc_scrn_rows = LINES ) > 0 &&
           ( _zc_scrn_cols = COLUMNS - __zc_LeftMargin - __zc_RightMargin ) > 0 )) && return

        # If that didn't work, try "stty" and then "tput"
        read -r   LINES            COLUMNS _   < <( stty size     2>/dev/null ) ||
        { read -r LINES && read -r COLUMNS ; } < <( tput -S <<<$'lines\ncols' ) &&
        (( LINES > 0 && COLUMNS > 0 &&
            ( _zc_scrn_cols = COLUMNS - __zc_LeftMargin - __zc_RightMargin,
              _zc_scrn_rows = LINES )
        ))
    }

    __zc_getkey() {
        local -a _zc_timeout=()
        (($1)) && _zc_timeout=( "$__zc_read_t01" )
        local __zgk_chr __zgk_restore_traps=$( trap -p ) __zgk_signal
        for __zgk_signal in SIGINT SIGQUIT
        do
            trap "  __zgk_chr=\$?
                    _zc_key=$__zgk_signal
                    $__zgk_restore_traps
                    return \$((__zgk_chr|128))
                 " "$__zgk_signal"
        done
        IFS= \
        read -rs -d '' "$__zc_read_n1" "${_zc_timeout[@]}" _zc_key || {
            local _zc_exitcode=$?
            if ((_zc_exitcode & 128))
            then
                # Interrupted by a signal, or timed out
                local -a _zc_signals
                eval "$( trap -l |
                         tr $'\t' $'\n' |
                         sed -n ' /[-+]/d;
                                  s/^ */_zc_signals[/;
                                  s/) /]=/p;
                                ' )"
                _zc_key=${_zc_signals[_zc_exitcode & 127]:-SIG#$_zc_exitcode}
                case $_zc_key in
                (SIG*)
                    trap _zc_key=SIGINT SIGINT
                    trap _zc_key=SIGQUIT SIGQUIT
                    return 0 ;;
                esac
            fi
            trap _zc_key=SIGINT SIGINT
            trap _zc_key=SIGQUIT SIGQUIT
            return $((_zc_exitcode))
        }
        while
            case "$_zc_key" in
            ('')  _zc_key=$'\n' ; break ;;          # compensate for bug in "read"
            ( $'\e[M'??? ) break ;;                 # mouse report
            ( $'\e' | $'\e'[\[O] | $'\e['[?O] | $'\e['*[\;0-9] | $'\e[M'* ) ;;
            ( [$'\xc0'-\$'xfe'] \
            | [$'\xe0'-\$'xfe'][$'\x80'-$'\xbf'] \
            | [$'\xf0'-\$'xfe']?[$'\x80'-$'\xbf'] \
            | [$'\xf8'-\$'xfe']??[$'\x80'-$'\xbf'] \
            | [$'\xfc'-\$'xfe']???[$'\x80'-$'\xbf'] \
            | [$'\xfe'-\$'xfe']????[$'\x80'-$'\xbf'] ) ;;  # don't stop on incomplete UTF-8 prefix
            (*) break ;;
            esac
            IFS= \
            read -rs -d '' "$__zc_read_n1" "$__zc_read_t01" __zgk_chr
        do
            [[ -z "$__zgk_chr" ]] && __zgk_chr=' '  # compensate for bug in "read" ?
            _zc_key="$_zc_key$__zgk_chr"
        done
        trap _zc_key=SIGINT SIGINT
        trap _zc_key=SIGQUIT SIGQUIT
        return 0
    }

#
# _zcomp part 1: ensure that xtrace and traps are properly restored, regardless
# of how part 2 returns
#
_zcomp() {
    local _zc_rc=$? _zc_savedash=${-//[iloprs]}
    set +x
    local _zc_undotrap=$( trap -p SIGINT SIGQUIT SIGWINCH )

    if (( __zc_xtrace_mode > 0 )) ||
     { (( __zc_xtrace_mode == 0 )) && [[ $_zc_savedash = *x* ]] ;}
    then
        (( __zc_xtrace_to_log )) && local BASH_XTRACEFD=7
        set -x
    fi

    _zcomp2 "$@" 2>&7

    # Revert signal handlers
    eval "$_zc_undotrap"
    _zc_rc=$?

    # Revert the « set » options.
    # Options -i, -l, -p, -r & -s can only be set at invocation and not unset.
    # Option -o takes a parameter and should not be present, but will break if
    # it is.
    # (It turns out that « set +${-//[iloprs]} -$oldopts » works even if $- or
    # $oldopts is empty, but it's undocumented so don't rely on it.)
    local _zc_undodash=${-//[iloprs]}
    set ${_zc_undodash:+"+$_zc_undodash"} ${_zc_savedash:+"-$_zc_savedash"} --
    return $((_zc_rc))
}

#
# __zc_itempos takes 1 parameter:
#   1) the item index to display
#
# uses caller's _zc_num_rows, _zc_col_offset, _zc_col_width
#
__zc_itempos() {
    local -i _zci=$1 _zc_row _zc_dcol
    (( _zc_row  = _zci%_zc_num_rows,
       _zc_dcol = _zci/_zc_num_rows - _zc_col_offset ))
    # restore cursor position
    printf %s "$__zc_cRestCursor"
    # move down to item row (row 0 is the one after the command line)
    __zc_cMoveD $(( _zc_row+1 ))
    # move to left margin
    printf '\r'
    # move right to item column
    __zc_cMoveR $(( _zc_dcol*(_zc_col_width+__zc_PaddingCols)+__zc_LeftMargin ))
}

#
# __zc_item takes 2 parameters:
#   1) the item index to display
#   2) whether it should be highlighted
#
# uses caller's _zc_col_width
#
__zc_item() {
    local -i _zci=$1 _zc_selected=$2
    local _zct="${COMPREPLY[_zci]}"
    # prune to fit, if necessary
    _zct=${_zct:0:_zc_col_width}
    # highlighted or plain?
    if (( _zc_selected ))
    then printf %s "$__zc_cSelect"
    else printf %s "$__zc_cNormal"
    fi
    # show current item,
    printf "%-*s" $(( _zc_col_width )) "$_zct"
    # back to normal text
    printf "%s" "$__zc_cEnd"
}

#
# _zcomp part 2: generate completion list, show menu, accept user selection
#
_zcomp2() {
    __zcdebug zcomp -@0 'Starting completion: args=' \
                    -@$# [] "$@" \
                    -@0 ' COMPREPLY=' \
                    -@ [] "${COMPREPLY[@]}"
    local -a _zc_genfunc=( "${@:2:$1}" )       ; shift $(($1+1))
    local -a _zc_gencmd=( "${@:2:$1}" )        ; shift $(($1+1))
    local -a _zc_compgen_args=( "${@:2:$1}" )  ; shift $(($1+1))
    local -a _zc_genargs=( "${@:2:$1}" )       ; shift $(($1+1))
    # The completion system provides 3 arguments:
    #   1. the first word (the name of the command being used to look up the completion);
    #   2. the word being completed (empty, if after a space); and
    #   3. the previous word (if any)

    (( $# == 0 )) || __zclog 'Invalid call to _zcomp with trailing args' || return 1

    local _zc_button _zc_key
    local -i _zc_first=1 _zc_redraw_needed _zc_redraw_now _zc_resize=1
    local -i _zc_col_offset _zc_col_width _zc_cur _zc_mcol _zc_mrow
    local -i _zc_last_item _zc_last_dcol _zc_num_items _zc_num_dcols _zc_num_rows _zc_num_vcols
    local -i _zc_prev_num_cols _zc_prev_num_rows _zc_saved_row _zc_scrn_cols _zc_scrn_rows
    local -i _zc_max_item_width _zcj _zck _zcl _zcm

    (( _zc_col_offset=0, _zc_cur=0, _zc_prev_num_cols=-1, _zc_prev_num_rows=-1 ))

    { __zc_gen && [[ "${COMP_TYPE:-9}" = 9 ]] ; } || {
        # Avoid showing menu if either
        # (a) insufficient items, or
        # (b) non-immediate COMP_TYPE
        __zcdebug zcomp \
                -@2 'Early completion: TYPE=%s COUNT=%u' "$COMP_TYPE" "$_zc_num_items" \
                -@0 ' COMPREPLY=' \
                -@ [] "${COMPREPLY[@]}"
        return 0
    }

    trap _zc_key=SIGINT SIGINT
    trap _zc_key=SIGQUIT SIGQUIT
    trap _zc_redraw=1 SIGWINCH SIGCONT

    while
        if (( _zc_resize || _zc_num_rows <= 0 || _zc_scrn_cols <= 0 || _zc_scrn_rows <= 0 ))
        then
            (( _zc_resize )) ||
                # (shouldn't happen; report as a bug)
                __zcerror -@ 'ERROR: _zc_num_rows is %s but _zc_resize is false' "$_zc_num_rows"

            # Get terminal dimensions, or play safe and return
            __zc_get_term_size || return

            # If the terminal is too small play it safe and simply return.
            (( _zc_scrn_cols < __zc_MinCols ||
               _zc_scrn_rows < __zc_MinRows )) &&
                return 1

            # Allow for margins (and double-check the minimum size)
            (( ( _zc_scrn_cols -= __zc_LeftMargin - __zc_RightMargin ) > 0 )) ||
                return 1

            # Clip effective terminal size
            (( _zc_scrn_rows <= __zc_MaxRows && __zc_MaxRows || ( _zc_scrn_rows = __zc_MaxRows ),
               _zc_scrn_cols <= __zc_MaxCols && __zc_MaxCols || ( _zc_scrn_cols = __zc_MaxCols ) ))

            # Compute tabular rows & columns
            (( _zc_col_width = _zc_max_item_width,
               _zc_col_width <= _zc_scrn_cols   || ( _zc_col_width = _zc_scrn_cols ),

               _zc_num_dcols = (_zc_scrn_cols+__zc_PaddingCols) / (_zc_col_width+__zc_PaddingCols),
               _zc_num_dcols <= _zc_num_items   || ( _zc_num_dcols = _zc_num_items ),
               _zc_num_dcols > 0                || ( _zc_num_dcols = 1 ),
               _zc_last_dcol = _zc_num_dcols-1,

               _zc_num_rows = 1 + _zc_last_item / _zc_num_dcols,
               _zc_num_rows < _zc_scrn_rows     || ( _zc_num_rows = _zc_scrn_rows-1 ),
               _zc_num_rows > 0                 || ( _zc_num_rows = 1 ),

               _zc_num_vcols = 1 + _zc_last_item / _zc_num_rows ))

            # Resize calculation done; force redraw if size has changed
            (( _zc_resize = 0,
               _zc_prev_num_rows == _zc_num_rows &&
               _zc_prev_num_cols == _zc_num_dcols || ( _zc_redraw_needed = 1 ) ))
        fi

        # Scroll sideways to keep current item in view, by updating _zc_col_offset
        # and forcing a redraw
        (( _zcj = _zc_cur/_zc_num_rows - _zc_last_dcol,
           _zc_col_offset < _zcj && (
           _zc_col_offset = _zcj, _zc_redraw_needed = 1 ),
           _zcj = _zc_cur/_zc_num_rows,
           _zc_col_offset > _zcj && (
           _zc_col_offset = _zcj, _zc_redraw_needed = 1 )))

        # If a redraw has been triggered, defer until the user stops pressing
        # navigation buttons. This relies on __zc_getkey being able to quietly
        # report a timeout, see below.
        if (( _zc_redraw_now || _zc_redraw_needed && !__zc_has_read_alarm_status || _zc_first ))
        then
            # save starting cursor position
            if ((_zc_first))
            then printf %s "$__zc_cSaveCursor"
            else printf %s "$__zc_cRestCursor"
            fi

            # Compute _zck as the index+1 of the bottom item of the rightmost
            # displayed column.
            (( _zck = (_zc_num_dcols+_zc_col_offset)*_zc_num_rows,
               _zck > _zc_num_items && (
               _zck = _zc_num_items )))
            # Display the menu
            for (( _zcj = 0 ; _zcj < _zc_num_rows ; _zcj++ )) do
                printf '\r\n'
                for (( _zcl = _zcj+_zc_col_offset*_zc_num_rows, _zcm = __zc_LeftMargin
                     ; _zcl < _zck
                     ; _zcl += _zc_num_rows, _zcm = __zc_PaddingCols )) do
                    printf '%*s' $(( _zcm )) ''
                    __zc_item $(( _zcl )) $(( _zcl == _zc_cur ))
                done
                printf %s "$__zc_cClearEoL"
            done

            if (( _zc_prev_num_rows > _zc_num_rows ))
            then
                # handle menu shrinkage: erase extra lines, then move cursor back up
                for ((; _zcj < _zc_prev_num_rows ; _zcj++ )) do
                    printf '\r\n'
                    printf %s "$__zc_cClearEoL"
                done
                __zc_cMoveU $((_zc_prev_num_rows-_zc_num_rows))
            elif ((_zc_prev_num_rows < _zc_num_rows))
            then
                # Menu expanded to take more lines (or was first drawn) so
                # re-save cursor position after compensating for scrolling:
                #  - go to previous saved cursor position
                #  - move $_zc_num_rows down without changing column (but
                #    stopping at bottom line of terminal)
                #  - move $_zc_num_rows up without chaning column
                #  - save new cursor position
                printf %s "$__zc_cRestCursor"
                __zc_cMoveD _zc_num_rows
                __zc_cMoveU _zc_num_rows
                printf %s "$__zc_cSaveCursor"
            fi
            (( _zc_prev_num_rows = _zc_num_rows,
               _zc_prev_num_cols = _zc_num_dcols ))

            ((__zc_MouseTrack && _zc_first)) && {
                # Ask Xterm to report current cursor position; this will cause a
                # "current position" «CSI?row;colR» response that will be read in the
                # main loop
                printf %s "$__zc_cReportCursor"
                # Turn on mouse tracking
                printf %s "$__zc_cStartTrackingMouse"
            }

            # Redraw done
            (( _zc_first = _zc_redraw_needed = _zc_redraw_now = 0 ))

            # TODO: optionally park cursor in the command line, rather than in
            # menu, like: printf %s "$__zc_cRestCursor"

            # Put cursor on item
            __zc_itempos $((_zc_cur))

        elif (( ! _zc_redraw_needed ))
        then

            #
            # Just highlight currently selected item:
            #
            __zc_itempos $((_zc_cur))
            __zc_item $((_zc_cur)) 1
            # move cursor back to start of item
            __zc_cMoveL _zc_col_width

            # TODO: optionally park cursor in the command line, rather than in
            # menu, like: printf %s "$__zc_cRestCursor"

        fi

        # If we're in "redraw needed" state, put a timeout on the read, and if
        # the timeout occurs, succeed with _zc_key set to SIGALRM.
        # (If this is an old version of Bash without __zc_has_read_alarm_status
        # then the redraw has been done, so _zc_redraw_needed is false).
        __zc_getkey $((_zc_redraw_needed))  # returns value in _zc_key
    do
        __zcinfo -@ 'Got key %q' "$_zc_key"

        #
        # Unhighlight the currently selected item
        #
        if (( ! _zc_redraw_needed ))
        then
            __zc_itempos $((_zc_cur))
            __zc_item $((_zc_cur)) 0
        fi

        # Terminals don't usually produce the ;1~ variant, but just make sure
        _zc_key=${_zc_key/';1~'/'~'}

        # Re-map key if requested
        # (But only if this version of Bash has associative arrays; fortunately
        # they don't introduce new syntax, so a conditional expansion is OK.)
        ((__zc_has_maps)) &&
            _zc_key="${__zc_KeyMap[$_zc_key]-$_zc_key}"

        case "$_zc_key" in

        # No keypress immediately available when _zc_redraw_needed
        (REDRAWNOW|SIGALRM) _zc_redraw_now=1 ;;

        # ctrl-L (formfeed) to request redrawing
        (REDRAW|$'\f')      _zc_redraw_needed=1 ;;

        ## Capture answer to initial "report cursor position" request
        ($'\e['[?0-9]*\;*R) _zc_key=${_zc_key//[^;0-9]/}\; _zc_saved_row=${_zc_key%%\;*} _zc_key=${_zc_key#*\;} ;;

        ## Space / Enter to confirm item
        (ACCEPT|' '|$'\r'|$'\n')
                            break ;;

        ## Escape / Menu / ctrl-C / SIGINT / SIGQUIT to abort
        (CANCEL|$'\e'|$'\e[29~'|$'\x03'|SIG*)
                            _zc_cur=-1
                            break ;;

        (IGNORE|'') ;;      # Ignore mapped key

        ## Mouse tracking
        ($'\e[M`'??)        (( _zc_col_offset > 0                             && --_zc_col_offset )) ;;  # 64 mouse scroll up
        ($'\e[Ma'??)        (( _zc_col_offset < _zc_num_vcols-_zc_num_dcols-1 && ++_zc_col_offset )) ;;  # 65 mouse scroll down
        ($'\e[M'???)        _zc_button="${_zc_key:3:1}"
                            printf -v _zc_mcol '%u-32&255' "'${_zc_key:4:1}"
                            printf -v _zc_mrow '%u-32&255' "'${_zc_key:5:1}"
                            #eval $( printf '(( _zc_mcol=%u-32&255, _zc_mrow=%u-32&255 ))' "'${_zc_key:4:1}" "'${_zc_key:5:1}" )
                            #_zc_mcol=${__zc_CMap["\\${_zc_key:4:1}"]}
                            #_zc_mrow=${__zc_CMap["\\${_zc_key:5:1}"]}
                            (( _zcj = _zc_mrow - (_zc_saved_row-_zc_num_rows+1),
                               _zck = _zc_mcol / (_zc_col_width+__zc_PaddingCols) + _zc_col_offset,
                               _zcj >= 0 && _zcj < _zc_num_rows &&
                               _zck >= 0 && _zck < _zc_num_dcols &&
                               ( _zcj += _zc_num_rows*_zck ) < _zc_num_items &&
                             ( _zc_cur = _zcj ) ))
                            :
                            [[ "$_zc_button" = ' ' ]] && break
                            ;;

        # shift-up - top of column
        ($'\e[1;2A')        (( _zc_cur -= _zc_cur % _zc_num_rows )) ;;
        # shift-down - bottom of column
        ($'\e[1;2B')        (( _zc_cur += _zc_num_rows-1 - _zc_cur % _zc_num_rows,
                               _zc_cur <= _zc_last_item        || ( _zc_cur = _zc_last_item ) )) ;;
        # shift-right - right of row
        ($'\e[1;2C')        (( _zc_cur += (_zc_cur / _zc_num_rows - 1 - (_zc_cur % _zc_num_rows > _zc_last_item % _zc_num_rows)) * _zc_num_rows )) ;;
        # shift-left - left of row
        ($'\e[1;2D')        (( _zc_cur %= _zc_num_rows )) ;;

        # up, shift-tab
        (PREV|$__zc_cKeyUp|$'\e'[\[O][AZ]|$'\x10')
                            (( _zc_cur--,
                               _zc_cur >= 0             || ( _zc_cur = _zc_last_item ) )) ;;
        # down
        (NEXT|$__zc_cKeyDn|$'\e'[\[O]B|$'\t'|$'\x0e')
                            (( _zc_cur++,
                               _zc_cur <= _zc_last_item || ( _zc_cur = 0 ) )) ;;
        # right
        ($__zc_cKeyRt|$'\e'[\[O]C)
                            (( _zc_cur += _zc_num_rows,
                               _zc_cur <= _zc_last_item || ( _zc_cur = (_zc_cur+1) % _zc_num_rows ) )) ;;
        # left
        ($__zc_cKeyLf|$'\e'[\[O]D)
                            (( _zc_cur < _zc_num_rows && (
                                    _zc_cur = (_zc_cur          + _zc_num_rows
                                            - (_zc_num_items+1) % _zc_num_rows)
                                                                % _zc_num_rows - 1
                                            + (_zc_num_items+1) ),
                               _zc_cur -= _zc_num_rows,
                               _zc_cur >= 0 && _zc_cur < _zc_num_items
                            )) || __zcerror -@ 'ERROR: key-left produced out-of-range item number %u; expecting between 0 and %u\n' $((_zc_cur)) $((_zc_last_item))
                            ;;

        # page-up - top of column, or prev column
        ($__zc_cKeyPU|$'\e[5~')
                            (( _zc_cur -= ( _zc_cur > 0 ),
                               _zc_cur -= _zc_cur % _zc_num_rows )) ;;
        # page-down - bottom of column, or next column
        ($__zc_cKeyPD|$'\e[6~')
                            (( _zc_cur++,
                               _zc_cur += _zc_num_rows-1 - _zc_cur % _zc_num_rows,
                               _zc_cur < _zc_num_items  || ( _zc_cur = _zc_last_item ) )) ;;
        # home - first item
        ($__zc_cKeyHm|$'\e[1~'|$'\e'[\[O]H)
                            (( _zc_cur = 0 )) ;;
        # end - last item
        ($__zc_cKeyEn|$'\e[4~'|$'\e'[\[O]F)
                            (( _zc_cur = _zc_last_item )) ;;

        # backspace
        ($'\x08'|$'\x7f')
                            # shellcheck disable=SC2086
                            ((COMP_POINT)) || break
                            [[ -n ${COMP_WORDS[COMP_CWORD]} ]] || break
                            ((--COMP_POINT)) ; COMP_LINE="${COMP_LINE:0:COMP_POINT-1}${COMP_LINE:COMP_POINT}"
                            printf %s "$__zc_cRestCursor"
                            __zc_gen || break
                            _zc_resize=1 ;;

        # extend current word with printable character
        ([!-~]*)
                            COMP_LINE="${COMP_LINE:0:COMP_POINT}$_zc_key${COMP_LINE:COMP_POINT}"
                            ((++COMP_POINT))
                            COMP_WORDS[COMP_CWORD]+="$_zc_key"
                            printf %s "$__zc_cRestCursor"
                            __zc_gen || break
                            _zc_resize=1 ;;

        esac
        (( _zc_cur <= _zc_last_item || ( _zc_cur = _zc_last_item ),
           _zc_cur >= 0 || ( _zc_cur = 0 ) ))
    done

    if (( _zc_cur >= 0 ))
    then COMPREPLY=( "${COMPREPLY[_zc_cur]}" )
    else COMPREPLY=()
    fi

    ((__zc_MouseTrack)) && {
        # Turn off mouse tracking
        printf %s "$__zc_cStopTrackingMouse"
    }

    printf %s "$__zc_cRestCursor"
    for (( _zcj=0 ; _zcj<_zc_num_rows ; _zcj++ )) do
        printf '\r\n'
        printf %s "$__zc_cClearEoL"
    done

    # reset cursor position
    printf %s "$__zc_cRestCursor"

    __zcdebug zcomp -@0 'Finished completion: COMPREPLY=' \
                    -@ [] "${COMPREPLY[@]}"
}

#
# Define "complete" as a function so that any subsequent completion
# requests will also be wrapped.
#
# If invoked with '-p' or '-r' or '-F __zcwrap_*', or with no args at all, just
# pass through to the builtin.  Otherwise synthesize a wrapper function, and
# call « builtin complete -F » with it.
#
# The -C and -F options have to be handled directly by the wrapper, because
# they aren't usable via compgen. According to the bash man page:
#
#      "When using the -F or -C options, the various shell variables set by the
#       programmable completion facilities, while available, will not have
#       useful values."
#
# Items that should only take effect after _zcomp returns should be handed
# to « builtin complete »:
#       -o filenames
#       -o noquote
#       -o nospace
#       names of commands for which completions are being defined, removed, or
#       queried (including the pseudo-names -D & -E)
#
# Everything else should be deferred to the synthesized wrapper.
#

complete() {
    local -a _zc_orig_args=("$@")
    local -a _zc_genfunc=()
    local -a _zc_gencmd=()
    local -a _zc_compgen_args=()
    local -a _zc_specargs=()
    local -i _zc_passthru=$(($#==0)) _zc_rc

    __zcdebug complete -@0 'START COMPLETE ' \
                       -@ [] "$@"

    while (($#))
    do
        case $1 in
        (-[AGW])
            _zc_compgen_args+=( "$1" "$2" )
            shift
            ;;
        (-C)
            _zc_gencmd=( "$2" )
            shift
            ;;
        (-[DEI])
            _zc_specargs+=( "$1" )
            ;;
        (-F)
            if [[ $2 = __zcwrap_* ]]
            then
                _zc_passthru=1
                break
            fi
            _zc_genfunc=("$2")
            shift
            ;;
        (-[PS])
            _zc_specargs+=( "$1" "$2" )
            shift
            ;;
        (-o)
            case $2 in
            (filenames|noquote|nospace)
                _zc_specargs+=(     "$1" "$2" ) ;;
            (*) _zc_compgen_args+=( "$1" "$2" ) ;;
            esac
            shift
            ;;
        (-[pr])
            _zc_passthru=1
            break
            ;;
        (-[a-z])
            _zc_compgen_args+=( "$1" )
            ;;
        (*)
            break
            ;;
        esac
        shift
    done
    _zc_specargs+=( "$@" )
    set --

    if ((_zc_passthru))
    then
        __zcdebug wrap -@0 'Making pass-thru completion ' \
                       -@ [] complete "${_zc_orig_args[@]}"
        builtin complete "${_zc_orig_args[@]}" || {
            _zc_rc=$?
            __zcerror -@1 'FAILED %x ' $? \
                      -@ [] builtin complete "${_zc_orig_args[@]}"
            return $((_zc_rc))
        }
    else
        # Work around bug in « complete -p » that fails to show the empty arg
        (( ${#_zc_specargs[@]} == 0 && _zc_WrapExistingCompletions )) && _zc_specargs=('')

        local _zc_wrapper="${_zc_genfunc[*]} ${_zc_gencmd[*]} ${_zc_compgen_args[*]}"
        _zc_wrapper="__zcwrap_${_zc_wrapper//[^_0-9a-zA-Z.-]/___}"
        __zcdebug wrap -@0 "Making wrapped completion " \
                       -@ [] complete "${_zc_orig_args[@]}"

        local _zc_wrapdef="$_zc_wrapper() { _zcomp"
        _zc__add() {
            _zc_wrapdef+=" $#"
            if (($#))
            then
                local IFS=' ' _zc_q
                printf -v _zc_q ' %q' "$@"
                _zc_wrapdef+=" $_zc_q"
            fi
        }
        _zc__add "${_zc_genfunc[@]}"
        _zc__add "${_zc_gencmd[@]}"
        _zc__add "${_zc_compgen_args[@]}"
        unset -f _zc__add
        _zc_wrapdef+=' $# "$@" ; }'     # Args passed in by the completion system, becomes _zc_genargs[@]

        __zcdebug wrap -@1 '→define wrapper: %q\n' "$_zc_wrapdef" \
                       -@0 '→' \
                       -@ [] builtin complete -F "$_zc_wrapper" "${_zc_specargs[@]}"
        eval "$_zc_wrapdef" || {
            _zc_rc=$?
            __zcerror -@2 'FAILED %x define wrapper %q' $? "$_zc_wrapper"
            return $((_zc_rc))
        }
        builtin complete -F "$_zc_wrapper" "${_zc_specargs[@]}" || {
            _zc_rc=$?
            __zcerror -@1 'FAILED %x ' $? \
                      -@ [] builtin complete -F "$_zc_wrapper" "${_zc_specargs[@]}"
            return $((_zc_rc))
        }
    fi
}

#
# Install the _zcomp handler over the top of every previously-installed handler.
#
# After defining « complete » as a function, simply re-read the output of
# « complete -p », which is in a format intended to be read in again.
#

(( __zc_HaveReloadedAllCompletions++ )) ||
_zc_WrapExistingCompletions=1 \
. <( builtin complete -p )

for _zc_f in "${_zc_atexit[@]}"
do
    "$_zc_f"
    unset -f "$_zc_f"
done

# unset unwanted variables & functions
unset ${!_zc_*} # work-around for bug in some 4.0 releases
. <( declare -F | sed -n 's/^declare\( -f _zc_\)/unset \1/p' )

return 0

#
# After sending \e[?1003h and before sending \e[?1003l, mouse events are
# presented as \e[M followed by 3 bytes, F+32, X+32, Y+32.
#
# F encodes:
#   (( F & 64 )) indicates use of a scroll wheel, in which case
#       (( F & 1 )) indicates down (otherwise up)
#   otherwise:
#       (( F & 3 )) button down (0, 1 or 2) or button release (3)
#       (( F & 32 )) indicates movement without changing button state
#
# F == 35 (first byte 'C') indicates that the mouse has moved while no button
# was held down, whereas F == 3 (first byte '#') means a button has been
# released, but does not tell you which one. If multiple buttons are held down
# together, each release will be reported as F==3.
#
# X,Y denotes the character cell the mouse cursor is over; 0,0 is top-left.
#

#
# up    \eOA  \e[A
# down  \eOB  \e[B
# right \eOC  \e[C
# left  \eOD  \e[D
#
# home  \e[1~ \e[H
# ins   \e[2~
# del   \e[3~
# end   \e[4~ \e[F
# pgup  \e[5~
# pgdn  \e[6~
#
# menu  \e[29~
#
# The sequences ending in ~ can take a ;n modifier with n between 2 & 8, where:
#   (( n - 1 & 1 )) indicates Shift
#   (( n - 1 & 2 )) indicates Alt
#   (( n - 1 & 4 )) indicates Ctrl
#

# vim: set fenc=utf8 :
