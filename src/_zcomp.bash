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
# User-configurable behaviour
#

__zc_DateTicks=1        # invoke "date" no more than once every second or so (before Bash version 4.2)
__zc_ForceCols=0        # set non-zero to override terminal height
__zc_ForceRows=0        # set non-zero to override terminal width
__zc_MaxCols=223        # } limited user preference; note that mouse tracking can't
__zc_MaxRows=223        # } report rows or columns higher than 223=0xff-0x20
__zc_MouseTrack=1       # arbitrary user preference (numeric true/false)
__zc_PaddingCols=2      # leave gaps between columns
__zc_ReserveCols=2      # don't use rightmost columns in terminal, to avoid auto-right-margin causing problems

################################################################################
#
# Note: This script only works with ANSI-style terminals and terminal
# emulators, such as Xterm; it can cope with resizable terminal windows.
#
# It also requires either "tput -S" (and a version that understands "rows" &
# "lines", so probably any POSIX compliant version), or "stty size" (which
# probably means only GNU's "stty").
#

case $TERM in
(ansi*\
|cons*\
|cygwin*\
|linux*\
|rxvt*\
|vt???*\
|wsvt*\
)               __zc_resizeable=0 ;;    # fixed-size terminal
(*[vwxy]term*\
|screen*\
)               __zc_resizeable=1 ;;    # resizable terminal
(*)             return ;;               # not supported on unknown terminal types
esac

__zc_cNormal=$'\e[33;40;0m'     # show list in yellow-on-black
__zc_cSelect=$'\e[37;40;1m'     # show highlit item in bright-white-on-black
__zc_cEnd=$'\e[39;49;0m'        # go back to "normal" colours

################################################################################
#
# stackable at-exit
#

declare -a _zc_atexit=()

################################################################################
#
# Check for supported shells (currently only Bash >= 3.0)
#
# BASH_VERSION can be avoided, because BASH_VERSINFO was added to Bash v2.0
#

(( __zc_BASH_VERSION = BASH_VERSINFO[0] * 1000000 + BASH_VERSINFO[1] * 1000 + BASH_VERSINFO[2] ))
(( 3001000 <= __zc_BASH_VERSION )) || return

#
# Compensate for shortcomings of earlier versions of Bash
#

__zc_has_varredir=1                 # can do {var}>... redirection
__zc_has_xtracefd=1                 # set -o xtrace writes to >&$BASH_XTRACEFD
__zc_read_n1=-N1                    # read -N is understood
__zc_read_t01=-t0.1                 # read -t can understand fractions of second
__zc_ts() { printf "%($*)T" -1 ; }  # timestamp (without trailing newline)

if (( __zc_BASH_VERSION < 4000000 ))
then
    # Note [4.0]
    # Bash v4.0 added support for associative arrays (maps); for numeric
    # variables use ((mapname_$x)) instead of ((mapname[$x])).
    # Bash v4.0 added support for fractional timeouts with « read -t ».
    # For older versions, use -t1 instead, which may cause user-observable
    # delays.
    __zc_has_xtracefd=0
    __zc_read_t01=-t1
fi

if (( __zc_BASH_VERSION < 4001000 ))
then
    # Note [4.1]
    # Bash v4.1 added support for « read -N$num ».
    # For older versions, use « read -n$num » instead, which may cause issues
    # if the tty's eol2 is set to some character that's embedded within a key's
    # escape sequence. (In particular this may apply when eol2 is set to ESC by
    # readline in vi mode.)
    __zc_read_n1=-n1
    # Bash v4.1 added support for « {var}> » redirection.
    # For older versions, use fixed numbers for filedescriptors.
    __zc_has_varredir=0
fi

if (( __zc_BASH_VERSION < 4002000 ))
then
    # Note [4.2]
    # Bash 4.2 added support for printf format specifier « %(...)T ».
    # For older versions, replace __zc_ts with a version that calls the external
    # « date » command instead, but at most once per __zc_DateTicks seconds
    # (based on when SECONDS changes), or whenever format ($*) changes.
    __zc_ts() {
        (( __zcpsec + __zc_DateTicks > SECONDS )) && [[ "$*" = "$__zcpfmt" ]] || {
            (( __zcpsec = SECONDS ))
            __zcpfmt="$*"
            __zcdate=$(date +"$__zcpfmt")
        }
        printf '%s' "$__zcdate"
    }
fi

################################################################################
#
# logging & debugging output
#
# We reserve fd#7 for debug output; by default it's just duped from stderr.
#

exec 7>&2
__zclog() { {
    __zc_ts +%F,%T
    printf ' [%u] ' $$
    local -i argc=$# n_shift=$# newline=1
    local fmt n
    while [[ $1 = '-@'* ]] ; do
        n=${1#-?}
        shift
        case $n in
        @|'')       n_shift=$# ;;
        «)          for ((n_shift=1;n_shift<=$#;++n_shift)) do [[ ${!n_shift} = » ]] && break ; done ; argc=n_shift-1 ;;
        n)          newline=0 ; continue ;;
        *[!0-9]*)   break ;;
        *)          n_shift=n argc=n_shift ;;
        esac

        fmt=$1
        shift

        case $fmt in
        [\[\(\{]?(%?)[\]\)\}])
            pre=${fmt::1} post=${fmt:${#fmt}-1:1} fmt=${fmt:1:${#fmt}-2}
            fmt=${fmt:-\ %q}
            sep=${fmt%%\%*} fmt=${fmt#"$sep"}

            printf "$pre"
            if ((argc>0))
            then
                printf "$fmt" "${@:1:1}"
                ((argc>1)) &&
                printf "$sep$fmt" "${@:2:argc-1}"
            fi
            printf "$post" ;;
        *)
            printf "$fmt" "${@:1:n}" ;;
        esac

        # discard
        shift $(( 0<=n_shift && n_shift<=$# ? n_shift : $# ))
    done
    printf '%s' "$*"
    ((newline)) && printf '\n'
} >&7 ; }

#
# output level:
#   0   silent
#   1   errors
#   2   warnings
#   3   info
#   4   verbose
#   5   debug
#
# debug level/type:
#
#   0   turn off xtrace inside completion (and restore on return; default, WiP)
#   1   don't turn off xtrace inside completion
#   2   turn on xtrace inside completion (and restore on return)
#   4   merge xtrace & zclog output
#
# If bash was invoked with "-x" (meaning, it's on when this file is loaded),
# set the debug level to 2.
#
# If an alternative logfile is given, __zclog output will go there when inside a
# completion function; xtrace output will also go there while inside completion,
# if the shell version supports BASH_XTRACEFD.
#
# Use «__zc_set_debug $level $filename» to change this setting.
#
# Note:  {var}> redirections were added in version 4.1 - avoid them.
#   git blame parse.y | grep REDIR_
#   ⇒ 0001803 2011-11-21 20:51:19 -0500 Bash-4.1 distribution source
#

declare -i __zc_nextmask=0 __zc_mask=0 __zc_loglevel=3
declare -Ai __zc_masknames
__zc_masknames=( [all]='~0' )

__zc_set_debug() {
    local _zc_debug=$1 i j k l
    __zc_debug_file=${2:-}
    (($#>=3)) && __zc_loglevel=$3

    for (( i=4 ; i <= $# ; ++i )) do
        j=${!i} k=${j#[!a-z]} j=${j%"$k"}

        if [[ $k = @(0|[1-9]*([0-9])|0x*([0-9a-f])) ]]
        then l=$((k))
        else l=${__zc_masknames[$k]:=$(( 1 << __zc_nextmask++ ))}
        fi

        case $j in
        +)      (( __zc_mask |=  l )) ;;
        -)      (( __zc_mask &=~ l )) ;;
        =|'')   (( __zc_mask  =  l )) ;;
        esac
    done

    # Default to stderr
    exec 7>&2

    if [[ -n $__zc_debug_file && $__zc_debug_file != - ]]
    then
        # Use a logfile, if specified
        exec 7>> "$__zc_debug_file" || return
    elif (( __zc_has_xtracefd && _zc_debug & 4  ))
    then
        # Merging logging & debugging output (debug&4 is set), and have support
        # for BASH_XTRACEFD
        if [[ -n BASH_XTRACEFD ]]
        then
            # Use BASH_XTRACEFD if it's already set
            exec 7>&$BASH_XTRACEFD
        else
            # If not already set, arrange to use it
            BASH_XTRACEFD=7
        fi
    fi

    (( __zc_xtrace_mode = (_zc_debug & 3 ^ 1)-1 ))  # -1=hide; 0=inherit; +1,+2=show
    (( _zc_debug & 8 && ( __zc_mask = ~0 ) ))    #
    (( _zc_level = _zc_debug >> 4 ))

    __zc_loaderfinish() { :; }

    if (( __zc_has_xtracefd ))
    then
        # Only if BASH_XTRACEFD is enabled, and only if {var}> redirections are enabled
        if ((_zc_debug & 4 && __zc_xtrace_mode > 0))
        then
            # merge xtrace with zclog
            BASH_XTRACEFD=7
        else
            # shouldn't be any output
            BASH_XTRACEFD=
            __zc_mask=0
        fi
    fi

    if ((__zc_xtrace_mode >= 0 || _zc_level > 0 || __zc_mask ))
    then
        __zclog -@ 'Starting loading, pid=%u tty=%s\n' $$ $(tty)
        __zc_loaderfinish() {
            __zclog -@ 'Finished loading, pid=%u tty=%s ex=%#x' $$ "${TTY:=$(tty)}" $?
            BASH_XTRACEFD=
            set $__zc_dashx
        }
    else
        exec 7>/dev/null
        __zc_loaderfinish() { :; }
    fi

    if ((_zc_level > 0)) ; then __zcerror() { __zclog "$@" ; } ; else __zcerror() { : ; }; fi
    if ((_zc_level > 1)) ; then __zcwarn()  { __zclog "$@" ; } ; else __zcwarn()  { : ; }; fi
    if ((_zc_level > 2)) ; then __zcinfo()  { __zclog "$@" ; } ; else __zcinfo()  { : ; }; fi
    if ((_zc_level > 3)) ; then __zctrace() { __zclog "$@" ; } ; else __zctrace() { : ; }; fi
    if ((_zc_level > 4)) ; then __zctrace() { __zclog "$@" ; } ; else __zctrace() { : ; }; fi
    if ((_zc_level > 5)) ; then __zcdebug() { local _zc_m=${__zc_masknames[$1]:=$(( 1 << __zc_nextmask++ ))}
                                              (( __zc_mask & _zc_m )) || return 0
                                              shift
                                              __zclog "$@" ; } ; else __zcdebug() { : ; }; fi

    #
    # Inside each generated wrapper function, redirect stderr (and xtracefd, if
    # different) to fd#7 when calling zcomp, so that xtrace for completion goes
    # with other debugging output.
    #
    __zc_fd_remap='2>&7'
    (( __zc_xtrace_mode > 0 && __zc_has_xtracefd && BASH_XTRACEFD > 2 && BASH_XTRACEFD != 7 )) && __zc_fd_remap+=" $BASH_XTRACEFD>&7"
}

if [[ $- = *x* ]]
then
    __zc_dashx=-x
    __zc_set_debug 2 $HOME/tmp/_zcomp.log +all
else
    __zc_dashx=+x
    __zc_set_debug 0 - -all
fi

################################################################################

    # __zc__swap is only used internally by __zc_sort (both versions)
    __zc__swap() {
        #__zcdebug sortswap "swapping [$(($1))]=${COMPREPLY[$1]} [$(($2))]=${COMPREPLY[$2]}"
        local temp=${COMPREPLY[$1]}
                     COMPREPLY[$1]=${COMPREPLY[$2]}
                                     COMPREPLY[$2]=$temp
    }

#   # non-recursive quicksort
#
#   __zc_sort() {
#       # eliminate duplicates in COMPREPLY[]
#       __zcdebug sortmain -@0 'pre-sort:' -@ '\n %q' "${COMPREPLY[@]}"
#       local -a partitions
#       local first last pv_pt pv_val
#       # sort COMPREPLY[] using a non-recursive quicksort
#       partitions=( -1 ${#COMPREPLY[@]} )
#       for ((; ${#partitions[@]} > 1 ;)) do
#           (( first=${partitions[@]: -2:1}+1,
#              last=${partitions[@]: -1:1}-1 ))
#           if (( last-first <= 1 ))
#           then
#               unset "partitions[${#partitions[@]}-1]"  # pop
#               if (( last>first )) && [[ "${COMPREPLY[first]}" > "${COMPREPLY[last]}" ]]
#               then
#                   # Degenerate case: just two items in the partition; and they're
#                   # out of order, so swap them.
#                   __zc__swap first last
#               fi
#           else
#               # Partition list so that all before the pivot point are less than
#               # the pivot value, and all at-or-after the pivot point are
#               # greater-than-or-equal to the pivot value, so:
#               # (1) scan past duplicates (not implemented yet)
#               # (2) pick a pivot value that isn't the lowest value
#               pv_val=${COMPREPLY[first]}
#               for (( pv_pt=last ; first<pv_pt ;--pv_pt)) do
#                   [[ "$pv_val" = "${COMPREPLY[pv_pt]}" ]] || break
#               done &&
#                   continue    # all identical!!
#               [[ "$pv_val" < "${COMPREPLY[pv_pt]}" ]] &&
#                   pv_val=${COMPREPLY[pv_pt]}
#               for (( pv_pt=last ; first<pv_pt ;)) do
#                   [[ ${COMPREPLY[first]} < $pv_val ]] && { (( ++first )) ; continue ; }
#                   [[ ${COMPREPLY[pv_pt]} < $pv_val ]] || { (( --pv_pt )) ; continue ; }
#                   __zc__swap first pv_pt
#               done
#               partitions[${#partitions[@]}-1]=$pv_pt
#               partitions[${#partitions[@]}]=$(( last+1 )) # push
#           fi
#       done
#       __zcdebug sortmain -@0 'post-sort:' -@ '\n %q' "${COMPREPLY[@]}"
#   }

    # heapsort (top-down)

    __zc_sort() {
        local i j k n
        #__zcdebug sortmain -@1 'pre-sort %u:' ${#COMPREPLY[@]} -@ '\n %q' "${COMPREPLY[@]}"
        for (( i=2, n=${#COMPREPLY[@]} ; i<=n ; ++i )) do
            for (( j=i ; (k=j>>1)>=1 ; j=k )) do
                if   [[ ${COMPREPLY[j-1]} > ${COMPREPLY[k-1]} ]]
                then
                    __zc__swap j-1 k-1
                fi
            done
        done
        #__zcdebug sortmain -@1 'heaped %u:' ${#COMPREPLY[@]} -@ '\n %q' "${COMPREPLY[@]}"
        for (( n=${#COMPREPLY[@]} ; n>0 ;)) do
            __zc__swap 0 n-1
            ((--n))
            for (( j=1 ; (k=j<<1)<=n ; j=k )) do
                if  if  ((k<n)) && [[ ${COMPREPLY[k]} > ${COMPREPLY[j-1]} ]]
                    then
                        [[ ${COMPREPLY[k]} > ${COMPREPLY[k-1]} ]] && ((++k))
                        true
                    else
                        [[ ${COMPREPLY[k-1]} > ${COMPREPLY[j-1]} ]]
                    fi
                then
                    __zc__swap j-1 k-1
                fi
            done
        done
        #__zcdebug sortmain -@1 'sorted  %u:' ${#COMPREPLY[@]} -@ '\n %q' "${COMPREPLY[@]}"
    }

    __zc_unique() {
        #__zcdebug sortuniq -@1 'START unique %u:' "$_zc_num_items" -@ '\n %q' "${COMPREPLY[@]}"
        local i was_ok
        for (( i=${#COMPREPLY[@]}-1, was_ok=1 ; i>=1 ; i-- )) do
            if [[ ${COMPREPLY[i-1]} = ${COMPREPLY[i]} ]]
            then
                #__zcdebug sortuniq -@3 'merge [%u] duplicated by [%u]=%q' $((i-1)) $((i)) "${COMPREPLY[i]}"
                unset 'COMPREPLY[i]'
                was_ok=0
            fi
        done
        ((was_ok)) && return
        COMPREPLY=("${COMPREPLY[@]}") ;
        #__zcdebug sortuniq -@1 'FINISH unique %u:' "$_zc_num_items" -@ '\n %q' "${COMPREPLY[@]}"
    }

    __zc_gen() {
        local IFS=$' \t\n'
        COMPREPLY=()
        $_zc_genfunc
        IFS=$'\n'
        (( ${#_zc_genargs[@]} )) && COMPREPLY+=( $( compgen "${_zc_genargs[@]}" "${COMP_WORDS[COMP_CWORD]}" ) )
        __zc_sort       # sort COMPREPLY[]
        __zc_unique     # remove dups
        # count of items, used in lots of places
        (( _zc_num_items = ${#COMPREPLY[@]} ))
        # find maximum column width, in _zc_max_item_width
        for (( _zc_max_item_width = 0, _zcj = 0 ; _zcj < _zc_num_items ; ++_zcj )) do
            (( _zc_max_item_width > ${#COMPREPLY[_zcj]} || ( _zc_max_item_width = ${#COMPREPLY[_zcj]} ) ))
        done
        # Set return status so that _zcomp bails out if fewer than two options remain
        (( _zc_num_items > 1 )) && _zc_redraw=1 _zc_resize=1
    }

    __zc_get_term_size() {
        read -r   LINES            COLUMNS _   < <( stty size     2>/dev/null ) ||
        { read -r LINES && read -r COLUMNS ; } < <( tput -S <<<$'lines\ncols' )
        _zc_resize=1
    }

    __zc_getkey() {
        local __zgk_chr
        IFS= \
        read -rs -d '' $__zc_read_n1 _zc_key || return $?
        while
            case "$_zc_key" in
            ('')  _zc_key=$'\n' ; break ;;          # compensate for bug in "read"
            ( $'\e[M'??? ) break ;;                 # mouse report
            ( $'\e' | $'\e[' | $'\e['[?O] | $'\e['*[\;0-9] | $'\e[M'* ) ;;
            ( [$'\xc0'-\$'xfe'] \
            | [$'\xe0'-\$'xfe'][$'\x80'-$'\xbf'] \
            | [$'\xf0'-\$'xfe']?[$'\x80'-$'\xbf'] \
            | [$'\xf8'-\$'xfe']??[$'\x80'-$'\xbf'] \
            | [$'\xfc'-\$'xfe']???[$'\x80'-$'\xbf'] \
            | [$'\xfe'-\$'xfe']????[$'\x80'-$'\xbf'] ) ;;  # don't stop on incomplete UTF-8 prefix
            (*) break ;;
            esac
            IFS= \
            read -rs -d '' $__zc_read_n1 $__zc_read_t01 __zgk_chr
        do
            [[ -z "$__zgk_chr" ]] && __zgk_chr=' '  # compensate for bug in "read" ?
            _zc_key="$_zc_key$__zgk_chr"
        done
        return 0
    }

_zcomp() {

    __zcdebug zcomp -@0 'Starting completion: args=' -@$# [] "$@" \
            -@0 ' COMPREPLY=' -@ [] "${COMPREPLY[@]}"

    local _zc_genfunc=$1
    local -a _zc_genargs=("${@:2}")

    local _zc_button _zc_first=1 _zc_key _zc_redraw _zc_resize _zcJ _zc_xtrap
    local -i _zc_col_offset _zc_col_width _zc_cur _zc_dcol _zc_mcol _zc_mrow
    local -i _zc_num_items _zc_num_dcols _zc_num_rows _zc_num_vcols
    local -i _zc_prev_num_rows _zc_row _zc_saved_row _zc_scrn_cols
    local -i _zc_scrn_rows _zc_max_item_width _zcj _zck _zcl

    : initial COMP_TYPE=$COMP_TYPE COMPREPLY: "${COMPREPLY[@]}"

    # Bail out if not wanting immediate completion
    [[ "${COMP_TYPE:-9}" = 9 ]] || return 0

    (( _zc_col_offset=0, _zc_cur=0, _zc_prev_num_rows=-1 ))

    __zc_gen || {
        __zcdebug zcomp -@1 'Early completion: COUNT=%u' "$_zc_num_items" \
                -@0 COMPREPLY= -@ [] "${COMPREPLY[@]}"
        return 0
    }

    _zc_xtrap=$( trap -p SIGINT SIGQUIT )
    trap _zc_key=SIGINT SIGINT
    trap _zc_key=SIGQUIT SIGQUIT

    while
        if ((_zc_resize))
        then
            # get screen dimensions
            (( ( _zc_scrn_cols = __zc_ForceCols ) || ( _zc_scrn_cols = COLUMNS ),
               ( _zc_scrn_rows = __zc_ForceRows ) || ( _zc_scrn_rows = LINES )   ))
            if ! (( _zc_scrn_cols && _zc_scrn_rows ))
            then
                # resort to using tput and/or stty to get values
                trap __zc_get_term_size WINCH   # only call stty again if the window size changes
                __zc_get_term_size &&
                    continue                    # go back and recompute based on $COLUMNS & $LINES
            fi
            (( _zc_scrn_rows <= __zc_MaxRows || ( _zc_scrn_rows = __zc_MaxRows ) ))
            (( _zc_scrn_cols <= __zc_MaxCols || ( _zc_scrn_cols = __zc_MaxCols ) ))
            (( _zc_col_width = _zc_max_item_width,
               _zc_col_width <= _zc_scrn_cols-__zc_ReserveCols  || ( _zc_col_width = _zc_scrn_cols-__zc_ReserveCols ) ))
            (( _zc_num_dcols = (_zc_scrn_cols-__zc_ReserveCols+__zc_PaddingCols) / (_zc_col_width+__zc_PaddingCols),
               _zc_num_dcols < _zc_num_items     || ( _zc_num_dcols = _zc_num_items ),
               _zc_num_dcols > 0                 || ( _zc_num_dcols = 1 ) ))
            (( _zc_num_rows = 1 + (_zc_num_items-1) / _zc_num_dcols,
               _zc_num_rows < _zc_scrn_rows || ( _zc_num_rows = _zc_scrn_rows-1 ) ))
            (( _zc_num_vcols = 1 + (_zc_num_items-1) / _zc_num_rows ))
            _zc_resize=0
            _zc_redraw=1
        fi

        (( _zc_row  = _zc_cur%_zc_num_rows,
           _zc_vcol = _zc_cur/_zc_num_rows,
           _zc_dcol = _zc_vcol - _zc_col_offset ))

        if (( _zc_dcol >= _zc_num_dcols ))
        then
            (( _zc_col_offset += _zc_dcol-_zc_num_dcols+1,
               _zc_dcol = _zc_vcol - _zc_col_offset ))
            _zc_redraw=1
        fi
        if (( _zc_dcol < 0 ))
        then
            (( _zc_col_offset += _zc_dcol,
               _zc_dcol = _zc_vcol - _zc_col_offset ))
            _zc_redraw=1
        fi

        if ((_zc_redraw))
        then
            # save starting cursor position
            ((_zc_first)) && printf '\e7'

            for (( _zcj = 0 ; _zcj < _zc_num_rows ; _zcj++ )) do
                printf '\r\n\e[K'
                for (( _zcl = _zcj+_zc_col_offset*_zc_num_rows ; _zcl < _zc_num_items && _zcl < (_zc_num_dcols+_zc_col_offset)*_zc_num_rows ; _zcl += _zc_num_rows )) do
                    printf " %s%-$((_zc_col_width+__zc_PaddingCols-2)).${_zc_col_width}s%s " "$__zc_cNormal" "${COMPREPLY[_zcl]}" "$__zc_cEnd"
                done
            done

            for ((; _zcj < _zc_prev_num_rows ; _zcj++ )) do
                printf '\r\n\e[K'
            done
            # this can't happen "first time" (because _zc_prev_num_rows is -1)
            if (( _zc_prev_num_rows > _zc_num_rows ))
            then printf "\e[%uA" $((_zc_prev_num_rows-_zc_num_rows))
            fi
            (( _zc_prev_num_rows = _zc_num_rows ))

            ((__zc_MouseTrack && _zc_first)) && {
                # Ask Xterm to report current cursor position; this will cause a
                # "current position" «CSI?row;colR» response that will be read in the
                # main loop
                printf '\e[?6n'
                # Turn on mouse tracking
                printf '\e[?1003h'
            }

            # re-save cursor position after any scrolling:
            #  - go to previous saved cursor position
            #  - move $_zc_num_rows down (which will be truncated if scrolling has occurred)
            #  - move $_zc_num_rows up
            #  - save new cursor position
            ((_zc_first)) && printf "\e8\e[%uB\e[%uA\e7" $_zc_num_rows $_zc_num_rows

            _zc_first=0
            _zc_redraw=0
        fi

        #
        # Highlight currently selected item:
        #
        # restore cursor position, left column, down (_zc_row+1), column
        # (_zc_dcol*(_zc_col_width+2)+1), left (_zc_col_width+1)
        #
        printf "\e8\r\e[%uB\e[%uG %s%-$_zc_col_width.${_zc_col_width}s%s \e[%uD" \
                $(( _zc_row+1 )) \
                $(( _zc_dcol*(_zc_col_width+__zc_PaddingCols)+1 )) \
                "$__zc_cSelect" \
                "${COMPREPLY[_zc_cur]}" \
                "$__zc_cEnd" \
                $(( _zc_col_width+__zc_PaddingCols-1 ))

        __zc_getkey     # returns value in _zc_key
    do
        printf "\e[%uD %s%-$_zc_col_width.${_zc_col_width}s%s \e[%uA\r" 1 "$__zc_cNormal" "${COMPREPLY[_zc_cur]}" "$__zc_cEnd" $(( _zc_row+1 ))

        # Terminals don't usually produce the ;1~ variant, but just make sure
        _zc_key=${_zc_key/';1~'/'~'}

        case "$_zc_key" in

        ## Capture answer to initial "report cursor position" request
        ($'\e['[?0-9]*\;*R) _zcJ=${_zc_key//[^;0-9]/} ; _zc_saved_row=${_zcJ%%\;*} ;; #_zc_saved_col=${_zcJ#*\;} ;;

        ## Enter / Escape / SIGINT / SIGQUIT
        (' '|$'\r'|$'\n')   break ;;
        ($'\e'|SIG*) _zc_cur=-1 ; break ;;

        ## Mouse tracking
        ($'\e[M`'*)         (( _zc_col_offset > 0                             && --_zc_col_offset )) ;;  # 64 mouse scroll up
        ($'\e[Ma'*)         (( _zc_col_offset < _zc_num_vcols-_zc_num_dcols-1 && ++_zc_col_offset )) ;;  # 65 mouse scroll down
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
                               _zcj + _zc_num_rows*_zck < _zc_num_items &&
                             ( _zc_cur = _zcj + _zc_num_rows*_zck ) ))
                            :
                            [[ "$_zc_button" = ' ' ]] && break
                            ;;

        # shift-up - top of column
        ($'\e[1;2A')        (( _zc_cur -= _zc_cur % _zc_num_rows )) ;;
        # shift-down - bottom of column
        ($'\e[1;2B')        (( _zc_cur += _zc_num_rows-1 - _zc_cur % _zc_num_rows,
                               _zc_cur < _zc_num_items         || ( _zc_cur = _zc_num_items-1 ) )) ;;
        # shift-right - right of row
        ($'\e[1;2C')        (( _zc_cur += (_zc_cur / _zc_num_rows - 1 - (_zc_cur % _zc_num_rows > (_zc_num_items-1) % _zc_num_rows)) * _zc_num_rows )) ;;
        # shift-left - left of row
        ($'\e[1;2D')        (( _zc_cur %= _zc_num_rows )) ;;

        # up, shift-tab
        ($'\e[A'|$'\e[Z')   (( _zc_cur--,
                               _zc_cur >= 0                  || ( _zc_cur = _zc_num_items-1 ) )) ;;
        # down, tab
        ($'\e[B'|$'\t')     (( _zc_cur++,
                               _zc_cur < _zc_num_items         || ( _zc_cur = 0 ) )) ;;
        # right
        ($'\e[C')           (( _zc_cur += _zc_num_rows,
                               _zc_cur < _zc_num_items || ( _zc_cur = (_zc_cur+1) % _zc_num_rows ) )) ;;
        # left
        ($'\e[D')           (( _zc_cur -= _zc_num_rows,
                               _zc_cur >= 0          || ( _zc_cur += _zc_num_rows*_zc_num_vcols-1,
                                                          _zc_cur < _zc_num_items || ( _zc_cur = _zc_num_items - 1 ) ) )) ;;
        # home
        ($'\e[H')           (( _zc_cur = 0 )) ;;
        # end
        ($'\e[F')           (( _zc_cur = _zc_num_items-1 )) ;;

        # backspace
        ($'\x08'|$'\x7f')   ((COMP_POINT)) || break
                            [[ -n ${COMP_WORDS[COMP_CWORD]} ]] || break
                            ((--COMP_POINT)) ; COMP_LINE="${COMP_LINE:0:COMP_POINT-1}${COMP_LINE:COMP_POINT}"
                            __zc_gen || break
                            _zc_redraw=1 _zc_resize=1 ;;

        # extend current word with printable character
        ([!-~]*)
                            COMP_LINE="${COMP_LINE:0:COMP_POINT}$_zc_key${COMP_LINE:COMP_POINT}"
                            ((++COMP_POINT))
                            COMP_WORDS[COMP_CWORD]+="$_zc_key"
                            __zc_gen || break
                            _zc_redraw=1 _zc_resize=1 ;;

        esac
        (( _zc_cur < _zc_num_items || ( _zc_cur = _zc_num_items-1 ) ))
        (( _zc_cur >= 0 || ( _zc_cur = 0 ) ))
    done

    if (( _zc_cur >= 0 ))
    then COMPREPLY=( "${COMPREPLY[_zc_cur]}" )
    else COMPREPLY=()
    fi

    ((__zc_MouseTrack)) && {
        # Turn off mouse tracking
        printf '\e[?1003l'
    }

    for (( _zcj=0 ; _zcj<_zc_num_rows ; _zcj++ )) do
        printf '\n\e[2K'
    done

    # reset cursor position
    printf '\e8'

    # reset signal handlers
    trap - SIGINT SIGQUIT ; eval "$_zc_xtrap"
    __zcdebug zcomp -@0 'Finished completion: COMPREPLY=' -@ [] "${COMPREPLY[@]}"
}

#
# And now for the hard part:
# Install the _zcomp handler over the top of every previously-installed handler
#

((__zc_debug)) && set +x
shopt -u nullglob  # messes with array variable indexing :-|
while
    IFS= \
    read -r _zc_line <&3
do
    __zcdebug zcomp -@1 'got [%q]' "$_zc_line"

    [[ $_zc_line = 'complete '* ]] || {
        __zcdebug zcomp -@1 "completion line does not start with 'complete' [%q]" "$_zc_line"
        continue
        }
    eval "_zc_cmdline=(${_zc_line#complete })" || {
        __zcdebug zcomp -@1 'unparsable completion line [%q]' "$_zc_line"
        continue
    }
    _zc_numargs=${#_zc_cmdline[@]}
    _zc_wrapargs=()
    _zc_genfunc=true
    _zc_gencmd=true
    for (( _zc_argnum=0 ; _zc_argnum<_zc_numargs ; ++_zc_argnum ))
    do
        if [[ ${_zc_cmdline[_zc_argnum]} = -F && ${_zc_cmdline[_zc_argnum+1]} = __zcwrap_* ]]
        then
            __zcdebug zcomp -@2 "Skipping '%q' which is already wrapped with %q" "$_zc_line" "${_zc_cmdline[_zc_argnum+1]}"
            continue 2
        fi
        case ${_zc_cmdline[_zc_argnum]} in
        (-[PS]) ((++_zc_argnum)) ;;
        (-F)
            _zc_genfunc=${_zc_cmdline[_zc_argnum+1]:?'Missing arg for -F'}
            unset '_zc_cmdline[_zc_argnum]' '_zc_cmdline[_zc_argnum+1]'
            ((++_zc_argnum))
            ;;
        (-C)
            _zc_gencmd=${_zc_cmdline[_zc_argnum+1]:?'Missing arg for -C'}
            unset '_zc_cmdline[_zc_argnum]' '_zc_cmdline[_zc_argnum+1]'
            ((++_zc_argnum))
            ;;
        (-[o])
            # need "-o OPT" in both cmdline and genargs
            _zc_wrapargs+=( "${_zc_cmdline[@]:_zc_argnum:2}" )
            ((++_zc_argnum))
            ;;
        (-[ACGW])
            _zc_wrapargs+=( "${_zc_cmdline[@]:_zc_argnum:2}" )
            unset '_zc_cmdline[_zc_argnum]' '_zc_cmdline[_zc_argnum+1]'
            ((++_zc_argnum))
            ;;
        (-[a-z])
            _zc_wrapargs+=( "${_zc_cmdline[_zc_argnum]}" )
            unset '_zc_cmdline[_zc_argnum]'
            ;;
        ([^-]*|-[DE])
            ;;
        esac
    done
    _zc_wrapper="$_zc_genfunc ${_zc_wrapargs[*]}"
    _zc_wrapper="__zcwrap_${_zc_wrapper//[^_0-9a-zA-Z.-]/___}"
    ((__zc_debug)) && set -x
    eval        "$_zc_wrapper() { _zcomp $( printf " %q" "$_zc_genfunc" "$_zc_gencmd" "${_zc_wrapargs[@]}" ) ; }"  &&
    complete -F "$_zc_wrapper" "${_zc_cmdline[@]}"
    ((__zc_debug)) && set +x
done 3< <( complete -p )
((__zc_debug)) && set $__zc_dashx

#__zcwrap__E() { _zcomp : -c ; }
#complete -F __zcwrap__E -E

__zc_loaderfinish

for _zc_f in "${_zc_atexit[@]}"
do
    "$_zc_f"
    unset -f "$_zc_f"
done

return 0

#($'\e[M '??)               ;;  # 0  mouse button 1 down
#($'\e[M!'??)               ;;  # 1  mouse button 2 down
#($'\e[M"'??)               ;;  # 2  mouse button 3 down
#($'\e[M@'??)               ;;  # 32 mouse button 1 drag
#($'\e[MA'??)               ;;  # 33 mouse button 2 drag
#($'\e[MV'??)               ;;  # 54 mouse button 3 drag
#($'\e[M#'??|$'\e[M\x81'??) ;;  # 3,97 mouse movement or button release

#
# home  \e[1~
# ins   \e[2~
# del   \e[3~
# end   \e[4~
# pgup  \e[5~
# pgdn  \e[6~
#
# ;n modifier has n = 1 plus the sum of
#   1 for shift
#   2 for alt
#   4 for ctrl
# so
#  ;1   plain (always omitted, but defined with value 1 to follow the general rule about an omitted values always defaulting to 1)
#  ;2   shift
#  ;3   ctrl
#  ;4   ctrl+shift
#  ;5   alt
#  ;6   shift+alt
#  ;7   ctrl+alt
#  ;8   ctrl+shift+alt
#

# vim: set fenc=utf8 :
