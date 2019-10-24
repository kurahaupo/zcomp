#!/module/for/bash

#
# NAMESPACE: parameters and functions with __zc_ prefix need to remain set
# between invocations; ones with _zc_ prefix are used but do not need to
# persist; all other items are localized.
#

################################################################################
#
# Note: this script is specific to Xterm and related VT100-derivative terminals
# It also requires either "tput -S" (that understands "rows" & "lines";
# probably any POSIX compliant version), or "stty size" (which means, probably
# only on GNU's "stty").
#

case $TERM in
(vt???*|ansi*|linux*)   __zc_resizeable=false;; # fixed-size terminal
(*[vwxy]term*|screen*)  __zc_resizeable=true ;; # resizable terminal
(*) return ;;                                   # not supported on unknown terminal types
esac

__zc_cNormal=$'\e[33;40;0m'     # show list in yellow-on-black
__zc_cSelect=$'\e[37;40;1m'     # show highlit item in bright-white-on-black
__zc_cEnd=$'\e[39;49;0m'        # go back to "normal" colours

################################################################################
#
# User-configurable behaviour
#

__zc_ForceCols=0        # set non-zero to override terminal height
__zc_ForceRows=0        # set non-zero to override terminal width
__zc_MaxCols=160        # won't work higher than 223 with mouse-tracking
__zc_MaxRows=24         # arbitrary user choice
__zc_MouseTrack=true    # arbitrary user choice
__zc_PaddingCols=2      # leave gaps between columns
__zc_ReserveCols=2      # don't use rightmost columns in terminal

################################################################################
#
# Check for supported shells (currently only Bash >= 3.0)
#

(( 10#${BASH_VERSION%%.*} < 3 )) && return

#
# Compensate for shortcomings of earlier versions of Bash
#

__zc_read_n1=-N1
__zc_read_t01=-t0.1
__zcd() { printf "%($*)T" -1 ; }

if (( ${BASH_VERSION%%.*} < 4 ))
then
    __zc_read_n1=-n1
    __zc_read_t01=-t1
    # call external "date" command at most once per second, when $SECONDS
    # changes or when format ($*) changes.
    __zcd() {
        (( __zcpsec == SECONDS )) && [[ "$*" = "$__zcpfmt" ]] || {
            (( __zcpsec = SECONDS ))
            __zcpfmt="$*"
            __zcdate=$(date +"$__zcpfmt")
            }
        printf '%s' $__zcdate
    }
fi

################################################################################
#
# debugging output?
#

__zc_dashx=+x
[[ $- = *x* ]] && __zc_dashx=-x #__zc_debug=true

if ${__zc_debug:-false}
then
    exec 4>| $HOME/tmp/_zcomp.log 5>&4 || return
    BASH_XTRACEFD=5
    __zclog() { printf >&4 '%s %s\n' "$(__zcd +%F,%T) [$$]" "$*" ; }
    __zclog "Starting loading, pid=$$ tty=$(tty)"
    __zc_loaderfinish() {
        __zclog "Finished loading, pid=$$ tty=$(tty)"
        BASH_XTRACEFD=
    }
else
    __zclog() { :; }
    __zc_loaderfinish() { :; }
fi

################################################################################

    # __zc__swap is only used internally by __zc_sort (both versions)
    __zc__swap() {
        #__zclog "swapping [$(($1))]=${COMPREPLY[$1]} [$(($2))]=${COMPREPLY[$2]}"
        local temp=${COMPREPLY[$1]}
                     COMPREPLY[$1]=${COMPREPLY[$2]}
                                     COMPREPLY[$2]=$temp
    }

#   # non-recursive quicksort
#
#   __zc_sort() {
#       # eliminate duplicates in COMPREPLY[]
#       __zclog "pre-sort:$(printf '\n %q' "${COMPREPLY[@]}")"
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
#       __zclog "post-sort:$(printf '\n %q' "${COMPREPLY[@]}")"
#   }

    # heapsort (top-down)

    __zc_sort() {
        local i j k n
        #__zclog "pre-sort (#${#COMPREPLY[@]}):$(printf '\n %q' "${COMPREPLY[@]}")"
        for (( i=2, n=${#COMPREPLY[@]} ; i<=n ; ++i )) do
            for (( j=i ; (k=j>>1)>=1 ; j=k )) do
                if   [[ ${COMPREPLY[j-1]} > ${COMPREPLY[k-1]} ]]
                then
                    __zc__swap j-1 k-1
                fi
            done
        done
        #__zclog "heaped (#${#COMPREPLY[@]}):$(printf '\n %q' "${COMPREPLY[@]}")"
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
        #__zclog "sorted (#${#COMPREPLY[@]}):$(printf '\n %q' "${COMPREPLY[@]}")"
    }

    __zc_unique() {
        local i was_ok
        for (( i=${#COMPREPLY[@]}-1, was_ok=1 ; i>=1 ; i-- )) do
            if [[ ${COMPREPLY[i-1]} = ${COMPREPLY[i]} ]]
            then
                #__zclog "merge [$((i-1))] duplicated by [$i]=${COMPREPLY[i]}"
                unset 'COMPREPLY[i]'
                was_ok=0
            fi
        done
        ((was_ok)) && return
        COMPREPLY=("${COMPREPLY[@]}") ;
        #__zclog "unique (#$_zc_num_items):$(printf '\n %q' "${COMPREPLY[@]}")"
    }

    __zc_gen() {
        local IFS=$' \t\n'
        COMPREPLY=()
        $_zc_genfunc
        IFS=$'\n'
        (( ${#_zc_genargs[@]} )) && COMPREPLY=( "${COMPREPLY[@]}" $( compgen "${_zc_genargs[@]}" "${COMP_WORDS[COMP_CWORD]}" ) )
        __zc_sort       # sort COMPREPLY[]
        __zc_unique     # remove dups
        # count of items, used in lots of places
        (( _zc_num_items = ${#COMPREPLY[@]} ))
        # find maximum column width, in _zc_max_item_width
        for (( _zc_max_item_width = 0, _zcj = 0 ; _zcj < _zc_num_items ; ++_zcj )) do
            (( _zc_max_item_width > ${#COMPREPLY[_zcj]} || ( _zc_max_item_width = ${#COMPREPLY[_zcj]} ) ))
        done
        # Bail out if only one option remains
        (( _zc_num_items > 1 )) && _zc_redraw=true _zc_resize=true
    }

    __zc_get_term_size() {
        read -r   LINES            COLUMNS _   < <( stty size     2>/dev/null ) ||
        { read -r LINES && read -r COLUMNS ; } < <( tput -S <<<$'lines\ncols' )
        _zc_resize=true
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

    __zclog "Starting completion: args=($*) COMPREPLY=(${COMPREPLY[*]})"

    local _zc_genfunc=$1
    local -a _zc_genargs=("${@:2}")

    local _zc_button _zc_first _zc_key _zc_redraw _zc_resize _zcJ _zc_xtrap
    local -i _zc_col_offset _zc_col_width _zc_cur _zc_dcol _zc_mcol _zc_mrow
    local -i _zc_num_items _zc_num_dcols _zc_num_rows _zc_num_vcols
    local -i _zc_prev_num_rows _zc_row _zc_saved_row _zc_scrn_cols
    local -i _zc_scrn_rows _zc_max_item_width _zcj _zck _zcl

    : initial COMP_TYPE=$COMP_TYPE COMPREPLY: "${COMPREPLY[@]}"

    # Bail out if not wanting immediate completion
    [[ "${COMP_TYPE:-9}" = 9 ]] || return 0

    _zc_first=true
    (( _zc_col_offset=0, _zc_cur=0, _zc_prev_num_rows=-1 ))

    __zc_gen || { __zclog "Early completion: COUNT=$_zc_num_items COMPREPLY=(${COMPREPLY[*]})" ; return 0 ; }

    _zc_xtrap=$( trap -p SIGINT SIGQUIT )
    trap _zc_key=SIGINT SIGINT
    trap _zc_key=SIGQUIT SIGQUIT

    while
        if $_zc_resize
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
            _zc_resize=false
            _zc_redraw=true
        fi

        (( _zc_row  = _zc_cur%_zc_num_rows,
           _zc_vcol = _zc_cur/_zc_num_rows,
           _zc_dcol = _zc_vcol - _zc_col_offset ))

        if (( _zc_dcol >= _zc_num_dcols ))
        then
            (( _zc_col_offset += _zc_dcol-_zc_num_dcols+1,
               _zc_dcol = _zc_vcol - _zc_col_offset ))
            _zc_redraw=true
        fi
        if (( _zc_dcol < 0 ))
        then
            (( _zc_col_offset += _zc_dcol,
               _zc_dcol = _zc_vcol - _zc_col_offset ))
            _zc_redraw=true
        fi

        if $_zc_redraw
        then
            # save starting cursor position
            $_zc_first && printf $'\e7'

            for (( _zcj = 0 ; _zcj < _zc_num_rows ; _zcj++ )) do
                printf $'\r\n\e[K'
                for (( _zcl = _zcj+_zc_col_offset*_zc_num_rows ; _zcl < _zc_num_items && _zcl < (_zc_num_dcols+_zc_col_offset)*_zc_num_rows ; _zcl += _zc_num_rows )) do
                    printf " %s%-$((_zc_col_width+__zc_PaddingCols-2)).${_zc_col_width}s%s " "$__zc_cNormal" "${COMPREPLY[_zcl]}" "$__zc_cEnd"
                done
            done

            for ((; _zcj < _zc_prev_num_rows ; _zcj++ )) do
                printf $'\r\n\e[K'
            done
            # this can't happen "first time" (because _zc_prev_num_rows is -1)
            if (( _zc_prev_num_rows > _zc_num_rows ))
            then printf "\e[%uA" $((_zc_prev_num_rows-_zc_num_rows))
            fi
            (( _zc_prev_num_rows = _zc_num_rows ))

            $__zc_MouseTrack && $_zc_first && {
                # Ask Xterm to report current cursor position; this will cause a
                # "current position" «CSI?row;colR» response that will be read in the
                # main loop
                printf $'\e[?6n'
                # Turn on mouse tracking
                printf $'\e[?1003h'
            }

            # re-save cursor position after any scrolling
            $_zc_first && printf "\e8\e[%uB\e[%uA\e7" $_zc_num_rows $_zc_num_rows

            _zc_first=false
            _zc_redraw=false
        fi

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
                            _zc_redraw=true _zc_resize=true ;;

        # extend current word with printable character
        ([!-~]*)
                            COMP_LINE="${COMP_LINE:0:COMP_POINT}$_zc_key${COMP_LINE:COMP_POINT}"
                            ((++COMP_POINT))
                            COMP_WORDS[COMP_CWORD]+="$_zc_key"
                            __zc_gen || break
                            _zc_redraw=true _zc_resize=true ;;

        esac
        (( _zc_cur < _zc_num_items || ( _zc_cur = _zc_num_items-1 ) ))
        (( _zc_cur >= 0 || ( _zc_cur = 0 ) ))
    done

    if (( _zc_cur >= 0 ))
    then COMPREPLY=( "${COMPREPLY[_zc_cur]}" )
    else COMPREPLY=()
    fi

    $__zc_MouseTrack && {
        # Turn off mouse tracking
        printf $'\e[?1003l'
    }

    for (( _zcj=0 ; _zcj<_zc_num_rows ; _zcj++ )) do
        printf $'\n\e[2K'
    done

    # reset cursor position
    printf $'\e8'

    # reset signal handlers
    trap - SIGINT SIGQUIT ; eval "$_zc_xtrap"
    __zclog "Finished completion: COMPREPLY=(${COMPREPLY[*]})"
}

#
# And now for the hard part:
# Install the _zcomp handler over the top of every previously-installed handler
#

${__zc_debug:-false} && set +x
shopt -u nullglob  # messes with array variable indexing :-|
while
    IFS=$' \t\n\r' \
    read -r _zc_line <&3
do
    __zclog "got [$_zc_line]"

    [[ $_zc_line = 'complete '* ]] || { __zclog "completion line does not start with 'complete' [$_zc_line]" ; continue ; }
    eval "_zc_cmdline=(${_zc_line#complete })" || { __zclog "unparsable completion line [$_zc_line]" ; continue ; }
    _zc_numargs=${#_zc_cmdline[@]}
    _zc_wrapargs=()
    _zc_genfunc=true
    _zc_gencmd=true
    for (( _zc_argnum=0 ; _zc_argnum<_zc_numargs ; ++_zc_argnum ))
    do
        if [[ ${_zc_cmdline[_zc_argnum]} = -F && ${_zc_cmdline[_zc_argnum+1]} = __zcwrap_* ]]
        then
            __zclog "Skipping '$_zc_line' which is already wrapped with ${_zc_cmdline[_zc_argnum+1]}"
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
            _zc_wrapargs=( "${_zc_wrapargs[@]}" "${_zc_cmdline[@]:_zc_argnum:2}" )
            ((++_zc_argnum))
            ;;
        (-[ACGW])
            _zc_wrapargs=( "${_zc_wrapargs[@]}" "${_zc_cmdline[@]:_zc_argnum:2}" )
            unset '_zc_cmdline[_zc_argnum]' '_zc_cmdline[_zc_argnum+1]'
            ((++_zc_argnum))
            ;;
        (-[a-z])
            _zc_wrapargs=( "${_zc_wrapargs[@]}" "${_zc_cmdline[_zc_argnum]}" )
            unset '_zc_cmdline[_zc_argnum]'
            ;;
        ([^-]*|-[DE])
            ;;
        esac
    done
    _zc_wrapper="$_zc_genfunc ${_zc_wrapargs[*]}"
    _zc_wrapper="__zcwrap_${_zc_wrapper//[^_0-9a-zA-Z.-]/___}"
    ${__zc_debug:-false} && set -x
    eval        "$_zc_wrapper() { _zcomp $( printf " %q" "$_zc_genfunc" "$_zc_gencmd" "${_zc_wrapargs[@]}" ) ; }"  &&
    complete -F "$_zc_wrapper" "${_zc_cmdline[@]}"
    ${__zc_debug:-false} && set +x
done 3< <( complete -p )
${__zc_debug:-false} && set $__zc_dashx

#__zcwrap__E() { _zcomp : -c ; }
#complete -F __zcwrap__E -E

__zc_loaderfinish

return 0

#($'\e[M '??)               ;;  # 0  mouse button 1 down
#($'\e[M!'??)               ;;  # 1  mouse button 2 down
#($'\e[M"'??)               ;;  # 2  mouse button 3 down
#($'\e[M@'??)               ;;  # 32 mouse button 1 drag
#($'\e[MA'??)               ;;  # 33 mouse button 2 drag
#($'\e[MV'??)               ;;  # 54 mouse button 3 drag
#($'\e[M#'??|$'\e[M\x81'??) ;;  # 3,97 mouse movement or button release
