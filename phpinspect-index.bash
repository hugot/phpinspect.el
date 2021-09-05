#!/bin/bash
##
# phpinspect-index.bash - Resolve namespaces and fix missing use statements in your PHP
# scripts.
###
# This script is derived from phpns, an earlier project that had a much wider scope than
# just index files for phpinspect.el. Much of the code and command line argument options
# can be removed.
# TODO: remove whatever functionality is not required for phpinspect.el

# shellcheck disable=SC2155
declare CACHE_DIR=./.cache/phpinspect
declare INFO=1

# Cache locations
declare CLASSES="$CACHE_DIR/classes"
declare NAMESPACES="$CACHE_DIR/namespaces"
declare USES="$CACHE_DIR/uses"
declare USES_OWN="$CACHE_DIR/uses_own"
declare USES_LOOKUP="$CACHE_DIR/uses_lookup"
declare USES_LOOKUP_OWN="$CACHE_DIR/uses_lookup_own"
declare FILE_PATHS="$CACHE_DIR/file_paths"
declare NAMESPACE_FILE_PATHS="$CACHE_DIR/namespace_file_paths"
declare INDEXED="$CACHE_DIR/indexed"

[[ $DEBUG -eq 2 ]] && set -x
shopt -s extglob
shopt -so pipefail

read -rd '' USAGE <<'EOF'
    phpns - Resolve namespaces and fix missing use statements in your PHP scripts.
    
    USAGE:
        phpns COMMAND [ ARGUMENTS ] [ OPTIONS ]
    
    COMMANDS:
        i,   index                             Index the PHP project in the current directory
        fu,  find-use             CLASS_NAME   Echo the FQN of a class
        fxu, fix-uses             FILE         Add needed use statements to FILE
        cns, classes-in-namespace NAMESPACE    Echo the classes that reside in NAMESPACE
        fp,  filepath             FQN          Echo the filepath of the class by the name of FQN.

    TO BE IMPLEMENTED:
        rmuu, remove-unneeded-uses FILE: Remove all use statements for classes that are not being used.

    OPTIONS FOR ALL COMMANDS:
        -s --silent     Don't print info.
    
    UNIQUE OPTIONS PER COMMAND:
        index:
            -d, --diff                Show differences between the files in the index and the files in the project directory.
            -N, --new                 Only index new files
        find-use:
            -j, --json                Provide possible use FQN's as a json array.
            -p, --prefer-own          If there are matches inside the "src" dir, only use those.
            -a, --auto-pick           Use first encountered match, don't provide a choice.
            -b. --bare                Print FQN's without any additives.
        fix-uses:
            -j, --json                Provide possible use FQN's per class as a json object with the class names as keys.
            -p, --prefer-own          If there are matches inside the "src" dir, only use those.
            -a, --auto-pick           Use first encountered match, for every class, don't provide a choice.
            -o, --stdout              Print to stdout in stead of printing to the selected file.
        filepath: -

EOF

execute() {
    declare command="$1" INFO="$INFO"
    declare -a CONFIG=()
    shift

    if [[ $command == @(-h|--help|help) ]]; then
        echo "$USAGE" >&2
        exit 0
    fi

    if ! [[ -f ./composer.json ]] && ! [[ -d ./.git ]]; then
        echo "No composer.json or .git file found, not in root of poject, exiting." >&2
        exit 1
    fi

    case "$command" in
        i | index)
            handleArguments index "$@" || return $?

            # The arguments to grep need to be dynamic here, because the diff option
            # requires different arguments to be passed to grep.
            declare -a grep_args=(
                -H
                '^\(class\|abstract[[:blank:]]\+class\|\(final[[:blank:]]\+\|/\*[[:blank:]]*final[[:blank:]]*\*/[[:blank:]]*\)class\|namespace\|interface\|trait\)[[:blank:]]\+[A-Za-z]\+'
                --exclude-dir={.cache,var,bin}
                --binary-files=without-match
            )

            # Only index new files
            if [[ ${CONFIG[$INDEX_NEW]} == '--new' ]]; then
                declare -a new_files=() deleted_files=()
                
                # Extract new files from diff.
                while IFS=':' read -ra diff_file; do
                    if [[ ${diff_file[0]} == '-' ]]; then
                        deleted_files=("${diff_file[1]}" "${deleted_files[@]}")
                    elif [[ ${diff_file[0]} == '+' ]]; then
                        new_files=("${diff_file[1]}" "${new_files[@]}")
                    fi
                done < <(diffIndex)

                # Inform the user if non-existent files were found. Right now the only
                # way to fix this is to reindex entirely.
                if [[ ${#deleted_files[@]} -gt 0 ]]; then
                    info "There are ${#deleted_files[@]} non-existent files in your index. Consider reindexing to prevent incorrect results."
                    info 'Some of these none existent files are:'
                    for i in {0..19}; do
                        [[ $i -ge ${#deleted_files[@]} ]] && break
                        infof '    - "%s"\n' "${deleted_files[$i]}"
                    done
                fi

                if [[ ${#new_files[@]} -eq 0 ]]; then
                    info 'No new files were found.'
                    return 0
                else
                    info "${#new_files[@]} new files found to index."
                fi

                # To exclusively index new files, add the filenames to the arguments array
                grep_args=("${grep_args[@]}" "${new_files[@]}")
            elif [[ ${CONFIG[$INDEX_DIFF]} == '--diff' ]]; then
                diffIndex
                return $?
            else
                grep_args=("${grep_args[@]}" '-r' '--include=*.php')
            fi

            # Index matching files
            grep -m 2 "${grep_args[@]}" | grep -v '^vendor/bin' | fillIndex

            # Add non-matching files to the file with indexed files.
            # This is necessary to be able to diff the index.
            grep -L "${grep_args[@]}" | grep -v '^vendor/bin' >> "$INDEXED"
            ;;
        fu | find-use)
            checkCache
            handleArguments find-use "$@" || return $?
            declare use_path='' class_name="${CONFIG[$CLASS_NAME]}"
            if [[ "$class_name" == @(array|string|float|int|void|mixed) ]]; then
                infof 'Type "%s" is not a class, but a primitive type.\n' "$class_name"
                return 1
            fi

            findUsePathForClass "$class_name"
            ;;
        fxu | fix-uses)
            checkCache
            handleArguments fix-uses "$@" || return $?

            declare file="${CONFIG[$FILE]}"

            if ! [[ -f $file ]]; then
                infof 'File "%s" does not exist or is not a regular file.\n' "$file"

                return 1
            elif [[ ${CONFIG[$STDOUT]} == '--stdout' ]]; then
                fixMissingUseStatements "$file"
            else
                # shellcheck disable=SC2005
                echo "$(fixMissingUseStatements "$file")" > "$file"
            fi
            ;;
        ns | namespace)
            checkCache
            declare file="$1"

            # Try the index, if that doesn't work, attempt to extract the namespace from the file itself.
            if ! grep "(?<=$file:).*" "$NAMESPACE_FILE_PATHS"; then
                grep -Po '(?<=^namespace[[:blank:]])[A-Za-z_\\]+' "$file"
            fi
            ;;
        cns | classes-in-namespace)
            handleArguments classes-in-namespace "$@" || return $?
            checkCache

            declare namespace="${CONFIG[$NAMESPACE]}\\"
            debug "Checking for namespace $namespace"

            awk -F ':' "/:${namespace//\\/\\\\}"'[^\\]+$/{ print $1; }' "$USES_LOOKUP"
            ;;
        fp | filepath)
            handleArguments filepath "$@" || return $?
            checkCache

            grep -Po "^.*(?=:${CONFIG[$CLASS_PATH]//\\/\\\\}$)" "$FILE_PATHS"
            ;;
        *)
            printf 'Command "%s" is not a valid subcommand.\n' "$command" >&2
            exit 1
            ;;
    esac
}

# shellcheck disable=SC2034
fixMissingUseStatements() {
    declare check_uses='false' check_needs='false' file="$1" namespace="$2"
    declare -A uses=() needs=() namespace=()
    declare -a classes=()
    
    classes=($(execute cns "$(execute ns "$file")"))
    for class in "${classes[@]}"; do
        namespace["$class"]='in_namespace'
    done

    findUsesAndNeeds < "$file"
    addUseStatements "${!needs[@]}" < "$file"
}

findUsePathForClass() {
    declare class="$1"
    if [[ ${CONFIG[$PREFER_OWN]} == '--prefer-own' ]]; then
        declare -a possibilities=($(grep -Po "(?<=^${CONFIG[$CLASS_NAME]}:).*" "$USES_LOOKUP_OWN"))
    else
        declare -a possibilities=($(grep -Po "(?<=^${CONFIG[$CLASS_NAME]}:).*" "$USES_LOOKUP"))
    fi

    if [[ ${#possibilities[@]} -eq 1 ]]; then
        use_path="${possibilities[0]}"
        debugf 'Single use path "%s" found' "${possibilities[0]}"

        # Provide an escaped string for json output if requested.
        [[ ${CONFIG[$JSON]} == '--json' ]] && printf -v use_path '"%s"' "${use_path//\\/\\\\}"
    elif [[ ${#possibilities[@]} -eq 0 ]]; then
        _handle_no_use
        return $?
    else
        _handle_multiple_uses
    fi

    infof 'Found use statement for "%s"\n' "$use_path" >&2
    if [[ ${CONFIG[$JSON]} == '--json' ]]; then
        echo '['
        echo "$use_path"
        printf ']'
    elif [[ ${CONFIG[$BARE]} ]]; then
        echo "$use_path"
    else
        echo "use $use_path;"
    fi
}

_handle_no_use() {
    if [[ ${CONFIG[$PREFER_OWN]} == '--prefer-own' ]]; then
        CONFIG[$PREFER_OWN]=
        execute fu "${CONFIG[@]}"
        return $?
    else
        infof 'No match found for class "%s"\n' "$class_name" >&2
        [[ ${CONFIG[$JSON]} == '--json' ]] && printf '[]'
    fi
    return 1
}

_handle_multiple_uses() {
    if [[ ${CONFIG[$AUTO_PICK]} == '--auto-pick' ]]; then
        use_path="${possibilities[0]}"

        return 0
    elif [[ ${CONFIG[$BARE]} == '--bare' ]]; then
        use_path="$(printf '%s\n' "${possibilities[@]}")"

        return 0
    elif [[ ${CONFIG[$JSON]} == '--json' ]]; then
        use_path="$(
        for i in "${!possibilities[@]}"; do
            printf '"%s"' "${possibilities[$i]//\\/\\\\}"
            [[ $i -lt $((${#possibilities[@]}-1)) ]] && printf ','
            echo
        done 
        )"
        
        return 0
    fi

    infof 'Multiple matches for class "%s", please pick one.\n' "$class_name" >&2
    select match in "${possibilities[@]}"; do
        use_path="$match"
        break
    done < /dev/tty
}

addUseStatements() {
    declare -a needs=("$@")
    declare use_statements=''
    if [[ ${CONFIG[$JSON]} == '--json' ]]; then
        declare -i length="$((${#needs[@]}-1))" current=0
        echo '{'
        for needed in "${needs[@]}"; do
            printf '"%s": ' "$needed"
            execute fu --json "$needed" "${CONFIG[$PREFER_OWN]}" "${CONFIG[$AUTO_PICK]}"
            [[ $((current++)) -lt $length ]] && printf ','
            echo
        done
        echo '}'

        return 0
    fi

    while IFS='' read -r line; do
        echo "$line"

        if [[ $line == namespace* ]]; then
            IFS='' read -r line && echo "$line"

            use_statements="$(
            for needed in "${needs[@]}"; do
                execute fu "$needed" "${CONFIG[$PREFER_OWN]}" "${CONFIG[$AUTO_PICK]}"
            done | sort
            )"

            [[ -n $use_statements ]] && echo "$use_statements"
        fi
    done

    declare -i added_uses=0
    added_uses="$(echo -n "$use_statements" | wc -l)"
    [[ -n $use_statements ]] && ((added_uses++))
    info "$added_uses use statements added out of ${#needs[@]} needed types. Types that were needed:" >&2
    infof '           - "%s"\n' "${needs[@]}" >&2
}

debug() {
    if [[ $DEBUG -ge 1 ]]; then
        echo "[DEBUG] => $1" >&2
    fi
}

# shellcheck disable=SC2059
debugf() {
    if [[ $DEBUG -ge 1 ]]; then
        declare format_string="$1"
        shift
        printf "[DEBUG] => $format_string" "$@" >&2
    fi
}

info() {
    if [[ $INFO -eq 1 ]]; then
        echo "[INFO] => $1" >&2
    fi
}

# shellcheck disable=SC2059
infof() {
    if [[ $INFO -eq 1 ]]; then
        declare format_string="$1"
        shift
        printf "[INFO] => $format_string" "$@" >&2
    fi
}

##
# Functions for parameter parsing

# Enum for config
declare -gri CLASS_NAME=0
declare -gri PREFER_OWN=1
declare -gri AUTO_PICK=2
declare -gri STDOUT=3
declare -gri JSON=4
declare -gri BARE=5
declare -gri WORD=6
declare -gri EXPAND_CLASSES=7
declare -gri NO_CLASSES=8
declare -gri NAMESPACE=9
declare -gri CLASS_PATH=10
declare -gri INDEX_DIFF=11 
declare -gri NO_VENDOR=12 # Keep this around as it might be used later on 
declare -gri INDEX_NEW=13
declare -gri FILE=14

handleArguments() {
    declare -p CONFIG &>>/dev/null || return 1
    declare command="$1"
    shift
    case "$command" in
        find-use)
            _handle_find_use_arguments "$@" || return $?
            ;;
        fix-uses)
            _handle_fix_uses_arguments "$@" || return $?
            ;;
        index)
            _handle_index_arguments "$@" || return $?
            ;;
        classes-in-namespace)
            _handle_classes_in_namespace_arguments "$@" || return $?
            ;;
        filepath)
            _handle_filepath_arguments "$@" || return $?
            ;;
        *)
            printf 'handleArguments (line %s): Unknown command "%s" passed.\n' "$(caller)" "$command">&2
            return 1
            ;;
    esac
}

_handle_filepath_arguments() {
    declare arg="$1"
    while shift; do
        case "$arg" in
            -s | --silent)
                INFO=0
                ;;
            --*)
                printf 'Unknown option: "%s"\n' "${arg}" >&2
                return 1
                ;;
            -*)
                if [[ ${#arg} -gt 2 ]]; then
                    
                    declare -i i=1
                    while [[ $i -lt ${#arg} ]]; do
                        _handle_filepath_arguments "-${arg:$i:1}"
                        ((i++))
                    done
                else
                    printf 'Unknown option: "%s"\n' "${arg}" >&2
                    return 1
                fi
                ;;
            '')
                :
                ;;
            *)
                if [[ -n ${CONFIG[$CLASS_PATH]} ]]; then
                    printf 'Unexpected argument: "%s"\n' "$arg" >&2
                    return 1
                fi
                CONFIG[$CLASS_PATH]="$arg"
        esac
        arg="$1"
    done
}

_handle_classes_in_namespace_arguments() {
    declare arg="$1"
    while shift; do
        case "$arg" in
            -s | --silent)
                INFO=0
                ;;
            --*)
                printf 'Unknown option: "%s"\n' "${arg}" >&2
                return 1
                ;;
            -*)
                if [[ ${#arg} -gt 2 ]]; then
                    declare -i i=1
                    while [[ $i -lt ${#arg} ]]; do
                        _handle_classes_in_namespace_arguments "-${arg:$i:1}"
                        ((i++))
                    done
                else
                    printf 'Unknown option: "%s"\n' "${arg}" >&2
                    return 1
                fi
                ;;
            '')
                :
                ;;
            *)
                if [[ -n ${CONFIG[$NAMESPACE]} ]]; then
                    printf 'Unexpected argument: "%s"\n' "$arg" >&2
                    return 1
                fi
                CONFIG[$NAMESPACE]="$arg"
        esac
        arg="$1"
    done
}

_handle_index_arguments() {
    declare arg="$1"
    while shift; do
        case "$arg" in
            -s | --silent)
                INFO=0
                ;;
            -d | --diff)
                CONFIG[$INDEX_DIFF]='--diff'
                ;;
            -N | --new)
                CONFIG[$INDEX_NEW]='--new'
                ;;
            --*)
                printf 'Unknown option: "%s"\n' "${arg}" >&2
                return 1
                ;;
            -*)
                if [[ ${#arg} -gt 2 ]]; then
                    declare -i i=1
                    while [[ $i -lt ${#arg} ]]; do
                        _handle_index_arguments "-${arg:$i:1}"
                        ((i++))
                    done
                else
                    printf 'Unknown option: "%s"\n' "${arg}" >&2
                    return 1
                fi
                ;;
            *)
                printf 'Unexpected argument: "%s"\n' "$arg" >&2
                return 1
                ;;
        esac
        arg="$1"
    done
}

_handle_fix_uses_arguments() {
    declare arg="$1"
    while shift; do
        case "$arg" in
            -s | --silent)
                INFO=0
                ;;
            -p | --prefer-own)
                CONFIG[$PREFER_OWN]='--prefer-own'
                ;;
            -a | --auto-pick)
                CONFIG[$AUTO_PICK]='--auto-pick'
                ;;
            -o | --stdout)
                CONFIG[$STDOUT]='--stdout'
                INFO=0
                ;;
            -j | --json)
                CONFIG[$STDOUT]='--stdout'
                CONFIG[$JSON]='--json'
                ;;
            --*)
                printf 'Unknown option: "%s"\n' "${arg}" >&2
                return 1
                ;;
            -*)
                if [[ ${#arg} -gt 2 ]]; then
                    declare -i i=1
                    while [[ $i -lt ${#arg} ]]; do
                        _handle_fix_uses_arguments "-${arg:$i:1}"
                        ((i++))
                    done
                else
                    printf 'Unknown option: "%s"\n' "${arg}" >&2
                    return 1
                fi
                ;;
            '')
                :
                ;;
            *)
                if [[ -n ${CONFIG[$FILE]} ]]; then
                    printf 'Unexpected argument: "%s"\n' "$arg" >&2
                    return 1
                fi
                CONFIG[$FILE]="$arg"
        esac
        arg="$1"
    done
}

# shellcheck disable=SC2034
_handle_find_use_arguments() {
    declare arg="$1"
    while shift; do
        case "$arg" in
            -s | --silent)
                INFO=0
                ;;
            -b | --bare)
                CONFIG[$BARE]='--bare'
                ;;
            -p | --prefer-own)
                CONFIG[$PREFER_OWN]='--prefer-own'
                ;;
            -a | --auto-pick)
                CONFIG[$AUTO_PICK]='--auto-pick'
                ;; 
            -j | --json) CONFIG[$STDOUT]='--stdout'
                CONFIG[$JSON]='--json'
                INFO=0
                ;;
            --*)
                printf 'Unknown option: "%s"\n' "${arg}" >&2
                return 1
                ;;
            -*)
                if [[ ${#arg} -gt 2 ]]; then
                    declare -i i=1
                    while [[ $i -lt ${#arg} ]]; do
                        _handle_find_use_arguments "-${arg:$i:1}"
                        ((i++))
                    done
                else
                    printf 'Unknown option: "%s"\n' "${arg}" >&2
                    return 1
                fi
                ;;
            '')
                :
                ;;
            *)
                if [[ -n ${CONFIG[$CLASS_NAME]} ]]; then
                    printf 'Unexpected argument: "%s"\n' "$arg" >&2
                    return 1
                fi
                CONFIG[$CLASS_NAME]="$arg"
        esac
        arg="$1"
    done
}

##
# This function outputs the difference between the files that are present in the
# index and the files that are present in the project directory. The output format is:
# +:NEW_FILE        (**Not in index but exists on disk**)
# -:DELETED_FILE    (**In index but does not exist on disk**)
##
diffIndex() {
    diff --unchanged-line-format='' --new-line-format='+:%L' --old-line-format='-:%L' \
        <(sort -u < "$INDEXED" | sed '/^[[:blank:]]*$/d') \
        <(find ./ -name '*.php' -type f | sed 's!^\./\|^./\(var\|.cache\|vendor/bin\)/.\+$!!g; /^[[:blank:]]*$/d' | sort)
}

##
# This function reads the output of a grep command with the option -H or
# --with-filename enabled. The lines containing class and namespace declarations
# will be parsed and added to the index.
#
# shellcheck disable=SC2153
##
fillIndex() {
    [[ -n $CACHE_DIR ]]            || return 1
    [[ -n $CLASSES ]]              || return 1
    [[ -n $NAMESPACES ]]           || return 1
    [[ -n $USES ]]                 || return 1
    [[ -n $USES_LOOKUP ]]          || return 1
    [[ -n $USES_LOOKUP_OWN ]]      || return 1
    [[ -n $FILE_PATHS ]]           || return 1
    [[ -n $NAMESPACE_FILE_PATHS ]] || return 1
    [[ -n $INDEXED ]]              || return 1

    [[ -d $CACHE_DIR ]] || mkdir -p "$CACHE_DIR"

    # Clean up index files if not diffing.
    if [[ ${CONFIG[$INDEX_NEW]} != '--new' ]]; then
        echo > "$NAMESPACES"
        echo > "$CLASSES"
        echo > "$USES"
        echo > "$USES_LOOKUP"
        echo > "$FILE_PATHS"
        echo > "$USES_OWN"
        echo > "$USES_LOOKUP_OWN"
        echo > "$NAMESPACE_FILE_PATHS"
        echo > "$INDEXED"
    fi

    declare -A namespaces=() classes=()
    while IFS=':' read -ra line; do
        declare file="${line[0]}"

        # Save the namespace or class to add to the FQN cache later on.
        if [[ "${line[1]}" =~ (class|trait|interface)[[:blank:]]+([A-Za-z_]+) ]]; then
            classes[$file]="${BASH_REMATCH[2]}"
        elif [[ "${line[1]}" =~ namespace[[:blank:]]+([A-Za-z_\\]+) ]]; then
            namespaces[$file]="${BASH_REMATCH[1]}"
        else 
            debugf 'No class or namespace found in line "%s"' "${line[0]}"
        fi

        # Add filename to file with indexed filenames. This is required
        # for diffing the index.
        echo "$file" >> "$INDEXED"

        if [[ $((++lines%500)) -eq 0 ]]; then
            info "indexed $lines lines."
        fi
    done

    # Fill up the index
    declare -i uses=0
    for file in "${!classes[@]}"; do
        declare namespace="${namespaces[$file]}"
        declare class="${classes[$file]}"

        if [[ -z $class ]]; then
            debugf 'Class is missing for file "%s"\n' "$file"
            debugf 'Namespace: "%s"\n' "$namespace"
            continue
        fi

        ((uses++))
        [[ $((uses%500)) -eq 0 ]] && info "Found FQN's for $uses classes."

        echo "$namespace"                >> "$NAMESPACES"
        echo "$class"                    >> "$CLASSES"
        echo "$namespace\\$class"        >> "$USES"
        echo "$class:$namespace\\$class" >> "$USES_LOOKUP"
        echo "$file:$namespace\\$class"  >> "$FILE_PATHS"
        echo "$file:$namespace"          >> "$NAMESPACE_FILE_PATHS"

        if [[ $file != 'vendor/'* ]]; then
            echo "$namespace\\$class"        >> "$USES_OWN"
            echo "$class:$namespace\\$class" >> "$USES_LOOKUP_OWN"
        fi

    done

    # This keeps the index of class names unique, so that completing class names takes as little
    # time as possible.
    # Use echo and a subshell here to prevent changing the file before the command is done.
    # shellcheck disable=SC2005
    echo "$(sort -u < "$CLASSES")" > "$CLASSES"

    # Ditto for the namespaces index
    # shellcheck disable=SC2005
    echo "$(sort -u < "$NAMESPACES")" > "$NAMESPACES"
    
    info "Finished indexing. Indexed ${lines} lines and found FQN's for $uses classes." >&2
}

checkCache() {
    if ! [[ -d "$CACHE_DIR" ]]; then
        info "No cache dir found, indexing." >&2
        execute index
    fi
}

##
# Find use statements and needed classes in a file.

findUsesAndNeeds() {
    declare -p needs &>>/dev/null || return 1
    declare -p uses &>>/dev/null || return 1
    # shellcheck disable=SC2154
    declare -p namespace &>>/dev/null || return 1

    while read -r line; do
        [[ $line == namespace* ]] && check_uses='true'
        if [[ $line == ?(@(abstract|final) )@(class|interface|trait)* ]]; then
            check_uses='false' 
            check_needs='true'
            
            read -ra line_array <<<"$line"
            set -- "${line_array[@]}"
            while shift && [[ "$1" != @(extends|implements) ]]; do :; done;
            while shift && [[ -n $1 ]]; do 
                [[ $1 == 'implements' ]] && shift
                [[ $1 == \\* ]] || _set_needed_if_not_used "$1"
            done
        fi

        if $check_uses; then
            if [[ $line == use* ]]; then
                declare class_name="${line##*\\}"
                [[ $class_name == *as* ]] && class_name="${class_name##*as }"
                debug "Class name: $class_name"
                class_name="${class_name%%[^a-zA-Z]*}"
                uses["$class_name"]='used'
            fi
        fi

        if $check_needs; then
            if [[ $line == *function*([[:space:]])*([[:alnum:]_])\(* ]]; then
                _check_function_needs "$line"
                continue
            fi
            _check_needs "$line"
        fi
    done
}

_check_function_needs() {
    # Strip everything up until function name and argument declaration.
    declare line="${1#*function}" function_declaration="${1#*function}"

    # Collect the entire argument declaration
    while [[ $line != *'{'* ]] && read -r line; do
        function_declaration="$function_declaration $line"
    done

    declare -a words=()
    read -ra words <<<"$function_declaration"
    for i in "${!words[@]}"; do
        if [[ "${words[$i]}" =~ ^'$'[a-zA-Z_]+ ]]; then
            declare prev_word="${words[$((i-1))]}"
            if [[ $prev_word =~ ^([^\(]*\()?([A-Za-z]+)$ ]]; then
                declare class_name="${BASH_REMATCH[2]}"
                debugf 'Found parameter type "%s" for function "%s"\n' "$class_name" "$function_declaration"
                _set_needed_if_not_used "$class_name"
            fi
        fi
    done
    if [[ "$function_declaration" =~ \):[[:space:]]+([a-zA-Z]+) ]]; then
        declare class_name="${BASH_REMATCH[1]}"
        debugf 'Found return type "%s" for function "%s"\n' "$class_name" "$function_declaration"
        _set_needed_if_not_used "$class_name"
    fi
}

_check_needs() {
    declare line="$1" match=''
    if _line_matches "$line"; then
        declare class_name="${match//[^a-zA-Z]/}"

        debugf 'Extracted type "%s" from line "%s". Entire match: "%s"\n' "$class_name" "$line" "${BASH_REMATCH[0]}"
        _set_needed_if_not_used "$class_name"

        line="${line/"${BASH_REMATCH[0]/}"}"
        _check_needs "$line"
    fi
}

# shellcheck disable=SC2049 
_line_matches() {
    if [[ $line =~ 'new'[[:space:]]+([^\\][A-Za-z]+)\( ]] \
        || [[ $line =~ 'instanceof'[[:space:]]+([A-Za-z]+) ]] \
        || [[ $line =~ catch[[:space:]]*\(([A-Za-z]+) ]] \
        || [[ $line =~ \*[[:blank:]]*@([A-Z][a-zA-Z]*) ]]; then 
        match="${BASH_REMATCH[1]}"
        return $?
    elif [[ $line =~ @(var|param|return|throws)[[:space:]]+([A-Za-z]+) ]] \
        || [[ $line =~ (^|[\(\[\{[:blank:]])([A-Za-z]+)'::' ]]; then
        match="${BASH_REMATCH[2]}"
        return $?
    fi
    return 1
}

_set_needed_if_not_used() {
    declare class_name="$1"
    if [[ -z ${uses["$class_name"]} ]] \
        && [[ -z ${namespace["$class_name"]} ]] \
        && [[ "$class_name" != @(static|self|string|int|float|array|object|bool|mixed|parent|void) ]]; then
        needs["$class_name"]='needed'
    fi
}

execute "$@"
