# Echoes the first non-empty argument (like Bash's ${var:-fallback})
function default
    for arg in $argv
        if test -n "$arg"
            echo $arg
            return
        end
    end
end

# Exports a shell variable
function export
    set -gx $argv[1] $argv[2..-1]
end
