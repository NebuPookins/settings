function claude_week_progress --description 'Show % of Claude Pro weekly usage cycle elapsed (cached)'
    set -l cache /tmp/claude-week-progress-cache
    set -l cache_ttl 300  # seconds (5 min)
    if not test -f $cache
        or test (math (date +%s) - (stat -c %Y $cache)) -gt $cache_ttl
        claude-week-progress --short > $cache
    end
    cat $cache
end
