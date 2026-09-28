function deepseek_peak_status --description 'Show DeepSeek API peak/off-peak status and time to next change'
    # Effective 00:00 Beijing time on 2026-08-23: peak/off-peak tiers apply
    # Mon-Fri (Beijing time) only; Sat/Sun (Beijing time) are always off-peak.
    # Peak windows (Beijing local time) per DeepSeek API pricing docs: 09:00-12:00 and 14:00-18:00.
    set -l peak_a_start 540
    set -l peak_a_end 720
    set -l peak_b_start 840
    set -l peak_b_end 1080

    set -l now_epoch (date -u +%s)
    set -l beijing_epoch (math "$now_epoch + 28800") # UTC+8, no DST
    set -l beijing_wd (date -u -d @$beijing_epoch +%u) # 1=Mon .. 7=Sun
    set -l beijing_h (date -u -d @$beijing_epoch +%H)
    set -l beijing_m (date -u -d @$beijing_epoch +%M)
    set -l beijing_min (math "$beijing_h * 60 + $beijing_m")

    set -l peak_status
    set -l next_status
    set -l remaining

    if test $beijing_wd -ge 6
        # Weekend in Beijing time: always off-peak until Monday's first peak window.
        set peak_status off-peak
        set next_status peak
        set -l days_to_monday 1
        if test $beijing_wd -eq 6
            set days_to_monday 2
        end
        set remaining (math "$days_to_monday * 1440 + $peak_a_start - $beijing_min")
    else if test $beijing_min -ge $peak_a_start -a $beijing_min -lt $peak_a_end
        set peak_status peak
        set next_status off-peak
        set remaining (math "$peak_a_end - $beijing_min")
    else if test $beijing_min -ge $peak_b_start -a $beijing_min -lt $peak_b_end
        set peak_status peak
        set next_status off-peak
        set remaining (math "$peak_b_end - $beijing_min")
    else
        set peak_status off-peak
        set next_status peak
        if test $beijing_min -lt $peak_a_start
            set remaining (math "$peak_a_start - $beijing_min")
        else if test $beijing_min -ge $peak_a_end -a $beijing_min -lt $peak_b_start
            set remaining (math "$peak_b_start - $beijing_min")
        else if test $beijing_wd -eq 5
            # Friday evening (Beijing time): skip the weekend, next peak is Monday morning.
            set remaining (math "(1440 - $beijing_min) + 2 * 1440 + $peak_a_start")
        else
            set remaining (math "(1440 - $beijing_min) + $peak_a_start")
        end
    end

    set -l hours (math "floor($remaining / 60)")
    set -l mins (math "$remaining % 60")

    set -l status_color green
    if test $peak_status = peak
        set status_color red
    end

    printf '%s%s%s (%s in %dh%02dm)' (set_color $status_color) $peak_status (set_color normal) \
        $next_status $hours $mins
end
