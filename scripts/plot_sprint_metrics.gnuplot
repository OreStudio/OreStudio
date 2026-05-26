# Sprint Activity — multi-panel (3 rows)
# Panel 1: PRs & Commits per day (dual axis)
# Panel 2: Commits per PR (scatter)
# Panel 3: Daily line churn (stacked bar)
#
# Invoke: gnuplot -e "sprint=18" scripts/plot_sprint_metrics.gnuplot

sprint_str = sprintf("%02d", sprint)

set terminal pngcairo size 1200, 1400 font "sans,10"
set output sprintf("assets/images/sprint_%s/sprint_activity.png", sprint_str)
set datafile separator "\t"
set style fill solid 0.7
set grid ytics lc rgb "#cccccc" lw 1
set border 3
set tics nomirror
set key outside right

set multiplot layout 3, 1 title sprintf("Sprint %d — Activity", sprint)

# Panel 1: PRs & Commits
set ylabel "PRs"
set y2label "Commits"
set ytics nomirror
set y2tics
set format x ""
set title "PRs & Commits per Day"
infile_act = sprintf("build/output/sprint_%s/sprint_activity.csv", sprint_str)
plot infile_act using 2 every ::1 title "PRs" with boxes lc rgb "#4A90D9", \
     "" using 3 every ::1 axes x1y2 title "Commits" with linespoints lc rgb "#E67E22" lw 2 pt 7

# Panel 2: PR Cycle Time (one bar per merged PR)
set ylabel "Hours"
set y2tics
unset y2label
set format x "%m/%d"
set title "PR Cycle Time (hours from open to merge)"
unset key
infile_cycle = sprintf("build/output/sprint_%s/pr_cycle_times.csv", sprint_str)
stats infile_cycle using 4 every ::1 nooutput
if (STATS_records > 0) {
    set format x ""
    plot infile_cycle using 0:4 every ::1 with boxes lc rgb "#E67E22"
    set format x "%m/%d"
} else {
    set format x ""
    plot 0 title "No merged PRs this sprint" with lines lc rgb "#999999"
    set format x "%m/%d"
}

# Panel 3: Line churn
set ylabel "Lines"
set format x "%m/%d"
set title "Daily Line Churn"
set style data histograms
set style histogram rowstacked
set boxwidth 0.8 relative
plot infile_act using 5 every ::1 title "Added" lc rgb "#27AE60", \
     "" using 6 every ::1 title "Deleted" lc rgb "#E74C3C"

unset multiplot


# Sprint Progress — multi-panel (2 rows)
# Panel 1: Cumulative stories done
# Panel 2: PR cycle time

set terminal pngcairo size 1200, 900 font "sans,10"
set output sprintf("assets/images/sprint_%s/sprint_progress.png", sprint_str)
set datafile separator "\t"
set style fill solid 0.7
set grid ytics lc rgb "#cccccc" lw 1
set border 3
set tics nomirror
set key outside right

set multiplot layout 2, 1 title sprintf("Sprint %d — Progress", sprint)

# Panel 1: Cumulative stories done
set ylabel "Stories"
set format x ""
set title "Cumulative Stories Done"
infile_prog = sprintf("build/output/sprint_%s/sprint_progress.csv", sprint_str)
stats infile_prog using 2 every ::1 nooutput
if (STATS_records > 0) {
    plot infile_prog using 2 every ::1 with linespoints lc rgb "#2C3E50" lw 3 pt 9 title "Done"
} else {
    set format x ""
    plot 0 title "No story transitions" with lines lc rgb "#999999"
    set format x "%m/%d"
}

# Panel 2: PR cycle time
set ylabel "Hours"
set format x "%m/%d"
set title "PR Cycle Time (open → close)"
set style data boxes
set boxwidth 0.7 relative
set xtics rotate by -45
stats infile_cycle every ::1 nooutput
if (STATS_records > 0) {
    set format x ""
    plot infile_cycle using 0:4 every ::1:xtic(2) lc rgb "#E74C3C" title "Cycle Hrs"
    set format x "%m/%d"
} else {
    set format x ""
    plot 0 title "No merged PRs" with lines lc rgb "#999999"
    set format x "%m/%d"
}

unset multiplot
