# Sprint Activity — multi-panel (3 rows)
# Panel 1: PRs & Commits per day (dual axis)
# Panel 2: PR cycle time (bars)
# Panel 3: Daily line churn (stacked bar)
#
# Invoke: gnuplot -e "sprint=18" scripts/plot_sprint_metrics.gnuplot

sprint_str = sprintf("%02d", sprint)

set terminal pngcairo size 1200, 1400 font "sans,10"
set output sprintf("assets/images/sprint_activity_sprint_%s.png", sprint_str)
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

# Panel 2: PR cycle time (bar per PR)
set ylabel "Hours"
set y2tics
unset y2label
set format x "%m/%d"
set title "PR Cycle Time (hours from open to merge)"
unset key
infile_cycle = sprintf("build/output/sprint_%s/pr_cycle_times.csv", sprint_str)
plot infile_cycle using 0:4 every ::1 with boxes lc rgb "#E67E22"

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
