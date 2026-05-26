# Sprint Activity — multi-panel (3 rows)
# Panel 1: PRs & Commits per day (dual axis)
# Panel 2: Commits per PR (scatter)
# Panel 3: Daily line churn (stacked bar)

set terminal pngcairo size 1200, 1400 font "sans,10"
set output "assets/images/sprint_18/sprint_activity.png"
set datafile separator "\t"
set style fill solid 0.7
set grid ytics lc rgb "#cccccc" lw 1
set border 3
set tics nomirror
set key outside right

set multiplot layout 3, 1 title "Sprint Activity"

# Panel 1: PRs & Commits
set ylabel "PRs"
set y2label "Commits"
set ytics nomirror
set y2tics
set format x ""
set title "PRs & Commits per Day"
plot "build/output/sprint_18/sprint_activity.csv" using 2 every ::1 title "PRs" with boxes lc rgb "#4A90D9", \
     "" using 3 every ::1 axes x1y2 title "Commits" with linespoints lc rgb "#E67E22" lw 2 pt 7

# Panel 2: Commits per PR
set ylabel "Commits"
set y2tics
unset y2label
set format x "%m/%d"
set title "Commits per PR (one point per merged PR)"
unset key
plot "build/output/sprint_18/pr_cycle_times.csv" using 5 every ::1 with points pt 7 lc rgb "#27AE60"

# Panel 3: Line churn
set ylabel "Lines"
set format x "%m/%d"
set title "Daily Line Churn"
set style data histograms
set style histogram rowstacked
set boxwidth 0.8 relative
plot "build/output/sprint_18/sprint_activity.csv" using 5 every ::1 title "Added" lc rgb "#27AE60", \
     "" using 6 every ::1 title "Deleted" lc rgb "#E74C3C"

unset multiplot


# Sprint Progress — multi-panel (2 rows)
# Panel 1: Cumulative stories done
# Panel 2: PR cycle time

set terminal pngcairo size 1200, 900 font "sans,10"
set output "assets/images/sprint_18/sprint_progress.png"
set datafile separator "\t"
set style fill solid 0.7
set grid ytics lc rgb "#cccccc" lw 1
set border 3
set tics nomirror
set key outside right

set multiplot layout 2, 1 title "Sprint Progress"

# Panel 1: Cumulative stories done
set ylabel "Stories"
set format x ""
set title "Cumulative Stories Done"
plot "build/output/sprint_18/sprint_progress.csv" using 2 every ::1 with linespoints lc rgb "#2C3E50" lw 3 pt 9 title "Done"

# Panel 2: PR cycle time
set ylabel "Hours"
set format x "%m/%d"
set title "PR Cycle Time (open → merge)"
set style data boxes
set boxwidth 0.7 relative
set xtics rotate by -45
plot "build/output/sprint_18/pr_cycle_times.csv" using 4 every ::1:xtic(1) lc rgb "#E74C3C" title "Cycle Hrs"

unset multiplot
