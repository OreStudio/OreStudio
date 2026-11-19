# GENERATED FILE — tangled from doc/agile/sprint_charts.org.
# Edit the source block there and re-tangle; do not edit directly.
#
# PR Cycle Time — standalone chart, only run when PR data exists
sprint_str = sprintf("%02d", sprint)
if (!exists("datadir")) datadir = sprintf("build/output/sprint_%s", sprint_str)

set terminal pngcairo size 800, 440 font "sans,10"
set output sprintf("doc/agile/versions/v0/sprint_%s/pr_cycle.png", sprint_str)
set datafile separator "\t"
set grid ytics lc rgb "#cccccc" lw 1
set border 3
set tics nomirror
set key below horizontal
set ylabel "Open to merge (log scale)"
infile_cycle = datadir."/pr_cycle_times.csv"
minutes(h) = h * 60 < 0.05 ? 0.05 : h * 60
stats infile_cycle using (minutes($4)) every ::1 nooutput
set logscale y
set yrange [0.04:*]
set ytics ("3 s" 0.05, "30 s" 0.5, "5 min" 5, "1 h" 60, "10 h" 600, "1 d" 1440, "10 d" 14400)
set xdata time
set timefmt "%Y-%m-%dT%H:%M"
set format x "%m-%d"
set xtics 86400 rotate by -45 font "sans,8"
set title sprintf("Sprint %d — PR Cycle Time, %d PRs", sprint, STATS_records)
plot infile_cycle using 3:(minutes($4)) every ::1 with points pt 7 ps 0.5 lc rgb "#60E74C3C" title "PR", \
     STATS_median with lines lw 2 lc rgb "#2C3E50" title sprintf("Median %.1f min", STATS_median), \
     STATS_up_quartile with lines lw 2 dt 2 lc rgb "#2C3E50" title sprintf("Upper quartile %.0f min", STATS_up_quartile)
