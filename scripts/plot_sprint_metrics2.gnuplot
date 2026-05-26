# Sprint Progress — multi-panel (2 rows)
# Panel 1: Cumulative stories done
# Panel 2: PR cycle time

sprint_str = sprintf("%02d", sprint)

set terminal pngcairo size 1200, 900 font "sans,10"
set output sprintf("assets/images/sprint_progress_sprint_%s.png", sprint_str)
set datafile separator "\t"
set style fill solid 0.7
set grid ytics lc rgb "#cccccc" lw 1
set border 3
set tics nomirror
set key outside right

set multiplot layout 2, 1 title sprintf("Sprint %d — Progress", sprint)

# Panel 1: Cumulative stories done — use index-based x axis
set ylabel "Stories"
set xlabel "Day"
set title "Cumulative Stories Done"
infile_prog = sprintf("build/output/sprint_%s/sprint_progress.csv", sprint_str)
plot infile_prog using 1:2 every ::1 with linespoints lc rgb "#2C3E50" lw 3 pt 9 title "Done"

# Panel 2: PR cycle time
set ylabel "Hours"
set xlabel "PR"
set title "PR Cycle Time (open → close)"
set style data boxes
set boxwidth 0.7 relative
infile_cycle = sprintf("build/output/sprint_%s/pr_cycle_times.csv", sprint_str)
plot infile_cycle using 0:4 every ::1 xtic(1) lc rgb "#E74C3C" title "Cycle Hrs"

unset multiplot
