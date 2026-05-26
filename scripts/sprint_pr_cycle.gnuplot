# PR Cycle Time — standalone chart, only run when PR data exists
sprint_str = sprintf("%02d", sprint)

set terminal pngcairo size 800, 400 font "sans,11"
set output sprintf("doc/agile/versions/v0/sprint_%s/pr_cycle.png", sprint_str)
set datafile separator "\t"
set style fill solid 0.7
set grid ytics lc rgb "#cccccc" lw 1
set border 3
set tics nomirror
set key outside right
set xlabel "PR"
set ylabel "Hours"
set title sprintf("Sprint %d — PR Cycle Time (open → close)", sprint)

infile_cycle = sprintf("build/output/sprint_%s/pr_cycle_times.csv", sprint_str)
plot infile_cycle using 0:4 every ::1 with boxes lc rgb "#E74C3C" title "Cycle Hrs"
