# Cumulative Stories Done — standalone chart, only run when story data exists
sprint_str = sprintf("%02d", sprint)

set terminal pngcairo size 800, 400 font "sans,11"
set output sprintf("assets/images/sprint_stories_done_sprint_%s.png", sprint_str)
set datafile separator "\t"
set style fill solid 0.7
set grid ytics lc rgb "#cccccc" lw 1
set border 3
set tics nomirror
set key outside right
set xlabel "Day"
set ylabel "Stories"
set title sprintf("Sprint %d — Cumulative Stories Done", sprint)

infile_prog = sprintf("build/output/sprint_%s/sprint_progress.csv", sprint_str)
plot infile_prog using 0:2 every ::1 with linespoints lc rgb "#2C3E50" lw 3 pt 9 title "Done"
