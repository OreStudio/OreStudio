# GENERATED FILE — tangled from doc/agile/sprint_charts.org.
# Edit the source block there and re-tangle; do not edit directly.
#
# Line Churn — standalone chart
sprint_str = sprintf("%02d", sprint)
if (!exists("datadir")) datadir = sprintf("build/output/sprint_%s", sprint_str)

set terminal pngcairo size 800, 400 font "sans,11"
set output sprintf("doc/agile/versions/v0/sprint_%s/line_churn.png", sprint_str)
set datafile separator "\t"
set style fill solid 0.7
set grid ytics lc rgb "#cccccc" lw 1
set border 3
set tics nomirror
set key outside right
set xlabel "Day"
set ylabel "Lines"
set title sprintf("Sprint %d — Daily Line Churn", sprint)
set xtics rotate by -45 font "sans,8"

infile_act = datadir."/sprint_activity.csv"
set boxwidth 0.4
plot infile_act using ($0-0.2):4:xtic(1) every ::1 with boxes lc rgb "#27AE60" title "Added", \
     "" using ($0+0.2):5 every ::1 with boxes lc rgb "#E74C3C" title "Deleted"
