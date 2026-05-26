# PRs & Commits per Day — standalone chart
sprint_str = sprintf("%02d", sprint)

set terminal pngcairo size 800, 400 font "sans,11"
set output sprintf("assets/images/sprint_prs_commits_sprint_%s.png", sprint_str)
set datafile separator "\t"
set style fill solid 0.7
set grid ytics lc rgb "#cccccc" lw 1
set border 3
set tics nomirror
set key outside right
set xlabel "Day"
set ylabel "PRs"
set y2label "Commits"
set ytics nomirror
set y2tics
set title sprintf("Sprint %d — PRs & Commits per Day", sprint)

infile_act = sprintf("build/output/sprint_%s/sprint_activity.csv", sprint_str)
plot infile_act using 2 every ::1 title "PRs" with boxes lc rgb "#4A90D9", \
     "" using 3 every ::1 axes x1y2 title "Commits" with linespoints lc rgb "#E67E22" lw 2 pt 7
