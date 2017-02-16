set datafile separator ","
set terminal pngcairo  background "#ffffff" enhanced font "arial,8" fontscale 1.0 size 500, 300
set output 'circles-level-6.png'
set title "6 Levels Indra Pearls"
set xrange [-3: 3]
set yrange [-3: 3]
plot "prueba-6.csv" with circles lc rgb "blue" fs transparent solid 0.15 noborder
