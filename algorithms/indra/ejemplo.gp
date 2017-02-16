#set terminal pngcairo  background "#ffffff" enhanced font "arial,8" fontscale 1.0 size 500, 300 
#set output 'circles.3.png'
set bar 1.000000 front
set style circle radius graph 0.02, first 0.00000, 0.00000 
set style ellipse size graph 0.05, 0.03, first 0.00000 angle 0 units xy
unset key
set style textbox transparent margins  1.0,  1.0 border
unset logscale
set size ratio -1 1,1
unset paxis 1 tics
unset paxis 2 tics
unset paxis 3 tics
unset paxis 4 tics
unset paxis 5 tics
unset paxis 6 tics
unset paxis 7 tics
set title "Trace of unconstrained optimization with trust-region method" 
set xrange [ -2.50000 : 1.50000 ] noreverse nowriteback
set yrange [ -1.00000 : 2.50000 ] noreverse nowriteback
set paxis 1 range [ * : * ] noreverse nowriteback
set paxis 2 range [ * : * ] noreverse nowriteback
set paxis 3 range [ * : * ] noreverse nowriteback
set paxis 4 range [ * : * ] noreverse nowriteback
set paxis 5 range [ * : * ] noreverse nowriteback
set paxis 6 range [ * : * ] noreverse nowriteback
set paxis 7 range [ * : * ] noreverse nowriteback
set colorbox vertical origin screen 0.9, 0.2, 0 size screen 0.05, 0.6, 0 front  noinvert bdefault
x = 0.0
r = 0.01
types = 6
keyx = -137.0
keyy = -15.0
keyr = 25.0
i = 6
## Last datafile plotted: "prueba-6.png"
plot 'prueba-6.csv' with circles lc rgb "blue" fs transparent solid 0.15 noborder,     'optimize.dat' u 1:2 with linespoints lw 2 lc rgb "black"
