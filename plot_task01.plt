 set multiplot layout 2,1 rowsfirst
 set label 1 ""
 set title "Newton Interpolation"
 set xlabel "x [U]"
 set ylabel "z_r [U]"
 set xrange [  -6.0000000     :   6.0000000     ]
 plot "data_f_r.txt"
 set label 2 ""
 set title "Cubic Spline"
 set xlabel "y [U]"
 set ylabel "z_s [U]"
 set xrange [  -6.0000000     :   6.0000000     ]
 plot "data_f_s.txt"
 unset multiplot
 pause -1 "Hit return to continue"
