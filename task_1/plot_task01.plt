 set multiplot layout 2,1 rowsfirst
 set label 1 ""
 set title "Newton Interpolation"
 set xlabel "x [U]"
 set ylabel "z_r [U]"
 set style line 1 lt rgb "#DC143C" pt 7
 set xrange [  -6.0000000     :   6.0000000     ]
 plot "task_1/data_f_r.txt" with line lt rgb "#00008b" title "Newton Interpolation ", "task_1/x_points.txt" using 1:2:(sprintf("(%g,%g)",$1,$2)) with labels offset 1,1 font ",7" point ls 1 notitle
 set label 2 ""
 set title "Cubic Spline"
 set xlabel "y [U]"
 set ylabel "z_s [U]"
 set style line 2 lt rgb "#DC143C" pt 7
 set xrange [  -6.0000000     :   6.0000000     ]
 plot "task_1/data_f_s.txt" with lines lt rgb "#00008b" title "Cubic Spline ", "task_1/y_points.txt" using 1:2:(sprintf("(%g,%g)",$1,$2)) with labels offset 1,1 font ",7" point ls 2 notitle
 unset multiplot
 pause -1 "Hit return to continue"
