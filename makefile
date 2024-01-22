P1: run_p1
P2: run_p2
P3: run_p3
P4: run_p4
P5: run_p5
P6: run_p6
all: run_p1 run_p2 run_p3 run_p4 run_p5 run_p6

run_p1:
	Rscript code/P1.R
	
run_p2:
	Rscript code/P2.R
	
run_p3:
	Rscript code/P3.R
	
run_p4:
	Rscript code/P4.R
	
run_p5:
	Rscript code/P5.1.R
	Rscript code/P5.2.R
	
run_p6:
	Rscript code/P6.R	
