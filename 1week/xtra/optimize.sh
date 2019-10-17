#!/bin/bash
# allows to compile either by optimizing or not
# one can choose three possible levels
ARG1=${1-foo}
ARG2=${2-bar}
options=("1","2","3")
if [[ "$ARG1" = "-o" ]]
then
	if [[ "$ARG2" = "1" ]]||[[ "$ARG2" = "2" ]]||[[ "$2" = "3" ]]
	then
		mv ex3.f ex3_opt.f
		gfortran ex3_opt.f -O$ARG2 -o ex3_opt.x
		python autocontrol.py
		mv performance.pdf performance_opt_$ARG2.pdf
		mv ex3_opt.f ex3.f
	else
		echo "Optimization option not recognized."
	fi
else
	gfortran ex3.f -o ex3.x
	python autocontrol.py
fi
