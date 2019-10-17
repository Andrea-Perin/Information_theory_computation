#!/bin/bash
# $1 : number of the week
# $2 : number of exercise
upcompile() {
echo 'You are being redirected to perina@spiro.fisica.unipd.it ...'
ssh perina@spiro.fisica.unipd.it << EOF
cd information_theory/$1week
if [ -d "ex$2" ]; then rm -Rf ex$2; fi
mkdir ex$2
exit
EOF
scp $1week/ex$2/* perina@spiro.fisica.unipd.it:/home/perina/information_theory/$1week/ex$2
#ssh perina@spiro.fisica.unipd.it << EOF
#cd information_theory/$1week/ex$2
#for fname in *.f
#do
#echo \$fname
#gfortran \$fname -o \${fname%.*}.x 
#echo "Compiled \$fname"
#done;
#EOF
}
