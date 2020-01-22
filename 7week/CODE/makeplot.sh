gnuplot -e "outname='$1'" -e "inname='$2'" -e "numframes='$3'" -e "xl='$4'" -e "xu='$5'" -e "every='$6'" plotpalette.gnu
xdg-open $1 
