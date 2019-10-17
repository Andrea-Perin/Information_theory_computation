UPCOMPILE FORMAT: ./upcompile.sh <num_week> <num_ex>
This script:
* connects to perina@spiro.fisica.unipd.it/home/perina/information_theory
* deletes the directory ./<num_week>week/ex<num_ex> if it exists, and then recreates;
* uploads the content of the local /home/andrea/Uni/QI/Exercises/<num_week>week/ex<num_ex> directory in the remote one just created;
* compiles all uploaded .f files
