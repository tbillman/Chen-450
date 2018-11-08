#!/bin/bash
#SBATCH -J Proj4
#SBATCH -o Proj4%j.txt
#SBATCH -t 12:00:00
#SBATCH --mail-user=trb9259@uncw.edu
#SBATCH --mail-type=end
cd $SCRATCH
module load R
R --slave < Billman_Project4-Bridges.R
