#!/bin/bash
#SBATCH -A hpc2n2025-093
#SBATCH -J test
#SBATCH -n 5
#SBATCH -t 10:00:00
#SBATCH --output=slurm-%j.out

#ml GCC/12.3.0 OpenMPI/4.1.5
# Moduler

Rscript rf_model.R

echo "done"

exit 0


#sbatch slurm.sh
