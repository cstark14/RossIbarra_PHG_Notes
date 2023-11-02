#!/bin/bash -l
#SBATCH -D /group/jrigrp11/cstark/
#SBATCH -J addExistingHaplo
#SBATCH -o /group/jrigrp11/cstark/phgSbatchLogs/out_addExistingHaplo.txt
#SBATCH -e /group/jrigrp11/cstark/phgSbatchLogs/error_addExistingHaplo.txt
#SBATCH -t 24:00:00
#SBATCH --partition=bigmemm
#SBATCH --mem=524000 
#SBATCH --mincpus=16
# mail alerts at beginning and end of job
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END
# send mail here
#SBATCH --mail-user=crstark@ucdavis.edu

module load conda
module load apptainer
conda activate singularity
singularity exec -B /group/jrigrp11/cstark/:/mnt --pwd /mnt phg16.simg phg/CreateHaplotypesFromGVCF.groovy -config phg/gvcf_config.txt
