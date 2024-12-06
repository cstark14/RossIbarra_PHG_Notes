#!/bin/bash -l
#SBATCH -D /group/jrigrp11/cstark/
#SBATCH -J phgTwoToThreeAssemblyMAFFTest
#SBATCH -o /group/jrigrp11/cstark/phgSbatchLogs/out_PHGinitialDB.txt
#SBATCH -e /group/jrigrp11/cstark/phgSbatchLogs/error_PHGinitialDB.txt
#SBATCH -t 24:00:00
#SBATCH --partition=high2

module load conda
module load apptainer
conda activate singularity
singularity run -B /group/jrigrp11/cstark/:/mnt --pwd /mnt phg16.simg /tassel-5-standalone/run_pipeline.pl -Xmx100G -debug -configParameters phg/initial_config.txt -MakeInitialPHGDBPipelinePlugin -endPlugin