#!/bin/bash -l
#SBATCH -D /home/vale878/
#SBATCH -J phgTwoToThreeAssemblyMAFFTest
#SBATCH -o /home/vale878/phgSbatchLogs/out-%A_PHGanchorwave.txt
#SBATCH -e /home/vale878/phgSbatchLogs/error-%A_PHGanchorwave.txt
#SBATCH -t 24:00:00
#SBATCH --partition=bigmemm

module load apptainer
apptainer run phg40.simg /tassel-5-standalone/run_pipeline.pl -configParameters phg/maff_from_anchorwave_config.txt -AssemblyMAFFromAnchorWavePlugin -endPlugin