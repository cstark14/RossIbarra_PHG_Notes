#!/bin/bash -l
#SBATCH -D /home/vale878/
#SBATCH -J debuggingMinimapKill
#SBATCH -o /home/vale878/out-%A_minimap2.txt
#SBATCH -e /home/vale878/rror-%A_minimap2.txt
#SBATCH -t 04:00
#SBATCH --partition=bigmemm

module load minimap2
minimap2 -x splice -t 2 -k 12 -a -p 0.4 -N20 phg/inputDir/reference/Zm-B73-REFERENCE-NAM-5.0.fa phg/inputDir/assemblies/maff/refCDS.fa