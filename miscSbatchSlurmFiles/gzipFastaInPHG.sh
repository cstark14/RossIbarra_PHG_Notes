#!/bin/bash -l
#SBATCH -D /group/jrigrp11/cstark/filesFromMicahK_Danforth/
#SBATCH -J gzipFastaFilesFromMicahDanforth
#SBATCH -o /group/jrigrp11/cstark/filesFromMicahK_Danforth/out-%j_gzipFA.txt
#SBATCH -e /group/jrigrp11/cstark/filesFromMicahK_Danforth/error-%j_gzipFA.txt
#SBATCH -t 24:00:00
#SBATCH --partition=high2

gzip /group/jrigrp11/cstark/filesFromMicahK_Danforth/phg_v2.4.8.162_ZeaSyn/output/updated_assemblies/*.fa