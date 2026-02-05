#!/bin/bash

# Request resources:
#SBATCH --time=12:00:0  # 6 hours (hours:minutes:seconds)
#SBATCH -p shared
#SBATCH -n 1                    # number of MPI ranks
#SBATCH --cpus-per-task=16   # number of MPI ranks per CPU socket
#SBATCH --mem-per-cpu=1G

module load aocc/5.0.0
module load aocl/5.0.0
export MV2_ENABLE_AFFINITY=0

echo "Running code"
rm vtk_data/output/*

export REFINE=2

sbcl --dynamic-space-size 16000  --disable-debugger --load "tpb.lisp" --quit
