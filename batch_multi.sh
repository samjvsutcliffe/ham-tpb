#!/bin/bash

# Request resources:
#SBATCH --time=12:00:0  # 6 hours (hours:minutes:seconds)
#SBATCH -p shared
#SBATCH -n 16                    # number of MPI ranks
#SBATCH --cpus-per-task=16   # number of MPI ranks per CPU socket
#SBATCH --mem-per-cpu=1G
#SBATCH -N 1-16                    # number of compute nodes. 

module load gcc
module load intelmpi/2021.10
module load aocl

echo "Running code"
rm output/*

#sbcl --dynamic-space-size 16000  --disable-debugger --load "build_step.lisp" --quit

#cp ~/quicklisp/local-projects/cl-mpm-worker/mpi-worker ./

#echo $OMP_NUM_THREADS
#export KAPPA=1.0
#export LC=1
#export REFINE=4
mpirun ./mpi-worker --dynamic-space-size 16000
