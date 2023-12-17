module load gcc
module load intelmpi/2021.10
module load aocl
sbcl --dynamic-space-size 16000  --disable-debugger --load "build_step.lisp" --quit
rm -r output-*
export KAPPA=1
export LC=1
export REFINE=1
sbatch batch_multi.sh 
export REFINE=2
sbatch batch_multi.sh 
export REFINE=4
sbatch batch_multi.sh 
export REFINE=6
sbatch batch_multi.sh
export REFINE=8
sbatch batch_multi.sh
export REFINE=10
sbatch batch_multi.sh
