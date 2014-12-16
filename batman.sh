#!/bin/bash
#PBS -N tcudacalc
#PBS -q debug
#PBS -l walltime=01:00:00 
#PBS -l nodes=1:ppn=1
#PBS -j oe

cd $PBS_O_WORKDIR

#export OMP_NUM_THREADS=$PBS_NUM_PPN
#NP=$PBS_NUM_NODES

#aprun -n $NP -d ${OMP_NUM_THREADS} ./tfft

aprun ./tcudacalc
