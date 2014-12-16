F90=ftn
FFLAGS=-r8 -Mcuda -lcufft
DFLAGS=

tcudacalc: test_cuda_calc.cuf
	$(F90) -o tcudacalc test_cuda_calc.cuf $(FFLAGS) $(DFLAGS)
