# epol
## Steps to setup EMVORADO-POL (eg. JUWELS at JSC)

1) First download EMVORADO-POL source codes from DKRZ

2) Download epol
git clone https://github.com/epol

3) Copy EMVORADO codes to epol
cp -r emvorado/src epol\
cp -r emvorado/externals epol

4) Compilation for JUWELS
cd epol/build_juwels\
make -f Makefile.rcl.intel.MPI.makedepf90 clean\
make -f Makefile.rcl.intel.MPI.makedepf90\
cd ..

5) Running EMVORADO
cd run_juwels\
./job_submission.ksh 



