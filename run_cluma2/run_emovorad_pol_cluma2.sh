#!/bin/bash

# This is a runscript for running the offline version of EMVORADO
# without observational radar data. Radar stations (locations, scan
# strategies, wavelengths etc. are defined in namelist RADARSIM_PARAMS).
# The input data are from an idealized COSMO-run with Weisman-Klemp-Type
# warm bubble triggered convection.
#
# The executable can be built using gfortran, openMPI, eccodes, hdf5 and netcdf by running
# $>  cd ../build_gfortran
# $>  make -f Makefile.gfortran.openMPI.makedepf90 -j10
#   or for the serial binary without openMPI instead of the parallel on onee
# $> make -f Makefile.gfortran.serial.makedepf90 -j10


##############################################################
# Give the absolute path and name of the executable:
##############################################################

# path to model binary, including the executable:
EXECUTABLE=${HOME}/emvorado-offline/build_cluma2/cosmo_refl_offline_master_par_rcl-x86-gnu.exe

#jobdatei=job_emvorado_offline
jobdatei=__PBSSCRIPT__

##############################################################
# Number of processors:
##############################################################

jobclass=batch
#mem=120            # requested memory in gb
jobmem=2            # requested memory in gb

nprocpernode=48    # number of cores per logical host; <=32

omp_threads=1      # do not change!
nprocx=8          # nprocx*nprocy+nprocio_radar <= 200 AND a multiple of nprocpernode
nprocy=6
nprocio_radar=0    # if > 0, apply async radar io

let nproc=nprocx*nprocy+nprocio_radar
let nnode=(nproc+nprocpernode-1)/nprocpernode
let mem=nprocpernode*jobmem

maxcputime=700:00:00  # hh:mm:ss


##############################################################
# Define some directories (ABSOLUTE PATH NAMES!!!) and
#  other parameters:
##############################################################

inputdir=__INPUTDIR__
outputdir=__RUNDIR__
ydir_mielookup=__LUTDIR__
ydirradarin=${inputdir}
ydirradarout=__OUTPUTDIR__
yinput_format=ncdf-cosmo

model_starttime=__MODEL_STARTTIME__
forecast_time=__FORECAST_TIME__

levtyp=eta
modgrid_filename='ivr/lfff00000000c.nc'
moddata_filename='lfff'${forecast_time}'.nc'

#
gscp2=__ITYPE_GSCP__
lambdaradar=__LAMBDA_RADAR__
ityperefl=__ITYPE_REFL__
hydroset=__HYDROSET__
##############################################################
# Preparations:
##############################################################

mkdir -p $ydir_mielookup
mkdir -p $outputdir
mkdir -p $ydirradarout

radaroutput=radvop.out
radarerrput=radvop.err

jobname_in_queue=__JOBNAME__

##############################################################
# Change to the output directory and start the operator run:
##############################################################

# Copy (this) runscript to outdir
# Not here, though, in the template script for the time loop. In this case this
# needs to be and is taken care of in the job_submit script.
#cp "$(readlink -f $0)" ${outputdir}

cd ${outputdir}

#rm ${outputdir}/*
#rm ${ydirradarout}/*

##############################################################
# Create namelist input:
##############################################################

cat > INPUT_DBZSIM << end_input_dbzsim
&GRID_IN
  !
  nprocx = ${nprocx},
  nprocy = ${nprocy},
  nprocio_radar = ${nprocio_radar},
  !
  ! Grid filename
  modgrid_filename = '${modgrid_filename}',
  !
  ! Type of vertical levels of input data ("eta", "z  ", "p  "):
  levtyp='${levtyp}',
  !
  ! whether or not U and V are on a staggered grid for grib-input:
  luv_staggered = .true.
  !
  ! whether or not U and V are defined relative to the rotated latlon grid
  !  (.TRUE.) or to the geographic directions (.FALSE.)
  luv_rotated = .true.
  !
  ! dlon = dx / rearth * (180 / PI) in degrees
  dlon=0.010,
  ! dlat = dy / rearth * (180 / PI) in degrees
  dlat=0.010,
  ! longitude of the lower left corner:
  startlon_tot=-1.50,
  ! latitude of the lower left corner:
  startlat_tot=-1.75,
  ie_tot = 300,   ! lon
  je_tot = 300,   ! lat
  ke_tot = 80,
  nboundlines = 3,
  pollon = -173.0,
  pollat = 39.0,
  ! Define cut-out domain on which the operator should run
  !  (e.g., to save memory and runtime during code testing):
  !is_sub_tot = 131,  ! start index in i-dir of cut-out window
  !ie_sub_tot = 180,  ! end   index in i-dir    -"-
  !js_sub_tot = 124,  ! start index in j-dir    -"-
  !je_sub_tot = 173,  ! end   index in j-dir    -"-
  !is_sub_tot = 155,  ! start index in i-dir of cut-out window
  !ie_sub_tot = 157,  ! end   index in i-dir    -"-
  !js_sub_tot = 148,  ! start index in j-dir    -"-
  !je_sub_tot = 150,  ! end   index in j-dir    -"-
 /

&REFL_OFFLINE
  !
  ldebug_refloffline=.true.,
  !
  ! 1) Time stamps of ASCII file names (input and output): 
  inputdir='${inputdir}',
  outputdir='${outputdir}',
  yinput_format='${yinput_format}',
  moddata_filename = '${moddata_filename}',
  model_starttime='${model_starttime}',
  forecast_time='${forecast_time}',
  !
  ! Type of microphysics scheme (same as in COSMO-model):
  itype_gscp=${gscp2},
  !
  ! Are the microphysics variables from the COSMO-model stored as
  !  densities (.true.) or mass-specific values (.false.) in the input files?
  linput_q_densities=.false.,
  !
  ! Should the output file be automatically gzip compressed?
  gzipflag=.true.,
  ! Suffix of the output dBZ file --- exactly 10 characters,
  ! for example 'dbz-mie   ', 'dbz-ray   '
  dbz_suffix='dbz-mie   ',
  !
  ! 2) Characteristics of the reflectiviy calculation (not for radar simulator,
  !     this here is an independent output of the grid point values!):
  !
  dbz_meta%itype_refl=1,
    ! Flag for using Mie lookup tables for program speedup:
    dbz_meta%llookup_mie=.false.,
    ! Wavelength of the radar in m:
    dbz_meta%lambda_radar=${lambdaradar},
    !
    ! Do not touch anything below, if you are not sure what that is!
    dbz_meta%station_id=-999999,
    !  (Description can be found in document mielib.pdf)
    dbz_meta%Tmeltbegin_s=273.15,
    dbz_meta%Tmeltbegin_g=273.15,
    dbz_meta%Tmeltbegin_h=273.15,
    dbz_meta%meltdegTmin_s=0.0,
    dbz_meta%meltdegTmin_g=0.0,
    dbz_meta%meltdegTmin_h=0.0,
    ! Einstellungen fuer itype_refl=1 (effektiver Brechungsindex bei Mie-Streuung):
      dbz_meta%ctype_drysnow_mie=    'masmas      ',
      dbz_meta%ctype_wetsnow_mie=    'mawsasmawsas',
      dbz_meta%ctype_drygraupel_mie= 'mis         ',
      ! Typ des Schmelzmodells fuer die Mie-Rechnung bei schmelzendem Graupel (1=soak, 2=twosphere, 3=watersphere):
      ! (+zugehoeriges ctype_wetgraupel_mie -- entsprechend anpassen)
      dbz_meta%igraupel_type=1,
      dbz_meta%ctype_wetgraupel_mie= 'mawsms      ',
      ! Nur bei itype_gscp >= 2000 wirksam:
      dbz_meta%ctype_dryhail_mie=    'mis         ',
      dbz_meta%ctype_wethail_mie=    'mws         ',
    ! Einstellungen fuer itype_refl=2 (effektiver Brechungsindex bei Rayleigh-Streuung):
      dbz_meta%ctype_drysnow_ray=    'mas         ',
      dbz_meta%ctype_wetsnow_ray=    'mawsas      ',
      dbz_meta%ctype_drygraupel_ray= 'mis         ',
      dbz_meta%ctype_wetgraupel_ray= 'mawsms      ',
      ! Nur bei itype_gscp >= 2000 wirksam:
      dbz_meta%ctype_dryhail_ray=    'mis         ',
      dbz_meta%ctype_wethail_ray=    'mawsms      ',
 /
end_input_dbzsim

cat > INPUT_RADARSIM << end_input_radarsim
&RADARSIM_PARAMS
!
dom = 1,
!
!--------------------------------------------------------
! Number of radar stations to simulate:
!   For each station from ista=1, nradsta_namelist, you can define one
!   structure block rs_meta(ista) and dbz_meta(ista) below.
!   If you define more blocks than nradsta_namelist, only the
!   first nradsta_namelist stations will be simulated.
! Important: give a unique station id to each station, which
!   is an integer number > 0 with at most 6 digits.
!
  !!! since the domain here is VERY small (11x16), only set a single station
  nradsta_namelist = 1, !4,
!
!--------------------------------------------------------
! Country flag for the observational radar data and/or
!  desired background radar meta data list:
!
!  1 = DWD German radar network       (default)
!  2 = MeteoSwiss Swiss radar network
!
  icountry = 1,
!
!--------------------------------------------------------
! GLOBAL SETTINGS, I.E., THE SAME FOR ALL RADAR STATIONS:
!--------------------------------------------------------
!CPS Lookup table
  itype_mpipar_lookupgen = 2,
  llookup_interp_mode_dualpol = .true.,
!CPS
!
  ldebug_radsim = .false., !.true.,
  lout_geom = .false.,
  loutradwind = .false.,
    lfill_vr_backgroundwind = .false.,
  loutdbz = .true.,
! compute and output all polarimetric variables:
!    loutpolall = .true.,
! compute and output only a "standard" subset:
    loutpolstd = .true.,
    lextdbz = .true.,
    ! Global switch to choose itype_refl for all radars. 
    !  This is then the background value for all radars, which can
    !  later be overwritten for the single stations via namelist.
    !  If no value or -99 for itype_refl_glob is given, the default
    !  value from dbz_namlst_d (3) will win.
    !  You can override it for each station by individual settings
    !  in the "dbz_meta"- structure below.
    dbz_meta_glob%itype_refl =${ityperefl},
    !CPS dbz_meta_glob%itype_Dref_fmelt = 1,   ! only effective for Mie, Tmatrix
    dbz_meta_glob%itype_Dref_fmelt = 2,        ! for fast lookup table
    ! Global switch to enable the use of Mie-lookup tables for all radars. 
    !  This is then the background value for all radars, which can
    !  later be overwritten for the single stations via namelist.
    dbz_meta_glob%llookup_mie = .true., !false.,
      itype_mpipar_lookupgen = 2,
      pe_start_lookupgen = 0,
      ydir_mielookup_read  = '${ydir_mielookup}',
      ydir_mielookup_write = '${ydir_mielookup}',
  lweightdbz = .false.,
  lfall = .false.,
  lonline = .false.,
    lsode   = .false.,
  lsmooth = .false.,
    ! Similar to itype_refl_glob, one can define background
    !  values for the number of smoothing points. If -99 or
    !  no values are given, the default
    !  value 1 will win.
    ngpsm_h_glob = 5,
    ngpsm_v_glob = 9,
  ! Take into account the effects of the minimum detectable signal
  !   for reflectivity and/or radial velocity:
  !   (There are 2 components of the rs_meta-structure for each radar station which hold
  !    the reference MDS, rs_meta%mds_Z0 [dBZ], at a certain reference range, rs_meta%mds_r0 [m].
  !    These can be set in the rs_meta-section below. The default is -20 dBZ at 10 km range.)
  lmds_z  = .false.,
  lmds_vr = .false.,
!
  dbz_meta_glob%lhydrom_choice_testing=${hydroset},

  ! JM210624:
  ! commented out to trigger default settings, which should be suitable for summer convection
  !CPS
  !dbz_meta_glob%Tmax_min_i=278.15,
  !dbz_meta_glob%Tmax_max_i=278.15,
  !dbz_meta_glob%Tmax_min_s=276.15,
  !dbz_meta_glob%Tmax_max_s=283.15,
  !dbz_meta_glob%Tmax_min_g=278.15,
  !dbz_meta_glob%Tmax_max_g=278.15,
  !dbz_meta_glob%Tmax_min_h=293.15,
  !dbz_meta_glob%Tmax_max_h=293.15,
  !dbz_meta_glob%Tmeltbegin_s=273.15,
  !dbz_meta_glob%Tmeltbegin_g=273.15,
  !dbz_meta_glob%Tmeltbegin_h=273.15,
  !dbz_meta_glob%meltdegTmin_s=0.0,
  !dbz_meta_glob%meltdegTmin_g=0.0,
  !dbz_meta_glob%meltdegTmin_h=0.0,
  !CPS
!
  lvoldata_output = true., !.false., !
    voldata_ostream(1)%format = 'cdfin-mulmom'  ! 'ascii', 'ascii-gzip', 'f90-binary', 'cdfin', 'cdfin-mulmom', 'grib2', 'grib2-mulmom'
    voldata_ostream(1)%content_dt = 0.0,
    voldata_ostream(1)%content_tref = 0.0,
    voldata_ostream(1)%output_list = 'all'

  lfdbk_output  = .false.,
!
  loutvolaux = .true.,
! loutnwp = .true.,
  lmodfield_output = .true.,
!
! Output directory of all radar files:
  ydirradarout = '${ydirradarout}',
!
!--------------------------------------------------------
! INDIVIDUAL SETTINGS FOR EACH RADAR STATION:
!--------------------------------------------------------
!
!--------------------------------------------------------
! STATION 1: rs_meta(1) and dbz_meta(1):
!
  rs_meta(1)%station_id          = 66150,
  rs_meta(1)%lambda  = ${lambdaradar},
! BoxPol coordinates
  rs_meta(1)%lat     = 50.73052,
  rs_meta(1)%lon     = 7.071663,
  rs_meta(1)%alt_msl = 99.5
! Scan definition
  rs_meta(1)%ra_inc  = 150.,
  rs_meta(1)%nra     = 1000,
  rs_meta(1)%nel     = 1,
  rs_meta(1)%el_arr  = 1.0, 1.5, 2.4, 4.5, 8.2, 11.0, 14.0, 18.0, 28.0,
!
  rs_meta(1)%ngpsm_v     = 1
  rs_meta(1)%ngpsm_h     = 1
!
 /
end_input_radarsim

##############################################################
# Strip comments from INPUT-files using ed
# (is necessary for the gfortran-compiler and does not
#  do any harm for other compilers)
##############################################################

for infi in INPUT_*
do
	
    # First, strip entire-lines-comments:
    echo Stripping comments from $infi ...
	
    ed $infi <<EOF
.
1,\$g/^ *!.*/d
.
w
q
EOF
    
    # Then, strip comments appearing on 
    # the same lines as namelist parameters:
    cat $infi | awk -F! '{gsub(" *$","",$1); print $1}' - > tmptmp.tmp
    mv tmptmp.tmp $infi

done


##############################################################
# Create batch job script:
##############################################################

cat > $jobdatei << marke
#!/bin/bash

#Job Submission to Cluma
#PBS -N ${jobname_in_queue} 
#PBS -l walltime=${maxcputime}
#PBS -l nodes=${nnode}:ppn=${nprocpernode} 
#PBS -V 
#PBS -u ${USER}
#PBS -q ${jobclass}    


echo
echo Starting at $(date)
echo

cd ${outputdir}

#################################################
# run the program
#################################################

# machine specific for X86 CLUMA2 

export OMP_NUM_THREADS=${omp_threads}
# set the stack size to unlimited, otherwise strange crashes may happen:
ulimit -s unlimited
export LD_LIBRARY_PATH=/usr/local/gcc94_suite/lib64/:/daten/daten01/z4/local/lapack-3.8.0/lib

/usr/local/gcc94_suite/bin/mpirun -np ${nproc} --mca btl_openib_allow_ib 1 ${EXECUTABLE} >> log_file 2>> err_file


marke

##############################################################
# Start the job script from the current directory:
##############################################################

chmod u+x $jobdatei

#qsub $jobdatei

