#!/bin/ksh
#Usage ./job_submission.ksh
#Other Inputs: $pfo_root/input/ftime_file.txt <for time loop>
#Namelist for PFO: $pfo_root/input/INPUT_DBZSIM1600
#job submission script: $pfo_root/input/pfo_run.ksh
#This script updates the namelist, job-submission script and submits the job

#Log: J Mendrok, modified from bpfo job script by P Shrestha

#--------------------------------------------------------------------------------------------------------
# User Settings
 projectdir=${HOME}
 emvodir=${projectdir}'/emvorado-offline/run_juwels/'

 script='run_emovorad_pol_juwels.sh'
 
 hm='' # all
 hydroset='.TRUE.,.TRUE.,.TRUE.,.TRUE.,.TRUE.,.TRUE.' #c,r,i,s,g,h
 itype_gscp='2483'
 itype_refl='5'
 lambdaradar='0.032'
 casename='HET_PROM1'
 #compute lookup table
 do_lut='true'
 lnd='HET'
 expname='runE_6_HET2483'
 expday='01115500'
 model_starttime='20150704030000'
 inputdir='/p/largedata/hbn33/shrestha1/'${casename}'/'${lnd}${itype_gscp}'/'${expname}'/cosout/'
 casedir=${emvodir}'/BoxPol-volscan.'${expday}'.'${expname}_${casename}

#-------------------------------------------------------------------------------------------------------
# Prep and run cases

 pbsscript=job_emvorado_offline
 cd ${emvodir}
 #rm -r ${casedir}
 mkdir -p ${casedir}
 
 #for reproducability, copy and keep this script and the runscript template in casedir
 cp "$(readlink -f $0)" ${casedir}/
 cp $script ${casedir}/

 outputdir=${casedir}'/emvo_output/'
 mkdir -p ${outputdir}
 workdir=${casedir}'/emvo_runs/'
 mkdir -p ${workdir}
 lutdir=${emvodir}'/lookuptables/'
 mkdir -p ${lutdir}
 pfodir=${casedir}'/pfo_output/'
 mkdir -p ${pfodir}
 anadir=${casedir}'/ana_output/'
 mkdir -p ${anadir}

# Big Loop for Time SnapShots
 timedir=${emvodir}/input/
 file=${timedir}"ftime_file.txt"

 if [[ "${do_lut}"0 == true0 ]]; then
   calc_lut='true'
 else
   calc_lut='false'
 fi

 while IFS= read -r line
   do

   forecast_time=${line}
   echo $forecast_time
   workno='run_'${forecast_time} 

   rundir=${workdir}/${workno}
   mkdir -p ${rundir}
   rm ${rundir}/*

   sid=${model_starttime:3:1}${model_starttime:5:1}${model_starttime:7:1} # last digits in year, month and day of modelstart_time
   fid=${forecast_time:2:4} # hhmm of forecast_time
   jobname=E${sid}${fid}

   cp loadenvsJUWELS.Intel ${rundir}/
   cp $script ${rundir}/
   sed "s@__INPUTDIR__@${inputdir}@g" -i ${rundir}/$script 
   sed "s@__OUTPUTDIR__@${outputdir}@g" -i ${rundir}/$script
   sed "s@__RUNDIR__@${rundir}@g" -i ${rundir}/$script
   sed "s@__LUTDIR__@${lutdir}@g" -i ${rundir}/$script
   sed "s@__FORECAST_TIME__@${forecast_time}@g" -i ${rundir}/$script
   sed "s@__MODEL_STARTTIME__@${model_starttime}@g" -i ${rundir}/$script
   sed "s@__HYDROSET__@${hydroset}@g" -i ${rundir}/$script
   sed "s@__ITYPE_GSCP__@${itype_gscp}@g" -i ${rundir}/$script
   sed "s@__ITYPE_REFL__@${itype_refl}@g" -i ${rundir}/$script
   sed "s@__LAMBDA_RADAR__@${lambdaradar}@g" -i ${rundir}/$script
   #sed "s@__ATTENUATE__@${att}@g" -i ${rundir}/$script
   sed "s@__PBSSCRIPT__@${pbsscript}@g" -i ${rundir}/$script
   sed "s@__JOBNAME__@${jobname}@g" -i ${rundir}/$script

   #Submit JOB
   cd ${rundir}
   pwd
   chmod u=+rx $script
   source loadenvsJUWELS.Intel 
   ./$script
   echo ${calc_lut}
   if [[ "${calc_lut}"0 == true0 ]]; then
     lut_id=$(sbatch $pbsscript)
     lut_id=${lut_id:8:12}
     #echo "jobid: "${lut_id}
     calc_lut='false'
   else
     if [[ "${do_lut}"0 == true0 ]]; then
       #echo "qsub --after "$lut_id $pbsscript
       sbatch --wait $lut_id $pbsscript
     else
       sbatch $pbsscript
     fi
   fi
   cd ${emvodir}
 done <"$file" # Big Loop
exit 0

