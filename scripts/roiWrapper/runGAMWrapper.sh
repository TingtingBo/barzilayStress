#!/bin/bash

# AFGR December 6th 2017
# This script will be used to run the GAM roi wise wrapper for Dr. Brazley's 
# early stress experiment

# Define statics
gamScript="/data/joy/BBL/tutorials/code/tutorials/roiWrapper/gamROI.R"
dataDir="/data/joy/BBL/projects/brazleyStress/data/"
subjID="bblid,scanid"
pAdjustMethod="fdr"
inputDir="/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/"
baseCovsForm="~s(ageAtScan1,k=4)+Cummulative_Stress_Load_No_Rape+sex+race2"

# Now dynamic variable arrays
covsFile=( t1Cov.RDS t1Cov.RDS t1Cov.RDS cbfCov.RDS restCov.RDS restCov.RDS dtiCov.RDS )
excludeVals=( t1Exclude t1Exclude t1Exclude pcaslExclude  restExclude restExclude dti64Exclude )
covIn=( averageManualRating averageManualRating averageManualRating pcaslTSNR restRelMeanRMSMotion restRelMeanRMSMotion dti64Tsnr )
inputName=( t1struct/n1601_jlfAntsCTIntersectionVol_20170412.csv t1struct/n1601_jlfAntsCTIntersectionCT_20170331.csv t1struct/n1601_jlfAtroposIntersectionGMD_20170410.csv asl/n1601_jlfAntsCTIntersectionPcaslValues_20170403.csv rest/n1601_jlfReHoValues_20170714.csv rest/n1601_jlfALFFValues_20170714.csv dti/n1601_jlfTRValues_20170411.csv )

# Now loop thorugh the length of our array and run our univariate analyses for each metric
for i in `seq 0 6` ; do 
  formulaValue=`echo ${baseCovsForm}+${covIn[i]}`
  covsValue=${dataDir}${covsFile[i]}
  inputData=${inputDir}${inputName[i]}

  # Now call the script 
  Rscript ${gamScript} -c ${covsValue} -o ${dataDir} -p ${inputData} -i ${excludeVals[i]} -u ${subjID} -f ${formulaValue} -a ${pAdjustMethod} -r false -n 1
  echo ${formulaValue} ; 
done
