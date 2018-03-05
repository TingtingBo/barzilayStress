#!/bin/bash

# AFGR December 6th 2017
# This script will be used to run the GAM roi wise wrapper for Dr. Brazley's 
# early stress experiment

# Define statics
gamScript="/data/joy/BBL/tutorials/code/roiWrapper/gamROI.R"
dataDir="/data/joy/BBL/projects/barzilayStress/data/"
subjID="bblid,scanid"
pAdjustMethod="fdr"
inputDir="/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/"
baseCovsForm="~s(ageAtScan1,k=4)+Cummulative_Stress_Load_No_Rape+sex+envSES"

# Now dynamic variable arrays
covsFile=( t1Cov.RDS t1Cov.RDS t1Cov.RDS cbfCov.RDS restCov.RDS restCov.RDS dtiCov.RDS faCov.RDS )
excludeVals=( t1Exclude t1Exclude t1Exclude pcaslExclude  restExclude restExclude dti64Exclude dti64Exclude )
covIn=( averageManualRating averageManualRating averageManualRating pcaslTSNR restRelMeanRMSMotion restRelMeanRMSMotion dti64Tsnr dti64Tsnr )
inputName=( t1struct/n1601_jlfAntsCTIntersectionVol_20170412.csv t1struct/n1601_jlfAntsCTIntersectionCT_20170331.csv t1struct/n1601_jlfAtroposIntersectionGMD_20170410.csv asl/n1601_jlfAntsCTIntersectionPcaslValues_20170403.csv rest/n1601_jlfReHoValues_20170714.csv rest/n1601_jlfALFFValues_20170714.csv dti/n1601_jlfTRValues_20170411.csv dti/n1601_JHULabelsFA_20170321.csv )

# Now loop thorugh the length of our array and run our univariate analyses for each metric
for i in `seq 0 7` ; do 
  formulaValue=`echo ${baseCovsForm}+${covIn[i]}`
  covsValue=${dataDir}${covsFile[i]}
  inputData=${inputDir}${inputName[i]}

  # Now call the script 
  Rscript ${gamScript} -c ${covsValue} -o ${dataDir} -p ${inputData} -i ${excludeVals[i]} -u ${subjID} -f ${formulaValue} -a ${pAdjustMethod} -r false -n 1
  echo ${formulaValue} ; 
done

# Now add the overall pathology but it is age regressed need to see if that is a probblem!
baseCovsForm="~s(ageAtScan1,k=4)+Cummulative_Stress_Load_No_Rape+sex+envSES+Overall_Psychopathology_ar"
for i in `seq 0 7` ; do 
  formulaValue=`echo ${baseCovsForm}+${covIn[i]}`
  covsValue=${dataDir}${covsFile[i]}
  inputData=${inputDir}${inputName[i]}

  # Now call the script 
  Rscript ${gamScript} -c ${covsValue} -o ${dataDir} -p ${inputData} -i ${excludeVals[i]} -u ${subjID} -f ${formulaValue} -a ${pAdjustMethod} -r false -n 1
  echo ${formulaValue} ; 
done

baseCovsForm="~s(ageAtScan1,k=4)+sex+race2+Overall_Psychopathology_ar*Cummulative_Stress_Load_No_Rape"
for i in `seq 0 7` ; do 
  formulaValue=`echo ${baseCovsForm}+${covIn[i]}`
  covsValue=${dataDir}${covsFile[i]}
  inputData=${inputDir}${inputName[i]}

  # Now call the script 
  Rscript ${gamScript} -c ${covsValue} -o ${dataDir} -p ${inputData} -i ${excludeVals[i]} -u ${subjID} -f ${formulaValue} -a ${pAdjustMethod} -r false -n 1
  echo ${formulaValue} ; 
done

# Now do just the overall p 
baseCovsForm="~s(ageAtScan1,k=4)+Overall_Psychopathology_ar+sex+envSES"
for i in `seq 0 7` ; do 
  formulaValue=`echo ${baseCovsForm}+${covIn[i]}`
  covsValue=${dataDir}${covsFile[i]}
  inputData=${inputDir}${inputName[i]}

  # Now call the script 
  Rscript ${gamScript} -c ${covsValue} -o ${dataDir} -p ${inputData} -i ${excludeVals[i]} -u ${subjID} -f ${formulaValue} -a ${pAdjustMethod} -r false -n 1
  echo ${formulaValue} ; 
done

# Now probe an interaction between PTSD nad no PTSD
# This requires different cov files and a diff model
covsFile=( t1CovIN.RDS t1CovIN.RDS t1CovIN.RDS cbfCovIN.RDS restCovIN.RDS restCovIN.RDS dtiCovIN.RDS faCovIN.RDS )
baseCovsForm="~s(ageAtScan1,k=4)+Cummulative_Stress_Load_No_Rape*PTSD+sex+envSES"

# Now loop thorugh the length of our array and run our univariate analyses for each metric
for i in `seq 0 7` ; do 
  formulaValue=`echo ${baseCovsForm}+${covIn[i]}`
  covsValue=${dataDir}${covsFile[i]}
  inputData=${inputDir}${inputName[i]}

  # Now call the script 
  Rscript ${gamScript} -c ${covsValue} -o ${dataDir} -p ${inputData} -i ${excludeVals[i]} -u ${subjID} -f ${formulaValue} -a ${pAdjustMethod} -r false -n 1
  echo ${formulaValue} ; 
done
