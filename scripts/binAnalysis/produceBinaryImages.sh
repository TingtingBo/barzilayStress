#!/bin/bash


## This sciript will be used to produce the binary images for the interaction figure for the TSE paper
## Just going to loop through where we see sig interactions and produce some binary images from the JLF labels for them
## Easy does it.


## First declare any statics
binScript="/data/joy/BBL/applications/xcpEngine/xcp_v06/utils/val2mask.R"
maskImageROI="/home/arosen/hiLo/data/05_BrainRankFigure/pncJLF/pncJLF_Labels.nii.gz"
maskImageLobe="/home/arosen/hiLo/data/05_BrainRankFigure/pncJLF/pncJLF_Lobes.nii.gz"
outputDir="/data/jux/BBL/projects/barzilayStress/scripts/binAnalysis/"
ohsnap="/share/apps/itksnap/itksnap-most-recent/bin/itksnap"
gsImg=""

## Now produce our in inary images
$binScript -i ${maskImageROI} -v 55,56 -o ${outputDir}pallidImage.nii.gz
$binScript -i ${maskImageROI} -v 44,45 -o ${outputDir}tbwmImage.nii.gz
$binScript -i ${maskImageROI} -v 23:34,47,48,55:62,70:207 -o ${outputDir}tbgmImage.nii.gz
$binScript -i ${maskImageLobe} -v 102,103 -o ${outputDir}insularImage.nii.gz
$binScript -i ${maskImageLobe} -v 106,107 -o ${outputDir}parietalImage.nii.gz

mkdir -p ${outputDir}/pallid
mv ${outputDir}pallidImage.nii.gz ${outputDir}/pallid
${ohsnap} -g /home/arosen/hiLo/data/05_BrainRankFigure/pncJLF/MNI152_T1_1mm_brain.nii.gz -s ${outputDir}/pallid/pallidImage.nii.gz

mkdir -p ${outputDir}/tbwm
mv ${outputDir}tbwmImage.nii.gz ${outputDir}/tbwm
${ohsnap} -g /home/arosen/hiLo/data/05_BrainRankFigure/pncJLF/MNI152_T1_1mm_brain.nii.gz -s ${outputDir}/tbwm/tbwmImage.nii.gz

mkdir -p ${outputDir}/tbgm
mv ${outputDir}tbgmImage.nii.gz ${outputDir}/tbgm
${ohsnap} -g /home/arosen/hiLo/data/05_BrainRankFigure/pncJLF/MNI152_T1_1mm_brain.nii.gz -s ${outputDir}/tbgm/tbgmImage.nii.gz

mkdir -p ${outputDir}/insular
mv ${outputDir}insularImage.nii.gz ${outputDir}/insular
${ohsnap} -g /home/arosen/hiLo/data/05_BrainRankFigure/pncJLF/MNI152_T1_1mm_brain.nii.gz -s ${outputDir}/insular/insularImage.nii.gz

mkdir -p ${outputDir}/parietal
mv ${outputDir}parietalImage.nii.gz ${outputDir}/parietal
${ohsnap} -g /home/arosen/hiLo/data/05_BrainRankFigure/pncJLF/MNI152_T1_1mm_brain.nii.gz -s ${outputDir}/parietal/parietalImage.nii.gz
