mkdir -p ./meNoRace
/home/arosen/hiLo/scripts/05_BrainRankFigure/scripts/makeZScoreJLFPNCTemplateImage.sh stressME-KEY.csv 4 1
mv outputImage.nii.gz meNoRace/

mkdir -p ./meWithRace
/home/arosen/hiLo/scripts/05_BrainRankFigure/scripts/makeZScoreJLFPNCTemplateImage.sh stressMEwithRace-KEY.csv 4 1
mv outputImage.nii.gz meWithRace/

mkdir -p ./meWithWhite
/home/arosen/hiLo/scripts/05_BrainRankFigure/scripts/makeZScoreJLFPNCTemplateImage.sh stressMEwithWhite-KEY.csv 4 1
mv outputImage.nii.gz meWithWhite/

mkdir -p ./meWithBlack
/home/arosen/hiLo/scripts/05_BrainRankFigure/scripts/makeZScoreJLFPNCTemplateImage.sh stressMEwithBlack-KEY.csv 4 1
mv outputImage.nii.gz meWithBlack/

mkdir -p ./intNoRace
/home/arosen/hiLo/scripts/05_BrainRankFigure/scripts/makeZScoreJLFPNCTemplateImage.sh stressInteraction-KEY.csv 4 1
mv outputImage.nii.gz intNoRace/

mkdir -p ./intWithRace
/home/arosen/hiLo/scripts/05_BrainRankFigure/scripts/makeZScoreJLFPNCTemplateImage.sh stressInteractionRace-KEY.csv 4 1
mv outputImage.nii.gz intWithRace/

cd meNoRace
itksnap -g /share/apps/fsl/5.0.8/data/standard/MNI152_T1_1mm_brain.nii.gz -s outputImage.nii.gz -l ../stressME-ColorTable.txt

cd ../meWithRace
itksnap -g /share/apps/fsl/5.0.8/data/standard/MNI152_T1_1mm_brain.nii.gz -s outputImage.nii.gz -l ../stressMEwithRace-ColorTable.txt

cd ../intNoRace
itksnap -g /share/apps/fsl/5.0.8/data/standard/MNI152_T1_1mm_brain.nii.gz -s outputImage.nii.gz -l ../stressInteraction-ColorTable.txt

cd ../intWithRace
itksnap -g /share/apps/fsl/5.0.8/data/standard/MNI152_T1_1mm_brain.nii.gz -s outputImage.nii.gz -l ../stressInteractionRace-ColorTable.txt

cd ../meWithWhite
itksnap -g /share/apps/fsl/5.0.8/data/standard/MNI152_T1_1mm_brain.nii.gz -s outputImage.nii.gz -l ../stressMEwithWhite-ColorTable.txt

cd ../meWithBlack
itksnap -g /share/apps/fsl/5.0.8/data/standard/MNI152_T1_1mm_brain.nii.gz -s outputImage.nii.gz -l ../stressMEwithBlack-ColorTable.txt
