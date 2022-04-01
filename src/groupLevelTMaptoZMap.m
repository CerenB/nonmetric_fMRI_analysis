% this is a quick and easy wat to convert the GLM tmaps into zmaps
% we are only converting the group level maps.
% subject level will be done elsewhere to SNR calculation see script
% calculatePeakSNR.m

% we only use GLM results - meaning only Nonmetric design + GLM analysis
% tmap


df = 9; % number of subjects - 2
mainpath = '/Users/battal/Cerens_files/fMRI/Processed/RhythmCateg/Nonmetric/derivatives/cpp_spm-stats/group/task-Nonmetric_space-MNI_FWHM-6_conFWHM-8/';


% all sounds
inputImagePath = fullfile(mainpath,'AllCateg');
inputImgName = fullfile(inputImagePath, 'Nonmetric_AllSounds_p0001_uncorr_s6_con8.nii');
[img, outputName] = convertTstatsToZscore(inputImgName,  df);


% block_complex
inputImagePath = fullfile(mainpath,'block_nonmetric');
inputImgName = fullfile(inputImagePath, 'Nonmetric_NonmetricVsRest_s6_c8_p0001_uncorr.nii');
[img, outputName] = convertTstatsToZscore(inputImgName,  df);


% block_simple
inputImagePath = fullfile(mainpath,'block_simple');
inputImgName = fullfile(inputImagePath, 'Nonmetric_SimpleVsRest_s6_c8_p0001_uncorr.nii');
[img, outputName] = convertTstatsToZscore(inputImgName,  df);



% block_simple vs. nonmetric
inputImagePath = fullfile(mainpath,'CategA_gt_CategB');
inputImgName = fullfile(inputImagePath, 'Nonmetric_SimplevsNonmetric_s6_con8_p0001_uncorr.nii');
[img, outputName] = convertTstatsToZscore(inputImgName,  df);




