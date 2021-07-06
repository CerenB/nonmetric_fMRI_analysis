clear;
clc;

pth = fullfile(fileparts(mfilename('fullpath')), '..');
addpath(pth);

% add FFT analysis lib
addpath(genpath(fullfile(pth, 'lib', 'FFT_fMRI_analysis')));

%% set paths
% set spm
[~, hostname] = system('hostname');
warning('off');

if strcmp(deblank(hostname), 'tux')
  addpath(genpath('/home/tomo/Documents/MATLAB/spm12'));
elseif strcmp(deblank(hostname), 'mac-114-168.local')
  warning('off');
  addpath(genpath('/Users/battal/Documents/MATLAB/spm12'));
end

% add cpp repo
run ../lib/CPP_BIDS_SPM_pipeline/initCppSpm.m;

% get all the parameters needed
opt = getOptionBlock();

%% FFT analysis

opt.anatMask = 0;
opt.maskType = 'whole-brain'; 
[opt.funcMask, opt.maskType] = getMaskFile(opt);


% opt.maskType = 'neurosynth'; 
% opt.funcMask = getMaskFile(opt);

% want to save each run FFT results
opt.saveEachRun = 0;

for iSmooth = [0 2]
    
    opt.FWHM = iSmooth; % 0 2 3 or 6mm smoothing
    
    % step size
    opt.nStepsPerPeriod = 2;
    % run fft
    calculateSNR(opt);
    
    opt.nStepsPerPeriod = 4;
    calculateSNR(opt);
end


%%
% group analysis - for now only in MNI
% individual space would require fsaverage
opt.nStepsPerPeriod = 2;
opt.FWHM = 2;
opt = groupAverageSNR(opt);





