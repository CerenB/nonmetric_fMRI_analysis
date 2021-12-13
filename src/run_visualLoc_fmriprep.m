clear;
clc;

%% set paths
% set spm
warning('off');
addpath(genpath('/Users/battal/Documents/MATLAB/spm12'));


% bspm fmri
% addpath(genpath('/Users/battal/Documents/MATLAB/bspmview'));
% add xjview
% addpath(genpath('/Users/battal/Documents/MATLAB/xjview'));

% add cpp repo
run ../lib/CPP_SPM/initCppSpm.m;

% get all the parameters needed
opt = getOption_fmriprep();

%% Run batches
% reportBIDS(opt);
bidsCopyInputFolder(opt);

bidsSTC(opt);

bidsSpatialPrepro(opt);

anatomicalQA(opt);

bidsResliceTpmToFunc(opt);

% % smoothing
% default is:
% opt.fwhm.func = 6 % - FWHM to apply to the preprocessed functional images.
% opt.fwhm.contrast = 6 % for group level. 
% if you want to change add here e.g.
% opt.fwhm.func = 2;
bidsSmoothing(opt);
