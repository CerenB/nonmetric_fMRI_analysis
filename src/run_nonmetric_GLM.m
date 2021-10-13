clear;
clc;

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

% bspm fmri
% addpath(genpath('/Users/battal/Documents/MATLAB/bspmview'));
% add xjview
% addpath(genpath('/Users/battal/Documents/MATLAB/xjview'));

% add cpp repo
run ../lib/CPP_SPM/initCppSpm.m;

% get all the parameters needed
opt = getOptionNonmetric();

%% Run batches
reportBIDS(opt);
bidsCopyRawFolder(opt, 1);

% %
bidsSTC(opt);
% % %
bidsSpatialPrepro(opt);
%
% % Quality control
% % anatomicalQA(opt);
% % bidsResliceTpmToFunc(opt);
% % functionalQA(opt);
%
% % smoothing
funcFWHM = 6;
bidsSmoothing(funcFWHM, opt);
%
%
% subject level univariate
bidsFFX('specifyAndEstimate', opt, funcFWHM);
bidsFFX('contrasts', opt, funcFWHM);
%
%
% funcFWHM = 3;
% bidsSmoothing(funcFWHM, opt);
% bidsFFX('specifyAndEstimate', opt, funcFWHM);
% bidsFFX('contrasts', opt, funcFWHM);
%
% %visualise the results
% %bidsResults(opt, funcFWHM);
%
% % % group level univariate
conFWHM = 8;
bidsRFX('smoothContrasts', opt, funcFWHM, conFWHM);
bidsRFX('RFX', opt, funcFWHM, conFWHM);
%
% % WIP: group level results
% % bidsResults(opt, FWHM);
%

%% MVPA - prep
% funcFWHM = 0;
% % bidsSmoothing(funcFWHM, opt);
% %
% % subject level univariate
% bidsFFX('specifyAndEstimate', opt, funcFWHM);
% bidsFFX('contrasts', opt, funcFWHM);
% % prep for mvpa
% bidsConcatBetaTmaps(opt, funcFWHM, 0, 0);
%
% funcFWHM = 0;
% % bidsSmoothing(funcFWHM, opt);
%
% % subject level univariate
% bidsFFX('specifyAndEstimate', opt, funcFWHM);
% bidsFFX('contrasts', opt, funcFWHM);
%
% % prep for mvpa
% bidsConcatBetaTmaps(opt, funcFWHM, 0, 0);
%
% % strvcat(SPM.xX.name)
