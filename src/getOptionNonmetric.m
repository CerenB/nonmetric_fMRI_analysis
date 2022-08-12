% (C) Copyright 2020 Remi Gau, Marco Barilari

function opt = getOptionNonmetric()
  % opt = getOption()
  % returns a structure that contains the options chosen by the user to run
  % slice timing correction, pre-processing, FFX, RFX.

  if nargin < 1
    opt = [];
  end

  % group of subjects to analyze
  opt.groups = {''};
  opt.session = '001';
  % suject to run in each group
  opt.subjects = { '013', '014', '015', '016', '017', ...
                  '018', '019', '020', '021', '023', ...
                  '024', '025', '026', '027', '028', ...
                  '029', '030', '031', '032', '033'}; 
  
  % 031 == 023, 032 == 007
  % '024','025','026', '027','028' % 28 == 21
  
  % '013', '014', '015', '016', '017', ...
%                   '018', '019', '020', '021', '023'

  % Uncomment the lines below to run preprocessing
  % - don't use realign and unwarp
  opt.realign.useUnwarp = true;

  % we stay in native space (that of the T1)
  % - in "native" space: don't do normalization
  opt.space = 'MNI'; % 'individual', 'MNI'

  % The directory where the data are located
  opt.dataDir = fullfile(fileparts(mfilename('fullpath')), ...
                         '..', '..', '..',  'raw');
  opt.derivativesDir = fullfile(opt.dataDir, '..');

  % task to analyze
  opt.taskName = 'Nonmetric';

  %% set paths
  [~, hostname] = system('hostname');
  if strcmp(deblank(hostname), 'tux')
    opt.dataDir = fullfile('/datadisk/data/RhythmCateg-fMRI/Nonmetric');
    opt.derivativesDir = fullfile( ...
                                  '/datadisk/data/RhythmCateg-fMRI/Nonmetric', ...
                                  'cpp_spm');
  elseif strcmp(deblank(hostname), 'mac-114-168.local')
    % The directory where the data are located
    opt.dataDir = fullfile(fileparts(mfilename('fullpath')), ...
                           '..', '..', '..', 'raw');
    opt.derivativesDir = fullfile(opt.dataDir, '..', ...
                                  'derivatives', 'cpp_spm');

    opt.roiDir = fullfile(fileparts(mfilename('fullpath')),  ...
                          '..', '..', '..', '..', 'RhythmCateg_ROI');
  end

  % Suffix output directory for the saved jobs
  opt.jobsDir = fullfile( ...
                         opt.dataDir, '..', 'derivatives', ...
                         'cpp_spm', 'JOBS', opt.taskName);

  % specify the model file that contains the contrasts to compute
  % univariate
  opt.model.file =  ...
      fullfile(fileparts(mfilename('fullpath')), '..', ...
               'model', 'model-Nonmetric_smdl.json');
%   opt.model.file =  ...
%       fullfile(fileparts(mfilename('fullpath')), '..', ...
%                'model', 'model-Nonmetric-OnlyPitch_smdl.json');


  %% Specify the result to compute
  opt.result.Steps(1) = returnDefaultResultsStructure();

  opt.result.Steps(1).Level = 'subject';

  opt.result.Steps(1).Contrasts(1).Name = 'AllCateg';
  opt.result.Steps(1).Contrasts(1).MC =  'none';
  opt.result.Steps(1).Contrasts(1).p = 0.001;
  opt.result.Steps(1).Contrasts(1).k = 0;

  % For each contrats, you can adapt:
  %  - voxel level (p)
  %  - cluster (k) level threshold
  %  - type of multiple comparison (MC):
  %    - 'FWE' is the defaut
  %    - 'FDR'
  %    - 'none'
  %
  % not working for multiple contrasts
  opt.result.Steps(1).Contrasts(2).Name = 'CategA_gt_CategB';
  opt.result.Steps(1).Contrasts(2).MC =  'none';
  opt.result.Steps(1).Contrasts(2).p = 0.001;
  opt.result.Steps(1).Contrasts(2).k = 0;
  %
  opt.result.Steps(1).Contrasts(3).Name = 'CategB_gt_CategA';
  opt.result.Steps(1).Contrasts(3).MC =  'none';
  opt.result.Steps(1).Contrasts(3).p = 0.001;
  opt.result.Steps(1).Contrasts(3).k = 0;
  
  opt.result.Steps(1).Contrasts(4).Name = 'Nonmetric';
  opt.result.Steps(1).Contrasts(4).MC =  'none';
  opt.result.Steps(1).Contrasts(4).p = 0.001;
  opt.result.Steps(1).Contrasts(4).k = 0;
  
  opt.result.Steps(1).Contrasts(5).Name = 'Simple';
  opt.result.Steps(1).Contrasts(5).MC =  'none';
  opt.result.Steps(1).Contrasts(5).p = 0.001;
  opt.result.Steps(1).Contrasts(5).k = 0;

  % Specify how you want your output (all the following are on false by default)
  opt.result.Steps(1).Output.png = true();

  opt.result.Steps(1).Output.csv = true();

  opt.result.Steps(1).Output.thresh_spm = true();

  opt.result.Steps(1).Output.binary = true();

  opt.result.Steps(1).Output.montage.do = true();
  opt.result.Steps(1).Output.montage.slices = 40:5:60; % in mm -8:3:15; % -12:4:60
  % axial is default 'sagittal', 'coronal'
  opt.result.Steps(1).Output.montage.orientation = 'axial';

  % will use the MNI T1 template by default but the underlay image can be
  % changed.
  opt.result.Steps(1).Output.montage.background = ...
      fullfile(spm('dir'), 'canonical', 'avg152T1.nii,1');

  %   opt.result.Steps(1).Output.NIDM_results = true();

  % Options for slice time correction

  opt.sliceOrder = [];

  opt.STC_referenceSlice = [];

  % Options for normalize
  % Voxel dimensions for resampling at normalization of functional data or leave empty [ ].
  opt.funcVoxelDims = [2.6 2.6 2.6];

  opt.parallelize.do = false;
  opt.parallelize.nbWorkers = 1;
  opt.parallelize.killOnExit = true;

  %% DO NOT TOUCH
  opt = checkOptions(opt);
  saveOptions(opt);

end
