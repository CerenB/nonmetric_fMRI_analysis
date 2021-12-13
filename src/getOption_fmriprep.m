% (C) Copyright 2020 Remi Gau, Marco Barilari

function opt = getOption_fmriprep()
  % opt = getOption()
  % returns a structure that contains the options chosen by the user to run
  % slice timing correction, pre-processing, FFX, RFX.

  if nargin < 1
    opt = [];
  end

  % group of subjects to analyze
  opt.groups = {''};
  % suject to run in each group
  opt.subjects = { '001'};

  % we stay in native space (that of the T1)
  % - in "native" space: don't do normalization
  % default in checkOptions is:
  % opt.space = {'individual', 'MNI'};
  % if you are only interested in one, you can choose as below:
%   opt.space = 'MNI';

  % The directory where the data are located
%   opt.dataDir = fullfile(fileparts(mfilename('fullpath')), ...
%                          '..', '..', '..',  'raw');

                     
  opt.dir.raw = fullfile(fileparts(mfilename('fullpath')), ...
                        '..', '..', '..', 'inputs', 'raw');
                    
  opt.dir.derivatives = fullfile(opt.dir.raw, '..', '..', 'outputs', 'derivatives');
  
%   
%   opt.dir.raw = fullfile(fileparts(mfilename('fullpath')), 'outputs', 'raw');
%   
%   opt.dir.derivatives = fullfile(opt.dir.raw, '..', 'derivatives');

  opt.pipeline.type = 'preproc';
  
  % task to analyze
  opt.taskName = 'visualLocalizer';
  
  opt.verbosity = 1;

%   opt.parallelize.do = false;
%   opt.parallelize.nbWorkers = 1;
%   opt.parallelize.killOnExit = true;

  %% DO NOT TOUCH
  opt = checkOptions(opt);
  saveOptions(opt);

end
