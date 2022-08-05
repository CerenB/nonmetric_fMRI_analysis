function omitSuffixOrganiseTsv(opt)
%small function to delete files suffix which were created with 
% CPP-BIDS - v2.1.0. 
% then it removed the empty rows/cells in tsv files

% add bids
bidsPath = '/Users/battal/Documents/GitHub/CPPLab/CPP_BIDS';
addpath(genpath(fullfile(bidsPath, 'src')));
addpath(genpath(fullfile(bidsPath, 'lib')));

% The directory where the data are located
for iSub = 1:length(opt.subjects)
    
    subject = ['sub-', opt.subjects{iSub}];
    session = ['ses-', opt.session];
    
    
    % remove data suffix
    removeAllDateSuffix(opt.dataDir, subject, session);
    
    
    % clean the tsv files from empty elements
    mainDir = fullfile(fileparts(mfilename('fullpath')), ...
        '..', '..', '..','..');
    taskNames = {'RhythmBlock'};
    
    organiseTsvFile(subject, session, mainDir, taskNames)
    
end
% remove cpp-bids repo now to prevent repos compatibility issues
rmpath(genpath(bidsPath));

end