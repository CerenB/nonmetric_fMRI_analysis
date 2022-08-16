clear all;

%% set paths
% set spm
warning('off');
addpath(genpath('/Users/battal/Documents/MATLAB/spm12'));

% add cpp repo
run ../lib/CPP_SPM/initCppSpm.m;

% read subject info
opt = getOptionNonmetric();
 
%% set input
% radius of the sphere
radius = 10;
                 
% Grahn 2007 study MNI-space coordinates                    
seedCoordinates = [
   -9	6	60   % Left  pre-SMA/SMA
   3	6	66   % Right pre-SMA/SMA
   -54	0	51   % Left premotor
   54	0	45   % Right premotor
   -24	6	9    % Left  putamen
   21	6	9    % Right putamen
   -30	-66	-24  % Left cerebellum
   30	-66	-27  % Right cerebellum
   -57	-15	9    % Left STG
   60	-33	6    % Right STG
     ];
 
    
roiNames = {'lSMA','rSMA','lpreM','rpreM','lputa','rputa', ...
            'lcereb', 'rcereb', 'lSTG', 'rSTG'};

rois2Use = 1:10;
seedCoordinates = seedCoordinates(rois2Use,:);
roiNames = roiNames(rois2Use);

%% set output

outputDir = [opt.roiDir,'/grahn2007'];
cd(outputDir);

%% for each ROI, make a .nii image

for iRoi = 1:size(seedCoordinates,1)
        
    roiCenter = seedCoordinates(iRoi,:);
    
    if ~isnan(roiCenter(1))
        
        % Create ROI to be in marsbar format
        tmpRoiName = [roiNames{iRoi}, ...
            '_sphere-', num2str(radius), ...
            'mm_x-', num2str(roiCenter(1)), ...
            '_y-', num2str(roiCenter(2)), ...
            '_z-', num2str(roiCenter(3))] ;  %ROI name
        
        params = struct('centre', roiCenter , 'radius', radius);
        
        roi = maroi_sphere(params);
        
        saveroi(roi, [tmpRoiName,'.mat']);
        mars_rois2img([tmpRoiName,'.mat'],[tmpRoiName,'.nii'])
        
        % delete ROI files
        delete([tmpRoiName,'_labels.mat'])
    end
end