clear all;

%% set paths
% set spm
warning('off');
addpath(genpath('/Users/battal/Documents/MATLAB/spm12'));

% add cpp repo
run ../lib/CPP_SPM/initCppSpm.m;

% start marsbar
marsbar('on');

% read subject info
opt = getOptionNonmetric();

% load the subjects/Groups information and the task name
[BIDS, opt] = getData(opt);
fprintf(1,'PROCESSING TASK: %s \n',opt.taskName)
  
%% set input
% Degree of first level smoothing
firstLevelSmoothing = 6 ;

% radius of the sphere
radius = 5;
                 
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

% tapping info
goodTapper = [1,2,5,7,8,9,10,12,14,15,16,21,25,27,28,30,32,33];
% badTapper = [3,4,6,11,13,17,18,19,20,23,24,26,29,31];

%% set output

outputDir = [opt.derivativesDir,'-stats/group/task-',...
                                opt.taskName, ...
                                '_space-', opt.space, ...
                                '_FWHM-', num2str(firstLevelSmoothing), ...
                                '_MarsBar_roi'];
if ~exist(outputDir,'dir')
    mkdir(outputDir)
end

cd(outputDir);
outputNameMat = fullfile(outputDir,...
                    [opt.taskName,...
                    '_GrahnCoord_Beta_GroupROIs_Smoothing',...
                    num2str(firstLevelSmoothing),...
                    '_Sphere',num2str(radius),'mm_', ...
                    datestr(now, 'yyyymmddHHMM'), '.mat']);
outputNameCsv = fullfile(outputDir,...
                    [opt.taskName,...
                    '_GrahnCoord_Beta_GroupROIs_Smoothing',...
                    num2str(firstLevelSmoothing),...
                    '_Sphere',num2str(radius),'mm_', ...
                    datestr(now, 'yyyymmddHHMM'),'.csv']);              
                
% mat struc to save
result = struct('sub',[],'subLabel', [], ...
                'simpleBeta',[], 'nonmetricBeta', [], ...
                'roi', [], 'roiCoord', [], ...
                'indexS', [], 'indexN', [], ...
                'expType', []);           
                



%% let' start
%count all the indices
c = 1; 

% for each ROI
for iSub = 1:length(opt.subjects) % for each subject
    
    subNumber = opt.subjects{iSub};   % Get the Subject ID

    %save experiment type info
    expType = 'no_pitch';
    expLabel = 2; %second exp(control)

    if str2double(subNumber)<=23
        expType = 'pitch_change';
        expLabel = 1;
    end
    
    subType = 'badTapper';
    subTypeLabel = 0;
    % save good/bad tapper info
    if ismember(str2double(subNumber),goodTapper)
        subType = 'goodTapper';
        subTypeLabel = 1;
    end
        
    fprintf(1,'PROCESSING SUBJECT No.: %i SUBJECT ID : %s \n',iSub,subNumber)
    
    for iRoi = 1:size(seedCoordinates,1)
        
        ffxDir = getFFXdir(subNumber, firstLevelSmoothing, opt);
        
        %ROI_center = IndividCoord(iSub,:,iROI) ;
        roiCenter = seedCoordinates(iRoi,:) ;
        
        if ~isnan(roiCenter(1))
            
            %% Create ROI to be in marsbar format
            tmpRoiName = 'VR' ;  % temporary ROI name

            params = struct('centre', roiCenter , 'radius', radius);
            
            roi = maroi_sphere(params);
            
            saveroi(roi, [tmpRoiName,'.mat']);
            mars_rois2img([tmpRoiName,'.mat'],[tmpRoiName,'.nii'])
            
            %% Beta extraction using marsbar
            
            spmFile = fullfile(ffxDir,'SPM.mat');
            % load spm file
            load(spmFile);
            
            roiFile = fullfile(outputDir,[tmpRoiName,'.mat']);

            % Make marsbar design object
            D  = mardo(spmFile);
            D  = mardo(SPM);
            
            % Set fmristat AR modelling
            % http://marsbar.sourceforge.net/faq.html#fmristat
            % https://sourceforge.net/p/marsbar/mailman/message/19644529/
            D = autocorr(D, 'fmristat', 2);  % to correct for error when estimating the model
           
            % Make marsbar ROI object
            R  = maroi(roiFile);
            % Fetch data into marsbar data object
            Y  = get_marsy(R, D, 'mean');
            % Get contrasts from original design
            xCon = get_contrasts(D);
            % Estimate design on ROI data
            E = estimate(D, Y);
            % Put contrasts from original design back into design object
            E = set_contrasts(E, xCon);
            % get design betas
            b = betas(E);
         
            % find which betas to extract
            C = zeros(length(xCon),size(SPM.xX.X,2));
            % find all the motion regressors/betas
            for ii = 1:size(SPM.xX.X,2)
                if strfind(SPM.xX.name{ii},'block_simple*bf(1)')
                    C(1,ii) = 1;
                elseif strfind(SPM.xX.name{ii},'block_nonmetric*bf(1)')
                    C(2,ii) = 1; 
                end

            end
            
            % loop through all the betas to mean them
            indexBeta1 = find(C(1,:)==1);
            indexBeta2 = find(C(2,:)==1);
            runNumber = length(indexBeta1);
            beta1 = mean(b(indexBeta1));
            beta2 = mean(b(indexBeta2));
            
            result(c).sub = iSub;
            result(c).subLabel = ['sub-', subNumber];
            result(c).simpleBeta = beta1;
            result(c).nonmetricBeta = beta2;
            result(c).roi = roiNames{iRoi};
            result(c).roiCoord = seedCoordinates(iRoi, :);
            result(c).indexS = indexBeta1;
            result(c).indexN = indexBeta2;
            result(c).expType = expType;
            result(c).expLabel = expLabel;
            result(c).subType = subType;
            result(c).subTypeLabel = subTypeLabel;
            
            % see which betas to extract 
            BETAS1(iSub,iRoi) = beta1;     % b(1) = simple
            BETAS2(iSub,iRoi) = beta2;     % b(2) = complex/nonmetric
            
            
            % delete ROI files
            delete([tmpRoiName,'_labels.mat'])
            delete([tmpRoiName,'.nii'])
            delete([tmpRoiName,'.mat'])
            
        else
            BETAS1(iSub,iRoi) = nan;     
            BETAS2(iSub,iRoi) = nan;     
        end
        
        c = c +1;
    end
end



% average the betas 
nMeanBETAS1(1,:) = nanmean(BETAS1);
nMeanBETAS2(1,:) = nanmean(BETAS2);

% save results as .mat
save(outputNameMat, 'result','BETAS1', 'BETAS2', 'nMeanBETAS1', 'nMeanBETAS2')

% save the result as .csv
csvResult = rmfield(result, 'indexS');
csvResult = rmfield(csvResult, 'indexN');
csvResult = rmfield(csvResult, 'roiCoord');
writetable(struct2table(csvResult), outputNameCsv);

%bar(x')
%ylim([-0.4 0.8])
%set(gca,'XTickLabel',{'lV5-Vis','rV5-Vis','lV5-Aud','rV5-Aud','lPT','rPT'})

