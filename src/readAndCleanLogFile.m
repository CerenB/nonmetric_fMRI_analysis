function [cleanTbl, outputTag] = readAndCleanLogFile(tsv, option)

  tsvFile = tsv;

  % open file
  fid = fopen(tsvFile, 'r');

  % read first line - header
  tline = fgetl(fid);

  % extract the headers
  vaiablerNames = strsplit(tline, '\t');

  nVars = length(vaiablerNames);

  data = {};
  c = 1;

  % read line by line
  while ~feof(fid)

    tline = fgetl(fid);
    tmp = strsplit(tline, '\t');

    % get rid off any column more than headers
    if length(tmp) > nVars
      tmp = tmp(1:nVars);
    end

    if length(tmp) < nVars
      nNANs = nVars - length(tmp);
      tmp(end + 1:end + nNANs) = {'n/a'};
    end

    data(c, :) = tmp;

    c = c + 1;
  end

  fclose(fid);

  % assign it into a table
  tbl = cell2table(data);
  tbl.Properties.VariableNames = vaiablerNames;

  switch option

    %% remove all nans
    case 1
      % remove unnecessary columns
      cleanTbl = tbl;

      variableNames = cleanTbl.Properties.VariableNames;
      if ismember('keyName', variableNames)
        cleanTbl.keyName = [];
        cleanTbl.pressed = [];
        cleanTbl.target = [];
      end

      nRows = height(cleanTbl);
      nCols = width(cleanTbl);

      rows2del = false(nRows, 1);

      for iCol = 1:nCols

        rows2del = rows2del | strcmp(cleanTbl{:, iCol}, 'n/a');

      end

      % remove the rows with n/a
      cleanTbl(rows2del, :) = [];

      outputTag = '_removeNA.tsv';
      %         writetable(cleanTbl,'clean2_logfile', 'Delimiter','\t');
      %         movefile('clean2_logfile.txt','clean2_logfile.tsv');

      %% replace 'n/a' with actual nan
    case 2
      cleanTbl = tbl;

      nCols = width(tbl);

      for iCol = 1:nCols

        idx = strcmp(tbl{:, iCol}, 'n/a');

        if any(idx)
          cleanTbl{find(idx), iCol} = {'NaN'};
        end

        idy = strcmp(tbl{:, iCol}, '');

        if any(idy)
          cleanTbl{find(idx), iCol} = {'NaN'};
        end

      end

      strVarNames = {'trial_type', 'patternID', 'segmentCateg', 'keyName'};

      vaiablerNames = cleanTbl.Properties.VariableNames;

      for iVar = 1:length(vaiablerNames)
        if all(strcmp(vaiablerNames{iVar}, strVarNames) == false)
          cleanTbl.(vaiablerNames{iVar}) = str2double(cleanTbl.(vaiablerNames{iVar}));
        end
      end

      outputTag = '_addedNA.tsv';
      %         writetable(cleanTbl,'fixed_logfile', 'Delimiter','\t');
      %         movefile('fixed_logfile.txt','fixed_logfile.tsv');

  end
