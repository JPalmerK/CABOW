% Process the MD filed data!

%% Clean up the workspace
close all; clear all; clc

% File located- C:\BitBucketRepositories\CABOWProcessing\Matlab
% File backup-

%% Step 1 Directory for the file locations
% 1) Cliptable directory (right whale upcall scores and bearing estimate
% 2) Edge table
% 3) Directory to source level (e.g. soundtrap) recording file locations
% 4) Meta, playbackk time and location (from ship GPS)
% 5) Cabow meta

% Head Directory for the files
headDir = 'C:\Users\Kaitlin Palmer\Desktop\MD Field Data\MDOFFSHORE';

% Directory for the source recording files
receivedRecordings =   dir('E:\Data\CABOW ID\pb321\recs\');




% 1/2) Clip and edge table- Output from PAMGuard
% Read in and define the timezone for the timestamp
clipTable1 =readtable('C:\Data\MD Field Trials\ReAnalysis\ClassifierNoiseUpcall_202202230254TRIAL1.csv');
clipTable1.UTC= clipTable1.UTC-seconds(11058405.559); % values from zoom convo


clipTable2 =readtable('C:\Data\MD Field Trials\ReAnalysis\ClassifierNoiseUpcall_20220255133_pb319.csv');
clipTable2.UTC= clipTable2.UTC-seconds(11504724.296);
clipTable2.processChannel= clipTable2.processChannel*0+4096;
clipTable = [clipTable1;clipTable2];
clipTable.UTC.TimeZone = 'UTC';




edgeTable1 = readtable('C:\Data\MD Field Trials\ReAnalysis\RightWhale_202202230254TRIAL1.csv');
edgeTable2 = readtable('C:\Data\MD Field Trials\ReAnalysis\RightWhale_202202251333_pb319.csv');
% adjust date
edgeTable1.UTC= edgeTable1.UTC-seconds(11058405.559);
edgeTable2.UTC= edgeTable2.UTC-seconds(11504724.296);

edgeTable1.ChannelBitmap= edgeTable1.ChannelBitmap*0+448;
edgeTable2.ChannelBitmap= edgeTable2.ChannelBitmap*0+28672;

edgeTable=[edgeTable1; edgeTable2];
edgeTable.UTC.TimeZone = 'UTC';
edgeTable=edgeTable(~isnan(edgeTable.UID),:);

clear clipTable1 clipTable2 edgeTable1 edgeTable2


% 3) Soundtrap playback files. Assume all files contain one full (30 clips)
% playback
sourceRecordingFiles = ...
    dir(fullfile(headDir, ...
    'playbacks_subset\SL_recordings\671129639\*wav*'));


% Soundtrap calibration constant
SoundtrapCal = -176; % unit 5039 high gain
SoundtrapCal =-176.2; % unit 671129639 high gain

% 4) Where the calls were played back
playbackLatLonDir =dir(('C:\Users\Kaitlin Palmer\Desktop\MD Field Data\MDOFFSHORE\Engineering Database\CSV exports\*.csv'));
playbackLatLon = readtable(fullfile(playbackLatLonDir(end).folder,...
    playbackLatLonDir(end).name));
playbackLatLon.UTC.TimeZone='UTC'; % Set the timezone

% 5) Cabow information- locations of the CABOWs and linking information from
% CABOW ID to channel bitmap (which is actually process channel? Don't ask
% me)
CabowLat =[38.21400948,38.19433124, 38.17251151, 38.15068512,38.13098957]';
CabowLon =[-74.92137854, -74.93413806, -74.93853488, -74.93413858, -74.92137912]';
Depth =[-23, -22, -21, -21, -25]';
CabowID = [324 325 321 327 319]';
processChannel = [1 8 64 512 4096]'; % This refers to the buoy number (total = 5)
ChannelBitmap =[7 56 448 3584 28672]'; % This refers to the hydrophone on the buoy for edge etections (total = 15)
E2E =[159.6 160.7 159.8 160.4 160.1]'; % end to end calibration constant Update!!!!
BearingOff =[88 -69 -142.8 -126 -81.7]'; % Bearing offset, enter if known

% Create a table with the CABOW information
CABOWTable = table(CabowLat, CabowLon, Depth,...
    CabowID, processChannel,ChannelBitmap, BearingOff, E2E);


% which files have been loaded?
[~, idxKeep, ~]=intersect(CABOWTable.processChannel,unique(clipTable.processChannel), 'stable')
CABOWTable=CABOWTable(idxKeep,:);

% Merge the CABOW table and the clip table by Process Channel
% (Channelbitmap is empty)
clipTable= join(clipTable, CABOWTable(:,[1:5,7]));

% Merge he CABOW table and the Edge table by ChannelBitmap
edgeTable = join(edgeTable, CABOWTable);

% Housekeeping
clear CabowLat CabowLon Depth CabowID processChannel E2E BearingOff clipTableDir...
    edgeTableDir ChannelBitmap processChannel E2E BearingOff playbackLatLonDir

%% Step 2) - Merge the clip table and the edge detection table

% appears there is a one second offset between the utc in the edge detector
% and the utc in the clip generator (whyyyyyyyyyyy? and more importantly is
% this consistant? Can we count oan a one second delay always?)
edgeTable.UTC =edgeTable.UTC -seconds(1);
clipTable.BearingEst = nan(height(clipTable),1);

for ii = 1:height(clipTable)

 EdgetableSub = edgeTable(edgeTable.CabowID==clipTable.CabowID(ii), :);
 [minTimediff(ii), idx] = min(abs(clipTable.UTC(ii)-EdgetableSub.UTC));
 
 if minTimediff(ii)<seconds(5)
  clipTable.BearingEst(ii) = EdgetableSub.Angle_0(idx);
 end

end


% Pull out the TP clicks and plot them
TPclips = clipTable(clipTable.Score>.8,:);

% Inspect the data
scatter(TPclips.RMSsignal-TPclips.RMSnoise, TPclips.BearingEst*(180/pi),...
    [], TPclips.CabowID, 'filled')
xlabel('Clip Number'); ylabel('Bearing')

% Housekeeping
clear TPclips idx  minTimediff ii

%% Step 2 Get upcall playback timing (manually) using the peak picker fx

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This step must be done manually and adjusted for each playback file. This
% needs to be done for every playback
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Define output SL table
SLtable  = [];

% Start at 5, first two (or three) orientation playbacks were not analyzed
% for detection/non detection


for jj = 1:length(sourceRecordingFiles)
    
    % Load the wave playback file
    [Sourceyy, fs1] = audioread(fullfile(sourceRecordingFiles(jj).folder,...
        sourceRecordingFiles(jj).name));
    
    % File date (refrencing from the end works for both soundtrap file names)
    fDate = datetime(sourceRecordingFiles(jj).name(end-15:end-4),'InputFormat','yyyyMMddHHmmss');
    fDate.TimeZone='UTC';
    
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Look at peaks and see if they are correct, once the correct
    % combination of exeptions (remove peaks manually, change trigger amplitude,
    % restrict samples etc. place the 'Bespoke' exception in the list below
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    
    %     % Display peaks to determine if they are correct
    %     findpeaks(Sourceyy(fs1*5:end),  'MinPeakDistance',...
    %         fs1*7,'MinPeakHeight',.25);
    
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %If peaks aren't right add exceptions here
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    
    % Find the 30 detections by looking for gaps larger than 7 seconds
    try
        [peaks, locs] = findpeaks(Sourceyy(fs1:end),  'MinPeakDistance',...
            fs1*7,'MinPeakHeight',.16);
        if length(locs)>60
             disp('multFiles')
%             peaks=[]; locs=[];
            
            if jj==4
                
                peaks= peaks(2:end); locs=locs(2:end);
            end
            
            if jj == 5
                 peaks= peaks(4:end-4); locs=locs(4:end-4);
            end
            
            if jj == 6
                  peaks =peaks([3:33, 35:end-2]);  locs =locs([3:33, 35:end-2]);
            end
            if jj == 79
               
                peaks= peaks(2:end-1); locs=locs(2:end-1);
            end
            
            if jj == 103
                
                peaks= peaks(3:end-1); locs=locs(3:end-1);
            end
            
            if jj==105
                peaks = peaks([2:31,33:end-1]);locs = locs([2:31,33:end-1]);
            end
            
            if jj==116
                peaks = peaks([2:31,34:end]);locs = locs([2:31,34:end]);
            end
            
        else
            
            diffTimes = diff(locs/fs1);
            keepIdx = find(diffTimes>7 & diffTimes<8.2);
            
            
            keepIdx = [keepIdx; max(keepIdx)+1];
            
            peaks = peaks(keepIdx); locs=locs(keepIdx);
            
        end
    catch
        disp('bad file')
        
        peaks=[]; locs=[];
    end
    
    
    
    
    % Start and stop of the locs (always chop the first second)
    peakStart = locs-(.4*fs1)+fs1;
    peakStop = locs+(.4*fs1)+fs1;
    
    % Bandpass filter the upcalls with the same process as is running in
    % pamguard
    filteredYY = bandpass(Sourceyy, [50 225], fs1,'ImpulseResponse','iir','Steepness',0.5);
    
    % Iterate through each of the peaks and calculate the source level
    SL =zeros(size(peakStart));
    for ii =1:length(peakStart)
        SL(ii) = rms(filteredYY(peakStart(ii):peakStop(ii)));
    end
    
    % All call types are simulated
    callType =repmat({'Simulated'}, [length(SL),1]);
    
    % Matlab date for the call time
    matlabDate = fDate+round(seconds((locs+fs1)/(fs1)));
    
    % Link the observed matlab times to the playback file
    playbackLatLon.UTC.Second= round(playbackLatLon.UTC.Second);
    
    % Figure out which calls have playbacks
    idxKeep =find(ismember(matlabDate, playbackLatLon.UTC));
    
    SL = SL(idxKeep);
    
    % Get the index of the matching time
    matchIdx =find(ismember(playbackLatLon.UTC,matlabDate));
    
    
    % Variable for the playback ID
    lat=playbackLatLon.Latitude(matchIdx) ; % Source lat
    lon = playbackLatLon.Longitude(matchIdx); % Source lon
    
    % Have the time and location of the played calls now calculate
    % sourcelevel and create variables for the call number, playback depth,
    % and playback id
    SLdB = (20*log10(SL))-SoundtrapCal; % Calib Source Level
    PlaybackId = ones(size(lon))*jj; % Playback ID
    callNum = (1:length(lon))'; % call number within the playback id
    depth = ones(size(lat))*8; % Depth of the playback transducer
    matlabDate=(matlabDate(idxKeep));
    callType= callType(idxKeep);
    SLtable  = [SLtable; table(SLdB, matlabDate, callType,...
        lat, lon, PlaybackId, depth, callNum)];
    
end


%% Calculate the distance and bearing from the playbacks to all five CABOW units
% Clip table updates done

% Duplicate this dataframe so there is a playback for each CABOW
SLtableOut =[];
for ii = 1:height(CABOWTable)
    
    SLtable.CABOWid = repmat(CABOWTable.CabowID(ii), [height(SLtable),1]);
    SLtable.bearingOfset=repmat(CABOWTable.BearingOff(ii), [height(SLtable),1]);
    SLtable.CABOWlat=repmat(CABOWTable.CabowLat(ii), [height(SLtable),1]);
    SLtable.CABOWlon=repmat(CABOWTable.CabowLon(ii), [height(SLtable),1]);
    SLtableOut=[SLtableOut; SLtable];
    
    
end
%     True range and bearing between all clips and each received clip with
%     a bearing estimate
[range, a12, a21]  = vdist2(...
    SLtableOut.CABOWlat,...
    SLtableOut.CABOWlon,...
    SLtableOut.lat, SLtableOut.lon);

% True range and bearing between CABOW and playback vessle
SLtableOut.range = range;
SLtableOut.Truebearing= a12; % True bearing 0:360 DEG
SLtableOut.ExpArrival = SLtableOut.matlabDate+seconds(SLtableOut.range/1462);

% housekeeping
SLtable = SLtableOut; clear SLtableOut



%% Link the Source Level Table to the Clip Table with clip measurements
% This links the annotations, detector scores, rms measurements, and CABOW
% metrics to the calls that were actually played back. Unfortunately has to
% be guesed based on time

clipTable.Upcall(clipTable.Score>.8)=1;
clipTable.FP = ones(height(clipTable),1);

% temporary variable for cleaning it out
SLtable.RecRMSnoise = nan(height(SLtable),1);
SLtable.RecRMSsignal = nan(height(SLtable),1);
SLtable.BearingEst = nan(height(SLtable),1);
SLtable.detected = zeros(height(SLtable),1);
SLtable.Score = nan(height(SLtable),1);
SLtable.id = nan(height(SLtable),1);
SLtable.zone= nan(height(SLtable),1);


for ii=1:height(SLtable)
    
    % Find the minimum time between the Source level playback time and the

    clipIdx = find(clipTable.CabowID==SLtable.CABOWid(ii));
    
    [minTimediff, idx] = min(abs(...
        clipTable.UTC(clipIdx)-SLtable.ExpArrival(ii)));

    
    %
    if minTimediff <= seconds(5)
        SLtable.RecRMSnoise(ii)=clipTable.RMSnoise(clipIdx(idx));
        SLtable.RecRMSsignal(ii)=clipTable.RMSsignal(clipIdx(idx));
        SLtable.BearingEst(ii) = clipTable.BearingEst(clipIdx(idx));
        SLtable.detected(ii)=clipTable.Upcall(clipIdx(idx));
        SLtable.zone(ii) = clipTable.Zone(clipIdx(idx));
        SLtable.id(ii)=clipTable.Id(clipIdx(idx));
        SLtable.Score(ii)=clipTable.Score(clipIdx(idx));
        clipTable.FP(clipIdx(idx),:)= 0;
        
        
    else
        disp('blarg!')
    end
    
    
end

% Clips before or after the start of the trials don't count as false
% positives

clipTable.FP(clipTable.UTC<min(SLtable.matlabDate))=0;
clipTable.FP(clipTable.UTC>max(SLtable.matlabDate))=0;

% Time period ok for analysis, as above
clipTable.TPOk = (clipTable.UTC>=min(SLtable.matlabDate) &...
    clipTable.UTC<=max(SLtable.matlabDate)) 

FP = sum( clipTable.TPOk .* clipTable.FP)

%% Get the ambient noise level for the missed detections

% Get all CABOW folders
allCABOWloc = struct2table(receivedRecordings(3:end));
fs = 2000;

% Step through each unit and get the filder list
badIDX=[];

for ii =1:height(allCABOWloc)
    
    CABOWsounds = dir(fullfile(allCABOWloc.folder{ii}, allCABOWloc.name{ii}, '**\*wav'))
    CABOWsounds = struct2table(CABOWsounds);
    CABOWsounds.MatlabDate = cell2mat(vertcat(cellfun(@(x) datenum(x(end-22:end-4),...
        'yyyymmdd_HHMMSS_fff'),CABOWsounds.name, 'UniformOutput', false)));
    
    CABOWsounds.StartDate =...
        cellfun(@(x) datetime(x(end-22:end-4),...
        'InputFormat','yyyyMMdd_HHmmss_SSS'),CABOWsounds.name);
    
    CABOWsounds.HumanReadable = datestr(CABOWsounds.MatlabDate, 'yyyymmdd_HHMMSS');
    CABOWsounds = sortrows(CABOWsounds,'MatlabDate','ascend');
    CABOWsounds.StartDate.TimeZone = 'UTC';
    
    CABOW_e2e = CABOWTable.E2E(CABOWTable.CabowID==str2num(CABOWsounds.name{1}(1:3)) )
    
    % Get the index of the sound files for that CABOW
    CABOWIndices=find(SLtable.CABOWid== str2num(CABOWsounds.name{1}(1:3))...
        & isnan( SLtable.RecRMSnoise))';
    
    % Step throught he indicies and get the source level
    for  jj = CABOWIndices
        
        try
            fileIdx = find(CABOWsounds.StartDate<...
                (SLtable.ExpArrival(jj)-seconds(1)),1,'last');
            finfo = audioinfo(fullfile(CABOWsounds.folder{fileIdx},CABOWsounds.name{fileIdx}));
            
            
            % Time difference to identify the samples
            aa = (CABOWsounds.StartDate(fileIdx) + seconds(finfo.Duration))...
                -(SLtable.ExpArrival(jj)-seconds(1));
            startSec = seconds(finfo.Duration)-aa;
            startSamp = seconds(round(startSec*fs));
            stopSamp = startSamp+fs;
            
            if stopSamp>finfo.TotalSamples
                
                % load the audio
                samps = audioread(fullfile(CABOWsounds.folder{fileIdx},...
                    CABOWsounds.name{fileIdx}),...
                    [startSamp finfo.TotalSamples]);
                samps = [samps;...
                    audioread(fullfile(CABOWsounds.folder{fileIdx+1},...
                    CABOWsounds.name{fileIdx+1}),...
                    [1 fs-length(samps)])];
            else
                
                % read in the audio segment
                samps = audioread(...
                    fullfile(CABOWsounds.folder{fileIdx},CABOWsounds.name{fileIdx}),...
                    [startSamp stopSamp]);
                
            end
            % Filster over noise band
            samps = samps(:,1);
            samps= bandpass(samps,[50 225],fs);
            
            
            movrms = 20*log10(sqrt(movmean(samps .^ 2, fs/4)))+CABOW_e2e;
            %     plot(movrms)
            

            SLtable.RecRMSnoise(jj) = min(movrms);
             SLtable.RecRMSsignal(jj) = median(movrms);
            
        catch
            badIDX = [badIDX, ii];
        end
    end
    
end




% housekeeping
clear allCABOWloc receivedTableLoc CABOWsounds CABOW_e2e movrms samps aa ...
    startSec stopSamp stopSamp

%% Histogram of data collected thus far

% Estimate of the signal excess for plotting


SLE =SLtable.SLdB-SLtable.RecRMSnoise;
% SLE(isnan(SLE)) = SLtable.SLdB(isnan(SLE))-median(SLtable.RecRMSnoise, 'omitnan');
% 
% % Estimate noise level from nearest detection for nan values
%SLtable.RecRMSnoise = estimateNL(SLtable, clipTable);

% First set of orientation playbacks not annotated
SLtable.detected(SLtable.PlaybackId<4 & SLtable.Score>.75)=1;

figure; histogram2(SLtable.range/1000,SLE)

xlabel('Range (km)'); ylabel('Signal Excess (dB)');zlabel('Counts')

%% Create the p(r|SLNL)- this is approximate because we don't have the noise
% level from the CABOW at the time when calls were not detected. Therefore
% we are just estimating the ambient noise level from the clips that DID
% arrive (anotated or not)

SLtable.detected(isnan(SLtable.detected))=0;
SLtable.detected(SLtable.zone ==1)=1;
SLNRbins =20:5: 75;
RangeBins =(0:500:10000);


nPlayedr=[];
nDetr = [];
figure(14)

for ii = 1:length(SLNRbins)-1
    
    dataSubSNR = SLtable(find(SLE>SLNRbins(ii) &...
        SLE<=SLNRbins(ii+1)),:);
    
    for jj=1:length(RangeBins)-1
        dataSub = dataSubSNR(dataSubSNR.range>RangeBins(jj) &...
            dataSubSNR.range<=RangeBins(jj+1),:);
        
        
        nPlayedr(ii,jj) = height(dataSub);
        nDetr(ii,jj) = sum(dataSub.detected);
        
        
    end
    
end

% Make the SLNL plot
x= repmat(1:size(nDetr,2),[(size(nDetr,1)),1]);
y= repmat(1:size(nDetr,1),[(size(nDetr,2)),1])';

pcolor(RangeBins(1:end-1),SLNRbins(1:end-1), nDetr./nPlayedr)
ylabel('SLNL'); xlabel('Range (m)'); colorbar

%% Figure out the bearing error

SLtable.deg= SLtable.BearingEst*(180/pi);
SLtable = movevars(SLtable, 'BearingEst', 'Before', 'id');
SLtable = movevars(SLtable, 'BearingEst', 'After', 'deg');
SLtable = movevars(SLtable, 'deg', 'Before', 'id');
SLtable = movevars(SLtable, 'Truebearing', 'Before', 'Score');
SLtable = movevars(SLtable, 'Truebearing', 'Before', 'id');
SLtable.deg(SLtable.deg<0)=360+SLtable.deg(SLtable.deg<0)
SLtable.newEst = SLtable.deg+SLtable.bearingOfset;
SLtable.newEst(SLtable.newEst<0)=SLtable.newEst(SLtable.newEst<0)+360;
SLtable.BearingError = SLtable.newEst-SLtable.Truebearing;
SLtable.BearingError (SLtable.BearingError >180)= mod(SLtable.BearingError (SLtable.BearingError >180),180)
SLtable.BearingError (SLtable.BearingError <-180)= mod(SLtable.BearingError (SLtable.BearingError <-180),180)
SLtable.SLNL =SLE;

hist(SLtable.BearingError, 500)
writetable(SLtable, 'C:\BitBucketRepositories\CABOWProcessing\ProcessedCSVfiles\MDtrialsRevision321.csv')