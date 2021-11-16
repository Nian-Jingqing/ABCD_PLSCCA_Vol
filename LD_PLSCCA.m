%PLS AND CCA ANALYSIS WITH BIFACTOR AND GMV

% root directory
root_dir = '/Users/leighton/Desktop/PLS_CCA_Updated/PLSCCA Analyses';

%Read in residuals (change file based on particular analysis)
Residuals = readtable('ABCD_PLSCCA_Residuals_PrimaryGroup1.csv');
Residuals = table2cell(Residuals);
Residuals = cell2mat(Residuals);

%define bifactor residuals and brain vol residuals
brain = Residuals(:,[3:89]);
bifactor = Residuals(:,[90:93]);
weight = Residuals(:,[2]);

% Add path to where PLS code is located
addpath = ('/Users/leighton/Desktop/Desktop/PLS_CCA_Updated/PLSCCA/WeightedPLS-main/plscmd');

% Choose path for results
result_path = [root_dir filesep 'PLSCCA_']; % Where your results will be saved.

%% Info about the dataset 
model_name = 'Sensitivity'; % this will be the main name for the output files
result_file_prefix = 'CCA'; % prefix for the outpuy that denotes what type of analysis this is
beh_variables = bifactor; % behavior matrix, accuracy data, order must match datamat_list

% Assigning BehavLabels for visualization purposes
BehavLabels =  {'Gen'; 'Ext'; 'ADHD'; 'Int'}; % This should be an array {'BehavVar1'; ...etc} so the LVs will show the right info. 

% Specify number of conditions and number of subjects (this is to separate the matrix you are feeding in, 
%the number of rows should equal numconds*num_subj
numconds = 1; % for pls code to know how data is structured
s = size(bifactor);
num_subj = s(1); % change if needed

%% Enter data matrix here: should be one row per subject, vertically concatenated by condition
% In the same order as in the 'conditions' variable above. 
datamat_lst{1} = brain; 
% Note: this is only for a design with 1 group. 

%% PLS Options: (shouldn't need to change anything here)
option.method = 3; %3 for Behavioral PLS, 1 for Task PLS. 
option.num_boot = 10000;
option.num_perm = 10000;
option.meancentering_type = 0; % Only really matters for many groups
option.stacked_behavdata = bifactor;
option.weights = weight;
option.doCCA = 1
% Result structure will contain results (see documentation for more details on other things to include if needed)
result = pls_analysis(datamat_lst, num_subj, numconds, option);

%% Visualization 
% Results will give you a bar plot where the number of bars is 

% For visualizations. Current shows LV bar plot, and Histogram of bootstrap ratios

LV_Vis = 2; %Change this to the LV number you want to see

if option.method == 1 %this means its a task PLS
    design = result.boot_result.orig_usc(:,LV_Vis); 
    % upper and lower confidence intervals for bootstrapped scores
    ll = result.boot_result.llusc(:,LV_Vis); % upper limit 
    ul = result.boot_result.ulusc(:,LV_Vis); % lower limit 
else
    design = result.boot_result.orig_corr(:,LV_Vis); 
    % upper and lower confidence intervals for bootstrapped correlations
    ll = result.boot_result.llcorr(:,LV_Vis); % upper limit 
    ul = result.boot_result.ulcorr(:,LV_Vis); % lower limit 
end

figure;
% Bar plot of LV
subplot(1,1,1)
bar(design);hold on;
h = errorbar(1:numel(design),design,design-ll,ul-design);
h.LineStyle = 'none' 
    a=get(h,'Children');
set(gca, 'FontSize', 16);
set(gca, 'FontWeight', 'bold');
cb_var = num2str(result.s(LV_Vis)^2 / sum(result.s .^2)*100);
title(['LV ' num2str(LV_Vis) ', p = ' num2str(result.perm_result.sprob(LV_Vis))...
    ', Crossblock covariance = ' num2str(result.s(LV_Vis)^2 / sum(result.s .^2)*100) '%']);
ylabel('Design Salience');
if option.method == 1
    set(gca,'XTickLabel', conditions);
elseif option.method == 3
    set(gca,'Xtick',[1:length(BehavLabels)],'XTickLabelRotation', 45);
    set(gca,'XTickLabel', BehavLabels);
end

bsrs = result.boot_result.compare_u(:,LV_Vis);
% bsrs are bootstrap ratios, taking brain score and dividing it by standard
% error defined by bootstrapping permutation, basically a reliability
% estimate

% positive bsrs means it matching design scores, negative means it's
% inversely matching

% as a check, correlate some columns of data mat to behav mat to make sure
% interepration of signs is good

%Histogram of bootstrap ratios
% this will show whether the channels-performance relationship is reliable,
% less than -3 or more than 3 are significant
subplot(1,2,2); hist(bsrs); title(['LV ' num2str(LV_Vis) '- Histogram of Bootstrap Ratios'])
set(gca, 'FontSize', 16);
set(gca, 'FontWeight', 'bold');

LV_probabilities = result.perm_result.sprob
% this will give p-values for LVs, max LVs are 3 for a three condition
% example

%% Saving all PLS results to a .mat file
usc = result.usc;
vsc = result.vsc;
u=result.u;
save([result_path result_file_prefix model_name '.mat'],...
    'LV_probabilities', 'result', 'bsrs', 'design','ll','ul','cb_var','BehavLabels','usc','vsc','u')

