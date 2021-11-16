
clear
addpath('/Users/leighton/Documents/Vanderbilt_University/Kaczkurkin_Lab/Software/gifti-main')
ProjectFolder = 'Users/leighton/Desktop/PLS_CCA_Updated/Figures/CortexVisualize';
PrimPLSGroup1 = csvread([ProjectFolder '/PLSCCA_PrimPLSGroup1_BrainLoads.csv'], 1);

% Create file for surface
[~, VertexLabel_lh, Name_lh] = read_annotation('/Users/leighton/Documents/Vanderbilt_University/Kaczkurkin_Lab/Software/freesurfer/subjects/fsaverage/label/lh.aparc.annot');
[~, VertexLabel_rh, Name_rh] = read_annotation('/Users/leighton/Documents/Vanderbilt_University/Kaczkurkin_Lab/Software/freesurfer/subjects/fsaverage/label/rh.aparc.annot');
PrimPLSGroup1 = csvread([ProjectFolder '/PLSCCA_PrimPLSGroup1_BrainLoads.csv'], 1);
ind_lh = find(PrimPLSGroup1(:, 1) < 2000);
PrimPLSGroup1_lh = PrimPLSGroup1(ind_lh, :);
ind_rh = find(PrimPLSGroup1(:, 1) > 2000);
PrimPLSGroup1_rh = PrimPLSGroup1(ind_rh, :);
% lh
Vertex_AllLabel_InAtlas_lh = Name_lh.table(2:end, 5);
VertexStatistical_lh = zeros(size(VertexLabel_lh));
Sig_ID = PrimPLSGroup1_lh(:, 1);
for i = 1:length(Sig_ID)
  Vertex_LabelID = Vertex_AllLabel_InAtlas_lh(Sig_ID(i) - 1000);
  VertexIndex = find(VertexLabel_lh == Vertex_LabelID);
  VertexStatistical_lh(VertexIndex) = PrimPLSGroup1_lh(i, 2);
end
V_lh = gifti;
V_lh.cdata = VertexStatistical_lh;
V_lh_File = [ProjectFolder '/PrimPLSGroup1_lh.func.gii'];
save(V_lh, V_lh_File);
% rh
Vertex_AllLabel_InAtlas_rh = Name_rh.table(2:end, 5);
VertexStatistical_rh = zeros(size(VertexLabel_rh));
Sig_ID = PrimPLSGroup1_rh(:, 1);
for i = 1:length(Sig_ID)
  Vertex_LabelID = Vertex_AllLabel_InAtlas_rh(Sig_ID(i) - 2000);
  VertexIndex = find(VertexLabel_rh == Vertex_LabelID);
  VertexStatistical_rh(VertexIndex) = PrimPLSGroup1_rh(i, 2);
end
V_rh = gifti;
V_rh.cdata = VertexStatistical_rh;
V_rh_File = [ProjectFolder '/PrimPLSGroup1_rh.func.gii'];
save(V_rh, V_rh_File);

