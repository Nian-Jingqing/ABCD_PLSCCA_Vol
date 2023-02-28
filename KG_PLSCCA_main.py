#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue May 24 11:34:25 2022

@author: Karam Ghanem

Comparing Multivariate Analytical Approaches to Investigating Brain Behavior Relationships

"""

from sklearn.cross_decomposition import PLSCanonical,CCA,PLSRegression
import numpy as np
from scipy.stats import pearsonr
import os
import os.path as op
import numpy as np
import nibabel as nib
import glob
import pandas as pd
import joblib
from nilearn import datasets as ds
from nilearn.input_data import NiftiLabelsMasker, NiftiMasker
from nilearn.image import resample_img, index_img, concat_imgs, math_img
import sklearn
import matplotlib.pylab as plt
from sklearn.preprocessing import StandardScaler
from sklearn.svm import LinearSVR, LinearSVC
from sklearn.decomposition import PCA
import seaborn as sns

local_path = '' #Create ABCD Data location as string 

def NonparametricImpute(input_vars):
	nan_inds = np.where(np.isnan(input_vars))[0]
	pres_inds = np.where(~np.isnan(input_vars))[0]
	rs = np.random.RandomState(0)
	rs.shuffle(pres_inds)
	input_vars[nan_inds] = input_vars[pres_inds[:len(nan_inds)]]
	return input_vars


n_comps = 4 #number of components 
abcd = pd.read_csv(local_path + '/ABCD_PLSCCA_Residuals_Primary.csv', low_memory=True) # ABCD Data

brain = abcd[abcd.columns[2:89]] #cortical and subcortical gray matter volume
bifactor = abcd[abcd.columns[89:93]] #psychopathology data for 4 psychopathology factors
weight =  abcd[abcd.columns[1]]

#Standardized weights

S_scaler = StandardScaler()
brain_val = brain.values
brain_val_ss = S_scaler.fit_transform(brain_val)
brain_val_ss = pd.DataFrame(brain_val_ss, columns = brain.columns)
brain_val_ss = NonparametricImpute(brain_val_ss)

S_scaler = StandardScaler()
bifactor_val  = bifactor.values
bifactor_val_ss = S_scaler.fit_transform(bifactor_val)
bifactor_val_ss = pd.DataFrame(bifactor_val_ss, columns = bifactor.columns)
bifactor_val_ss = NonparametricImpute(bifactor_val_ss)

Y = bifactor_val_ss
X = brain_val_ss

beh_variables = bifactor

BehavLabels =  ['Gen', 'Ext', 'ADHD', 'Int'] #psychopathology labels
#where do I find the 'general' psychopathology factor
# Run PLS Canonical https://scikit-learn.org/stable/modules/generated/sklearn.cross_decomposition.PLSCanonical.html
plsc = PLSCanonical(n_components=n_comps)
plsc.fit(X, Y) 
r2 = plsc.score(X, Y)  # coefficient of determination :math:`R^2`

#creating csv files for the loadings
xx_plsc= plsc.x_loadings_
csv_data = pd.DataFrame(xx_plsc)
csv_data.to_csv('/Users/dblab/Desktop/Project Amygdala/Amygdala/plsc_cortical_loadings.csv') 

yy_plsc = plsc.y_loadings_
csv_data = pd.DataFrame(yy_plsc)
csv_data.to_csv('/Users/dblab/Desktop/Project Amygdala/Amygdala/plsc_psychopathological_loadings.csv') 

# Find r value of every component
est = plsc
actual_Rs = np.array([pearsonr(X_coef, Y_coef)[0] for X_coef, Y_coef in
    zip(est.x_scores_.T, est.y_scores_.T)])

print(actual_Rs)

#[0.10405287 0.08384607 0.07613267 0.06829696]

#Plotting heat maps of the X loadings
OUT_DIR = ''
SUFFIX = ''
for counter, i_comp in enumerate(range(n_comps)):

  n_rois = plsc.x_loadings_.shape[0] #shape of X loadings 
  X_AM_weights = plsc.x_loadings_[:, i_comp] #redefine x loadings in a new variable


  f = plt.figure(figsize=(9, 6), dpi = 600) #figure dimensions
  X_comp_weights = np.zeros((n_rois, 1)) #zero matrix thatll be populated with the x loadings
  X_comp_weights[:, 0] = X_AM_weights


  dfdata = pd.DataFrame(X_AM_weights)
  dfdata.to_csv('%s/plsc_topcomp%i%s_style_.csv' % (OUT_DIR, counter + 1, SUFFIX)) #create a csv file with the data
  dfdata.to_excel('%s/plsc_topcomp%i%s_style_.xls' % (OUT_DIR, counter + 1, SUFFIX)) #create an xls file with the data


  #heat map function used to create heatmap with the data
  ax = sns.heatmap(dfdata, cbar=True, linewidths=.75,
                   cbar_kws={'shrink': 0.5}, #'orientation': 'horizontal'},  #, 'label': 'Functional coupling deviation'},
                   square=True,
                   cmap=plt.cm.RdBu_r, center=0) 

  #labelling the heat map
  ax.set_xticklabels(ax.get_xticklabels(), fontsize=10)
  ax.set_yticklabels(ax.get_yticklabels(), fontsize=10)
  
  #organizing the heatmap
  b, t = plt.ylim() # discover the values for bottom and top
  b += 0.5 # Add 0.5 to the bottom
  t -= 0.5 # Subtract 0.5 from the top
  plt.ylim(b, t) # update the ylim(bottom, top) values
  plt.tight_layout()

  # save figures as png and pdf
  plt.savefig('%s/plsc_topcomp%i%s_style_.png' % (OUT_DIR, counter + 1, SUFFIX), DPI=200)
  plt.savefig('%s/plsc_topcomp%i%s_style_.pdf' % (OUT_DIR, counter + 1, SUFFIX))


#Number of significant components PLSC

pls = PLSCanonical(n_components=n_comps)
pls.fit(X, Y) 
r2 = pls.score(X, Y)  # coefficient of determination :math:`R^2`

# Find r value of every component
est = pls
actual_Rs = np.array([pearsonr(X_coef, Y_coef)[0] for X_coef, Y_coef in
    zip(est.x_scores_.T, est.y_scores_.T)])

print(actual_Rs)

n_keep = 4
n_permutations = 1000
cur_X = np.array(X)
cur_Y = np.array(Y)
perm_rs = np.random.RandomState(1)
perm_Rs = []
perm_scores = []
n_except = 0
for i_iter in range(n_permutations):
    print(i_iter + 1)

    perm_rs.shuffle(cur_Y)

    # cur_X_perm = np.array([perm_rs.permutation(sub_entry) for sub_entry in cur_X])

    # same procedure, only with permuted subjects on the right side
    try:
        perm_plsc = PLSCanonical(n_components=n_keep, scale=False)  # VERIFY

        # perm_inds = np.arange(len(Y_netmet))
        # perm_rs.shuffle(perm_inds)
        # perm_cca.fit(X_nodenode, Y_netnet[perm_inds, :])
        perm_plsc.fit(cur_X, cur_Y)

        perm_R = np.array([pearsonr(X_coef, Y_coef)[0] for X_coef, Y_coef in
            zip(perm_plsc.x_scores_.T, perm_plsc.y_scores_.T)])
        cur_score = perm_plsc.score(cur_X, cur_Y)
        print(np.sort(perm_R)[::-1][:10])
        print(cur_score)
        perm_Rs.append(perm_R)
        perm_scores.append(cur_score)
    except:
        n_except += 1
        perm_Rs.append(np.zeros(n_keep))
perm_Rs = np.array(perm_Rs)

pvals = []
for i_coef in range(n_keep):  # COMP-WISE comparison to permutation results !!!
    cur_pval = (np.sum(perm_Rs[:, i_coef] > actual_Rs[i_coef])) / n_permutations
    pvals.append(cur_pval)
    # print cur_pval
    
# =============PLSC=====================
# New - [0.0, 0.046, 0.146, 0.485]
# 1 CCs are significant at p<0.05
# 1 CCs are significant at p<0.01
# =====================================
   

# Run Canonical Correlation (CCA) https://scikit-learn.org/stable/modules/generated/sklearn.cross_decomposition.CCA.html
cca = CCA(n_components=n_comps)
cca.fit(X, Y)
r2 = cca.score(X, Y)  # coefficient of determination :math:`R^2`


# Find r value of every component
est = cca
actual_Rs = np.array([pearsonr(X_coef, Y_coef)[0] for X_coef, Y_coef in
    zip(est.x_scores_.T, est.y_scores_.T)])

print(actual_Rs)

#[0.15707483 0.11114757 0.09888529 0.08914161]

#creating csv files for the loadings
xx_cca = cca.x_loadings_
csv_data = pd.DataFrame(xx_cca)
csv_data.to_csv('/Users/dblab/Desktop/Project Amygdala/Amygdala/cca_cortical_loadings.csv') 

yy_cca = cca.y_loadings_
csv_data = pd.DataFrame(yy_cca)
csv_data.to_csv('/Users/dblab/Desktop/Project Amygdala/Amygdala/cca_psychopathological_loadings.csv') 

#Plotting heat maps of the X loadings
OUT_DIR = ''
SUFFIX = ''
for counter, i_comp in enumerate(range(n_comps)):

  n_rois = cca.x_loadings_.shape[0] #shape of X loadings 
  X_AM_weights = cca.x_loadings_[:, i_comp] #redefine x loadings in a new variable


  f = plt.figure(figsize=(9, 6), dpi = 600) #figure dimensions
  X_comp_weights = np.zeros((n_rois, 1)) #zero matrix thatll be populated with the x loadings
  X_comp_weights[:, 0] = X_AM_weights


  dfdata = pd.DataFrame(X_AM_weights)
  dfdata.to_csv('%s/cca_topcomp%i%s_style_.csv' % (OUT_DIR, counter + 1, SUFFIX)) #create a csv file with the data
  dfdata.to_excel('%s/cca_topcomp%i%s_style_.xls' % (OUT_DIR, counter + 1, SUFFIX)) #create an xls file with the data


  #heat map function used to create heatmap with the data
  ax = sns.heatmap(dfdata, cbar=True, linewidths=.75,
                   cbar_kws={'shrink': 0.5}, #'orientation': 'horizontal'},  #, 'label': 'Functional coupling deviation'},
                   square=True,
                   cmap=plt.cm.RdBu_r, center=0) 

  #labelling the heat map
  ax.set_xticklabels(ax.get_xticklabels(), fontsize=10)
  ax.set_yticklabels(ax.get_yticklabels(), fontsize=10)
  
  #organizing the heatmap
  b, t = plt.ylim() # discover the values for bottom and top
  b += 0.5 # Add 0.5 to the bottom
  t -= 0.5 # Subtract 0.5 from the top
  plt.ylim(b, t) # update the ylim(bottom, top) values
  plt.tight_layout()

  # save figures as png and pdf
  plt.savefig('%s/cca_topcomp%i%s_style_.png' % (OUT_DIR, counter + 1, SUFFIX), DPI=200)
  plt.savefig('%s/cca_topcomp%i%s_style_.pdf' % (OUT_DIR, counter + 1, SUFFIX))

#Number of significant components CCA
# Run Canonical Correlation (CCA) https://scikit-learn.org/stable/modules/generated/sklearn.cross_decomposition.CCA.html
cca = CCA(n_components=n_comps)
cca.fit(X, Y)
r2 = cca.score(X, Y)  # coefficient of determination :math:`R^2`


# Find r value of every component
est = cca
actual_Rs = np.array([pearsonr(X_coef, Y_coef)[0] for X_coef, Y_coef in
    zip(est.x_scores_.T, est.y_scores_.T)])

print(actual_Rs)

n_keep = 4
n_permutations = 1000
cur_X = np.array(X)
cur_Y = np.array(Y)
perm_rs = np.random.RandomState(1)
perm_Rs = []
perm_scores = []
n_except = 0
for i_iter in range(n_permutations):
    print(i_iter + 1)

    perm_rs.shuffle(cur_Y)

    # cur_X_perm = np.array([perm_rs.permutation(sub_entry) for sub_entry in cur_X])

    # same procedure, only with permuted subjects on the right side
    try:
        perm_cca = CCA(n_components=n_keep, scale=False)  # VERIFY

        # perm_inds = np.arange(len(Y_netmet))
        # perm_rs.shuffle(perm_inds)
        # perm_cca.fit(X_nodenode, Y_netnet[perm_inds, :])
        perm_cca.fit(cur_X, cur_Y)

        perm_R = np.array([pearsonr(X_coef, Y_coef)[0] for X_coef, Y_coef in
            zip(perm_cca.x_scores_.T, perm_cca.y_scores_.T)])
        cur_score = perm_cca.score(cur_X, cur_Y)
        print(np.sort(perm_R)[::-1][:10])
        print(cur_score)
        perm_Rs.append(perm_R)
        perm_scores.append(cur_score)
    except:
        n_except += 1
        perm_Rs.append(np.zeros(n_keep))
perm_Rs = np.array(perm_Rs)

pvals = []
for i_coef in range(n_keep):  # COMP-WISE comparison to permutation results !!!
    cur_pval = (np.sum(perm_Rs[:, i_coef] > actual_Rs[i_coef])) / n_permutations
    pvals.append(cur_pval)
    # print cur_pval
    
# =============CCA=====================
# New - [0.0, 0.026, 0.104, 0.127]
# 1 CCs are significant at p<0.05
# 1 CCs are significant at p<0.01
# =====================================
   

# Run PLS Regression more info: https://scikit-learn.org/stable/modules/generated/sklearn.cross_decomposition.PLSRegression.html
plsr = PLSRegression(n_components=n_comps)
plsr.fit(X, Y) 
r2 = plsr.score(X, Y)  # coefficient of determination :math:`R^2`

# Find r value of every component
est = plsr
actual_Rs = np.array([pearsonr(X_coef, Y_coef)[0] for X_coef, Y_coef in
    zip(est.x_scores_.T, est.y_scores_.T)])

print(actual_Rs)

#[0.10405287 0.08327401 0.09504872 0.07128839]

#creating csv files for the loadings
xx_plsr = plsr.x_loadings_
csv_data = pd.DataFrame(xx_plsr)
csv_data.to_csv('/Users/dblab/Desktop/Project Amygdala/Amygdala/plsr_cortical_loadings.csv') 

yy_plsr = plsr.y_loadings_
csv_data = pd.DataFrame(yy_plsr)
csv_data.to_csv('/Users/dblab/Desktop/Project Amygdala/Amygdala/plsr_psychopathological_loadings.csv') 

#Plotting heat maps of the X loadings
OUT_DIR = ''
SUFFIX = ''
for counter, i_comp in enumerate(range(n_comps)):

  n_rois = plsr.x_loadings_.shape[0] #shape of X loadings 
  X_AM_weights = plsr.x_loadings_[:, i_comp] #redefine x loadings in a new variable


  f = plt.figure(figsize=(9, 6), dpi = 600) #figure dimensions
  X_comp_weights = np.zeros((n_rois, 1)) #zero matrix thatll be populated with the x loadings
  X_comp_weights[:, 0] = X_AM_weights


  dfdata = pd.DataFrame(X_AM_weights)
  dfdata.to_csv('%s/plsr_topcomp%i%s_style_.csv' % (OUT_DIR, counter + 1, SUFFIX)) #create a csv file with the data
  dfdata.to_excel('%s/plsr_topcomp%i%s_style_.xls' % (OUT_DIR, counter + 1, SUFFIX)) #create an xls file with the data


  #heat map function used to create heatmap with the data
  ax = sns.heatmap(dfdata, cbar=True, linewidths=.75,
                   cbar_kws={'shrink': 0.5}, #'orientation': 'horizontal'},  #, 'label': 'Functional coupling deviation'},
                   square=True,
                   cmap=plt.cm.RdBu_r, center=0) 

  #labelling the heat map
  ax.set_xticklabels(ax.get_xticklabels(), fontsize=10)
  ax.set_yticklabels(ax.get_yticklabels(), fontsize=10)
  
  #organizing the heatmap
  b, t = plt.ylim() # discover the values for bottom and top
  b += 0.5 # Add 0.5 to the bottom
  t -= 0.5 # Subtract 0.5 from the top
  plt.ylim(b, t) # update the ylim(bottom, top) values
  plt.tight_layout()

  # save figures as png and pdf
  plt.savefig('%s/plsr_topcomp%i%s_style_.png' % (OUT_DIR, counter + 1, SUFFIX), DPI=200)
  plt.savefig('%s/plsr_topcomp%i%s_style_.pdf' % (OUT_DIR, counter + 1, SUFFIX))
   
    
#Number of significant components PLSR

# Run PLS Regression more info: https://scikit-learn.org/stable/modules/generated/sklearn.cross_decomposition.PLSRegression.html
plsr = PLSRegression(n_components=n_comps)
plsr.fit(X, Y) 
r2 = plsr.score(X, Y)  # coefficient of determination :math:`R^2`

# Find r value of every component
est = plsr
actual_Rs = np.array([pearsonr(X_coef, Y_coef)[0] for X_coef, Y_coef in
    zip(est.x_scores_.T, est.y_scores_.T)])

print(actual_Rs)

n_keep = 4
n_permutations = 1000
cur_X = np.array(X)
cur_Y = np.array(Y)
perm_rs = np.random.RandomState(1)
perm_Rs = []
perm_scores = []
n_except = 0
for i_iter in range(n_permutations):
    print(i_iter + 1)

    perm_rs.shuffle(cur_Y)

    # cur_X_perm = np.array([perm_rs.permutation(sub_entry) for sub_entry in cur_X])

    # same procedure, only with permuted subjects on the right side
    try:
        perm_plsr = PLSRegression(n_components=n_keep, scale=False)  # VERIFY

        # perm_inds = np.arange(len(Y_netmet))
        # perm_rs.shuffle(perm_inds)
        # perm_cca.fit(X_nodenode, Y_netnet[perm_inds, :])
        perm_plsr.fit(cur_X, cur_Y)

        perm_R = np.array([pearsonr(X_coef, Y_coef)[0] for X_coef, Y_coef in
            zip(perm_plsr.x_scores_.T, perm_plsr.y_scores_.T)])
        cur_score = perm_plsr.score(cur_X, cur_Y)
        print(np.sort(perm_R)[::-1][:10])
        print(cur_score)
        perm_Rs.append(perm_R)
        perm_scores.append(cur_score)
    except:
        n_except += 1
        perm_Rs.append(np.zeros(n_keep))
perm_Rs = np.array(perm_Rs)

pvals = []
for i_coef in range(n_keep):  # COMP-WISE comparison to permutation results !!!
    cur_pval = (np.sum(perm_Rs[:, i_coef] > actual_Rs[i_coef])) / n_permutations
    pvals.append(cur_pval)
    # print cur_pval
    

# =============PLSR====================
# New - [0.0, 0.049, 0.0, 0.257]
# 1 CCs are significant at p<0.05
# 2 CCs are significant at p<0.01
# =====================================