#README

Author
--------------------------------------------------------------------------------
- Student Name: Lara Habashy
- Student ID: 101012961
- Date: August 3, 2020

Purpose
--------------------------------------------------------------------------------
Predicting breast cancer diagnosis as either cancerous or non-cancerous, using ML models

Source files
--------------------------------------------------------------------------------
- exploratory.r - set directory, import data, format (standardize) data, explores feature selection methods such as random forest variable importance. Explores mention for dimension reduction
- corr.r - analysis of correlation in the data
- modelling.r - building ML models based on subsets in *exploratory.r*, Logistic Regression, Support Vector Machine, Naive Bayes 
- eval.r - evaluation of ML models -- ROC curve, AUC score, accuracy, F-score, Kappa, computational complexity


Additional files
--------------------------------------------------------------------------------
data.csv (Source: Breast Cancer Wisconsin (Diagnostic) Data Set. (2016, September 25). 
          Retrieved July 13, 2020, from https://www.kaggle.com/uciml/breast-cancer-wisconsin-data#data.csv)


Compilation and launching instructions
--------------------------------------------------------------------------------
1. Download the Wisconsin Breast Cancer dataset found in this repository. [data.csv]
2. Adjust the directory setting line to match your directory. [exploratory.r Line 2]
3. Run exploratory.r
4. Run corr.r
5. Run modelling.r
6. Run eval.r
