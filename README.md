# Predicting-heart-disease

## Overview

Cardiovascular diseases (CVDs) are the number 1 cause of death globally, taking an estimated 17.9 million lives each year, which accounts for 31% of all deaths worldwide. Four out of 5CVD deaths are due to heart attacks and strokes, and one-third of these deaths occur prematurely in people under 70 years of age. This makes it important for health facilities to be able to predict occurrences of heart diseases in order to make plans for early treatment (Assmann et al. 2005). Heart failure is a common event caused by CVDs and this dataset contains 11 features that can be used to predict a possible heart disease.

People with cardiovascular disease or who are at high cardiovascular risk (due to the presence of one or more risk factors such as hypertension, diabetes, hyperlipidaemia or already established disease) need early detection and management wherein a machine learning model can be of great help. 

## Data

This dataset was created by combining different datasets already available independently but not combined before. In this dataset, 5 heart datasets are combined over 11 common features which makes it the largest heart disease dataset available so far for research purposes. The five datasets used for its curation are:

Cleveland: 303 observations
Hungarian: 294 observations
Switzerland: 123 observations
Long Beach VA: 200 observations
Stalog (Heart) Data Set: 270 observations
Total: 1190 observations
Duplicated: 272 observations

Final dataset: 918 observations

Every dataset used can be found under the Index of heart disease datasets from UCI Machine Learning Repository on the following link: https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/ The data dictionary is as follows:

- output: target variable (1=has heart attack, 0= has no heart attack)
- age: Age of the patient
- sex: Sex of the patient  (1= male, 0= female).
- cp: chest pain type (0=typical angina, 1=atypical angina, 2=non-anginal and 3=asymptomatic)
- trtbps: resting blood pressure (in mm Hg) 
- chol: cholestoral in mg/dl fetched via BMI sensor 
- fbs: fasting blood sugar (>120mg/dl=1, <= 120mg/dl=0).
- restecg: resting electrocardiographic results( 0=normal, 1=having ST-T wave abnormality,
2=showing probable or definate left ventricular hypertrophy by Este’s criteria.) 
- thalach = Maximum heart rate achieved.
- exng = exercise induced angina; 1=yes, 0=no. 
- oldpeak = ST depression. 
- slp = slope of peak exercise; 0=upsloping, 1=flat, 2=downsloping.
