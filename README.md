# Predicting-heart-disease

## Introduction

One of the leading cause of death for both men and women currently is heart disease. In the United States of America for every 36 seconds someone dies (Capewell et al. 2010). A cumulative of approximately 655,000 people have died from heart diseases. Some of these heart diseases are for example coronary artery disease and stroke. This makes it important for health facility to be able to predict occurrences of heart diseases in order to make plans for early treatment (Assmann et al. 2005). This research paper will try to figure out the relationship between several pre-existing health conditions and whether the health conditions were accompanied by heart disease.

## Data

The data used is from a secondary data source. It is from a statistical website called Kaggle. The data set has 303 observations of 14 variables. The dependent variable is called “target” and describes whether someone has heart disease or not. 1=has heart disease, 0= has no heart disease. The data is not 100% exhaustive as there might exist some more related variables. The data has no observable anomalies but after exportation to R there are some observable issues that needs to be corrected. This involves the data type as R reads the data differently from its original form in excel. The data dictionary is as follows:

- sex;  1= male, 0= female.
- cp = chest pain; 0=typical angina, 1=atypical angina, 2=non-anginal and 3=symptomatic
- trestbps = resting blood sugar 
- chol = cholesterol measurement. 
- fbs = fasting blood sugar; >120mg/dl=1, otherwise=0.
- restecg = resting electrocardiographic measurement; 0=normal, 1=having STT abnormality,
2=showing probable or definate left ventricular hypertrophy by Este’s criteria. 
- thalach = Maximum heart rate achieved.
- exang = exercise induced angina; 1=yes, 0=no. 
- oldpeak = ST depression. 
- slope = slope of peak exercise; 1=upsloping, 2=flat, 3=downsloping.
- ca = number of major vessels. 
- thal = thalassemia blood disorder.