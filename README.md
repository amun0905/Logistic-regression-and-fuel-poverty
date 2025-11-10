# Fuel Poverty Analysis with Logistic Regression

This R script explores fuel poverty in England using socio-economic data from the English Housing Survey: Fuel Poverty Dataset (2019). 


### Objectives

1. Load and clean the fuel poverty dataset.
2. Explore socio-economic variables using visualizations.
3. Fit and evaluate a logistic regression model.
4. Address class imbalance using oversampling.
5. Interpret model results and assess predictive performance.

---

## Dataset

- Source: UK Data Service  
  [English Housing Survey: Fuel Poverty Dataset, 2019](https://beta.ukdataservice.ac.uklogue#!/details
- Period: April 2018 – March 2020
- Note: Data access may require a license.

---

## Methodology

### 1. Data Preparation
- Rename variables for clarity.
- Recode categorical variables (e.g., Region, Tenure, Dwelling Type).
- Filter out non-residential dwellings.

### 2. Exploratory Data Analysis
- Visualize distributions of key variables:
  - Region
  - Tenure
  - Dwelling type
  - Working status
  - Ethnic origin
  - EPC rating
  - Household composition

### 3. Modeling
- Fit logistic regression (`glm`) to predict `In_fuel_Poverty`.
- Evaluate model using:
  - McFadden's pseudo R² (`blorr`)
  - Confusion matrix (`caret`)
  - ROC curve and AUC (`pROC`)
  - Precision, Recall, F1-score (`ROSE`)

### 4. Class Imbalance Handling
- Use random oversampling to balance minority class (fuel poor households).
- Refit model on balanced data and compare performance.

---

## Results Summary

| Model        | Accuracy | Sensitivity | Specificity | AUC  |
|--------------|----------|-------------|-------------|------|
| Initial (`mylog1`) | 0.85     | 0.04        | 0.99        | 0.52 |
| Balanced (`mylog2`)| 0.70     | 0.60        | 0.71        | 0.67 |

- Interpretation:
  - Vulnerable households, renters, ethnic minorities, and unemployed heads of household are more likely to be fuel poor.
  - Balancing the dataset improves sensitivity and overall model performance.

---

## Packages
library(dplyr)
library(ggplot2)
library(caTools)
library(caret)
library(ROSE)
library(pROC)
library(blorr)
