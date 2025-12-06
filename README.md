# COVID-19 and Cognitive Performance Analysis

This project investigates how COVID-19 infection and related clinical variables relate to cognitive performance across different domains (e.g., attention and language), using regression modelling and statistical testing in R. It was done for my Psychological Skills - R module. 

## 📊 Dataset

- Clinical and cognitive data for patients, including:
  - Demographic and clinical variables (e.g., age, education, COVID-related measures)
  - Cognitive outcomes such as **attention_targetDetection** and **language_verbalAnalogies**.

## 🧠 Objectives

- Model cognitive performance (attention and language) using relevant predictors.  
- Quantify how far individual patients deviate from model-based expectations.  
- Test whether these deviations are statistically significant, indicating impairment or resilience in specific domains.

## 🛠️ Tools & Libraries
- R:
  - `tidyverse` for data handling and wrangling
  - `ggplot2` for regression and scatter plots
  - Base R / `stats` functions for linear modelling and one-sample t‑tests
  - Additional packages as needed for data import and visualisation

## 🔍 Methods

- Construct a feature matrix `X` from selected clinical and demographic variables and separate targets for:
  - `attention_targetDetection`
  - `language_verbalAnalogies`.
- Split data into training and test sets using `train_test_split`.  
- Fit linear regression models for each outcome and obtain predicted scores for train and test sets.  
- Compute deviation scores (actual − predicted) and use one-sample t‑tests to assess whether mean deviations differ from zero.

## 💡 Key Analyses

- Visualise model performance by plotting actual vs. predicted scores for both training and test sets in each domain.  
- For each domain, report:
  - \(t\)-statistics and \(p\)-values for deviation scores
  - Mean deviation and count of negative deviations (worse-than-expected performance)
  - Interpret whether deviations suggest impairment or preserved performance at the group level.

## 📂 How to Run

1. Open the notebook/script in your preferred environment (e.g., Jupyter, or VS Code). 
2. Ensure the data file is available in the expected path and that required libraries are installed.  
3. Run all cells to reproduce data preparation, model fitting, plots, and statistical results.

## 🎯 Purpose

This project demonstrates how regression models and inferential statistics can be used together to interpret cognitive outcomes in a clinical/health context, linking data science methods to real-world questions about COVID-19 and cognition.
