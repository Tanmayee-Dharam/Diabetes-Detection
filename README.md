# Diabetes-Detection
Predictive Modeling investigating the likelihood of diabetes through a comprehensive analysis of various risk factors
This project was developed as part of a classroom assignment at the University of Texas at Dallas.

# Overview:
This project aims to predict the risk of diabetes using health-related attributes through statistical analysis and machine learning techniques. We analyzed a clinical dataset from a U.S. hospital containing both categorical and numerical patient health records to understand the factors contributing to diabetes and built models to classify patients accordingly.
The study is observational, allowing for causal inferences, and focuses on using derived health indicators like BMI, Chol/HDL Ratio, and Waist-to-Hip Ratio for better accuracy.

# Dataset Summary:
1. Records: 390 patient records
2. Features: 16 total (13 numerical, 3 categorical)
3. Derived Features: BMI, Chol/HDL Ratio, Waist-to-Hip Ratio
4. Target Variable: Diabetes class (Yes/No)
5. Class Distribution: 14.1% Diabetic, 83.5% Non-Diabetic

# Tech Stack:
- R Programming Language
- Libraries: caret, neuralnet, rpart, ggplot2
- Environment: RStudio

# Exploratory Data Analysis (EDA):
1. Univariate & Bivariate Analysis
2. Outlier Detection & Filtering
3. Chi-Square and T-tests for predictor significance
4. Correlation Heatmaps to reduce multicollinearity
5. Feature selection based on statistical significance and medical relevance

**Selected Features for Modeling:**
1. Glucose
2. Age
3. BMI
4. Systolic & Diastolic BP
5. Chol/HDL Ratio
6. Waist-to-Hip Ratio

# Machine Learning Models:
1️. Classification Tree
   - Built default, deep (overfitted), and pruned versions
   - Pruned tree achieved 87.58% accuracy
   - Extracted interpretable decision rules

2. Neural Network
- One hidden layer with optimal tuning
- Achieved 88.24% accuracy on validation set

3. Logistic Regression
 - Best model with highest accuracy and precision
 - Balanced performance on all key metrics (Accuracy, Sensitivity, Specificity, Precision)

# Conclusion:
After extensive model evaluation:
- Logistic Regression was the most effective model for predicting diabetes, offering a strong balance between all evaluation metrics.
- Neural Networks showed high performance but were less interpretable.
- Regression Trees offered understandable rules but showed signs of overfitting if not pruned.
