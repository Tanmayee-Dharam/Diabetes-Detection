#EDA

diabetes <- read.csv("diabetes.csv")
library(tidyverse)
library(ggplot2)
options(scipen = 999)

# removing Patient number column

df <- diabetes[-1]
numeric_predictors <- df[-c(6,15)]

# checking for missing data
sum(is.na(df))

# no missing values = no need of Data imputation

summary(df)

# Replacing commas with periods in the df
df$chol_hdl_ratio <- as.numeric(gsub(",", ".", df$chol_hdl_ratio))
df$bmi <- as.numeric(gsub(",", ".", df$bmi))
df$waist_hip_ratio <- as.numeric(gsub(",", ".", df$waist_hip_ratio))

#Univariate Analyisis
# Checking for Outliers
boxplot(df$cholesterol, main='Cholestrol')
boxplot(df$glucose, main='Glucose')
boxplot(df$age, main='Age')
boxplot(df$height, main='Height')
boxplot(df$waist, main='Waist')
boxplot(df$hip, main='Hip')
boxplot(df$bmi, main='BMI')
boxplot(df$systolic_bp, main='Systolic BP')

# removing extreme outliers
df_filtered <- subset(df, df$cholesterol <= 350 & df$glucose <= 350 & df$systolic_bp <=240
                      & df$bmi <=50)

count_gender <- table(df_filtered$gender)
count_gender
barplot(count_gender, main='Gender', ylab='count', las=1, ylim = c(0,250))

count_diabetes <- table(df_filtered$diabetes)/390
count_diabetes
barplot(count_diabetes, main="Diabetes", ylab='proportion of cases', las=1)

# checking Normality of Predictors
hist(df_filtered$cholesterol)
hist(df_filtered$glucose)
hist(df_filtered$hdl_chol)
hist(df_filtered$age)
hist(df_filtered$height)
hist(df_filtered$weight)
hist(df_filtered$bmi)

#Bivariate Analysis

# females who have diabetes
f_diabetes <- df_filtered %>% 
  filter(gender == 'female') %>% 
  filter(diabetes == 'Diabetes')

count(f_diabetes)
count(f_diabetes)/223   #14.35 % females have diabetes

# males who have diabetes
m_diabetes <- df_filtered %>% 
  filter(gender=='male') %>% 
  filter(diabetes=='Diabetes')

count(m_diabetes)
count(m_diabetes)/158   #14.56% of males have diabetes

# Conduct a chi square test to check if the dif in gender is statistically significant
# H0: proportion of individuals with diabetes is same for males and females
# H1: proportion of individuals with diabetes is not the same for males and females
df_diabetes <- df_filtered %>% 
  filter(diabetes =='Diabetes')

# Create a contingency table
contingency_table <- table(df_diabetes$gender, df_diabetes$diabetes)

# Perform a chi-square test for independence
chi_square_result <- chisq.test(contingency_table)

# Print the results
print(chi_square_result) # p-value = 0.2249

# older males vs females
summary(df_filtered$age)

# Calculating Proportion of females with diabetes who are older than 60
df_filtered %>% 
filter(gender == 'female') %>% 
 filter(diabetes == 'Diabetes') %>% 
  filter(age > 60 ) %>% 
    count()  # 15 females with diabetes are older than 60
15/223  #0.08% of females with diabetes are older than 60

# Calculating Proportion of males with diabetes who are older than 60
df_filtered %>% 
     filter(gender=='male') %>% 
     filter(diabetes=='Diabetes') %>% 
     filter(age > 60) %>% 
     count() # 11 males with diabetes are older than 60
11/158  #0.07% of males with diabetes are older than 60

# checking strength of correlation of predictors with the outcome variable
ggplot(df_filtered, aes(x = diabetes, y = cholesterol)) +
  geom_boxplot() +
  labs(title = "Boxplot of Cholestrol by Outcome Class")

ggplot(df_filtered, aes(x = diabetes, y = glucose)) +
  geom_boxplot() +
  labs(title = "Boxplot of Glucose by Outcome Class")

ggplot(df_filtered, aes(x = diabetes, y = hdl_chol)) +
  geom_boxplot() +
  labs(title = "Boxplot of HDL_Chol by Outcome Class")

ggplot(df_filtered, aes(x = diabetes, y = age)) +
  geom_boxplot() +
  labs(title = "Boxplot of Age by Outcome Class")

ggplot(df_filtered, aes(x = diabetes, y = height)) +
  geom_boxplot() +
  labs(title = "Boxplot of Height by Outcome Class")

ggplot(df_filtered, aes(x = diabetes, y = weight)) +
  geom_boxplot() +
  labs(title = "Boxplot of Weight by Outcome Class")

ggplot(df_filtered, aes(x = diabetes, y = bmi)) +
  geom_boxplot() +
  labs(title = "Boxplot of BMI by Outcome Class")

ggplot(df_filtered, aes(x = diabetes, y = systolic_bp)) +
  geom_boxplot() +
  labs(title = "Boxplot of Systolic_BP by Outcome Class")

ggplot(df_filtered, aes(x = diabetes, y = diastolic_bp)) +
  geom_boxplot() +
  labs(title = "Boxplot of Diastolic_BP by Outcome Class")

# Conduct a two sample t tests to check significance of predictors 
t.test(cholesterol~diabetes, var.equal=F,
       alternative='two.sided', data=df_filtered)
t.test(glucose~diabetes, var.equal=F,
       alternative='two.sided', data=df_filtered)
t.test(hdl_chol~diabetes, var.equal=F,
       alternative='two.sided', data=df_filtered)
t.test(height~diabetes, var.equal=F,
       alternative='two.sided', data=df_filtered)
t.test(age~diabetes, var.equal=F,
       alternative='two.sided', data=df_filtered)
t.test(bmi~diabetes, var.equal=F,
       alternative='two.sided', data=df_filtered)
t.test(waist~diabetes, var.equal=F,
       alternative='two.sided', data=df_filtered)
t.test(systolic_bp~diabetes, var.equal=F,
       alternative='two.sided', data=df_filtered) # p-value=0.98


# check correlation among predictors
df_filtered_numeric <- df_filtered[c(1,2,3,4,5,7,8,9,10,11,12,13,14)]

df_cor <- cor(df_filtered_numeric)

# checking relationship b/w actual predictors and derived predictors
ggplot(df_filtered, aes(x=cholesterol, y=chol_hdl_ratio, color=diabetes)) + geom_point()
cor(df_filtered$cholesterol, df_filtered$chol_hdl_ratio)

# creating heatmap for predictors
heatmap(df_cor, Rowv = NA, Colv = NA)

# Variable reduction based on EDA
df_filtered_var <- df_filtered[c(2,4,5,9,10,11,14,15)]


#creating Machine Learning Models

#Regression Tree
library(rpart)
library(rpart.plot)
library(caret)

# partition
set.seed(12)  
train.index.ct <- sample(c(1:dim(df_filtered_var)[1]), dim(df_filtered_var)[1]*0.6)  
train.df.ct <- df_filtered_var[train.index.ct, ]
valid.df.ct <- df_filtered_var[-train.index.ct, ]

# classification tree
default.ct <- rpart(diabetes ~ ., data = train.df.ct, method = "class")
# plot tree
prp(default.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)

# Creating the longest Tree 
deeper.ct <- rpart(diabetes ~ ., data = train.df.ct, method = "class", cp = 0, minsplit = 1)
# count number of leaves
length(deeper.ct$frame$var[deeper.ct$frame$var == "<leaf>"])
# plot tree
prp(deeper.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(deeper.ct$frame$var == "<leaf>", 'gray', 'white')) 


# Make prediction on the validation set using default tree
default.ct.point.pred.valid <- predict(default.ct, valid.df.ct, type = "class")
# Generate the confusion matrix for the validation set using the default tree
confusionMatrix(default.ct.point.pred.valid, as.factor(valid.df.ct$diabetes))

# Make predictions on the validation set using the deeper tree
deeper.ct.point.pred.valid <- predict(deeper.ct, valid.df.ct, type = "class")
# Generate the confusion matrix for the validation set using the deeper tree
confusionMatrix(deeper.ct.point.pred.valid, as.factor(valid.df.ct$diabetes))


# Prune the deeper tree

# argument xval refers to the number of folds to use in rpart's built-in
# cross-validation procedure
# argument cp sets a very smal value for the complexity parameter.
cv.ct <- rpart(diabetes ~ ., data = train.df.ct, method = "class", 
               cp = 0.00001, minsplit = 5, xval = 5)
# use printcp() to print the table. 
printcp(cv.ct)

prp(cv.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(cv.ct$frame$var == "<leaf>", 'gray', 'white'))  

# Optimal Tree 
optimal.cp = cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"CP"]

# prune by lower cp
pruned.ct <- prune(cv.ct, optimal.cp)
pruned.ct <- prune(cv.ct, 
                  cp = cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"CP"])
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])
prp(pruned.ct, type = 1, extra = 1, split.font = 1, varlen = -10)  

# Make predictions on the validation set using the deeper tree
pruned.pred.valid <- predict(pruned.ct, valid.df.ct, type = "class")
# Generate the confusion matrix for the validation set using the deeper tree
confusionMatrix(pruned.pred.valid, as.factor(valid.df.ct$diabetes))



# Neural Networks
library(neuralnet)
library(caret)

df_filtered_var

# partition the data
set.seed(2)

train.index.nn <- sample(c(1:dim(df_filtered_var)[1]), dim(df_filtered_var)[1]*0.6)  
train.df.nn <- df_filtered_var[train.index.nn, ]
valid.df.nn <- df_filtered_var[-train.index.nn, ]

# create the neaural net

df_filtered_var$diabetes <- df_filtered_var$diabetes=="Diabetes"
df_filtered_var$no_diabetes <- df_filtered_var$diabetes=="No diabetes"

nn <- neuralnet(diabetes ~ ., data = train.df.nn, linear.output = F, hidden = 7)

plot(nn)

# display weights
nn$weights

# display predictions
prediction(nn)

# plot network
plot(nn, rep="best")

# Predict the class probabilities for the validation dataset
prediction <- predict(nn, newdata = valid.df.nn, type = "response")

# Extract the predicted probabilities for the "Diabetes" class
predicted_probabilities <- prediction[, 1]

# Convert the predicted probabilities to binary classes
predicted_classes <- ifelse(predicted_probabilities > 0.5, "Diabetes", "No diabetes")

# Convert actual classes to binary classes
actual_classes <- ifelse(valid.df.nn$diabetes == "Diabetes", "Diabetes", "No diabetes")

# Compare predicted classes to actual classes in the validation dataset
confusion_matrix <- confusionMatrix(as.factor(predicted_classes), as.factor(actual_classes))

# Print the confusion matrix
print(confusion_matrix)






# Logistic Regression 

df_lr <- df_filtered     # This dataset has been treated for outliers
df_selected_var <- df_lr[c(2,4,5,9,10,11,14,15)]   # Few Predictors removed based on EDA

# Creating the Logistic Model with Selected Variables
# Partition the data into training and validation sets
set.seed(123)  # Setting seed for reproducibility
trainIndex <- createDataPartition(df_lr$diabetes, p = 0.7, list = FALSE, times = 1)
diabetes_train <- df_selected_var[trainIndex, ]
diabetes_test <- df_selected_var[-trainIndex, ]

# Convert 'diabetes' variable from strings to numeric binary
diabetes_train$diabetes <- ifelse(diabetes_train$diabetes == "Diabetes", 1, 0)

# Fit the logistic regression model using selected predictors
model_lr <- glm(diabetes ~ ., data = diabetes_train, family = "binomial")

# Summary of the logistic regression model
summary(model_lr)

# Make predictions on the test set
predictions <- predict(model_lr, newdata = diabetes_test, type = "response")

# Compare predicted probabilities to actual classes
predicted_classes <- ifelse(predictions > 0.5, "Diabetes", "No Diabetes")
actual_classes <- ifelse(diabetes_test$diabetes == "Diabetes", "Diabetes", "No Diabetes")

# Convert predicted_classes and actual_classes to factors with the same levels
predicted_classes_factor <- factor(predicted_classes, levels = unique(c(predicted_classes, actual_classes)))
actual_classes_factor <- factor(actual_classes, levels = unique(c(predicted_classes, actual_classes)))

# Create the confusion matrix
conf_matrix <- confusionMatrix(predicted_classes_factor, actual_classes_factor)
print(conf_matrix)







