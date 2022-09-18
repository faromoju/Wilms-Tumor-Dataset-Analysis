---
title: "MAS Project"
author: "TOBI"
date: "4/6/2022"
output:
  word_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

setwd("C:/Users/farom/Downloads/School/400 Level/COSC 408 Modelling and Simulation/MAS Project")

* 1.00. Loading the dataset into the global variables.
```{r}
setwd("C:/Users/farom/Downloads/School/400 Level/COSC 408 Modelling and Simulation/MAS Project")
clinical = read.csv("wt_target_2018_pub_clinical_data.csv")
names(clinical)
```
* 1.01. Deleting some useless columns off the dataset
```{r}
clinical = clinical[,-c(1:3,6:8,13:18,22,23,25,27)]
names(clinical)
str(clinical)
```
* 1.02. Load dplyr and rename some variables using the function dplyr::rename().
* 1.03. View column names, structure and summary of sorted data for insight
* 1.04. Display the clinical dataset
```{r}
library(dplyr) # A grammar for data manipulation
clinical = clinical %>% 
  rename(
    Diagnosis_Age = Diagnosis.Age,
    Diagnosis_Age_Days = Diagnosis.Age..days.,
    Cancer_Stage = Neoplasm.American.Joint.Committee.on.Cancer.Clinical.Group.Stage,
    Time_To_Event = Time.To.Event..days.,
    Ethnicity = Ethnicity.Category,
    Event_Type = Event.Type,
    Survival_Days = Overall.Survival.Days,
    Survival_Months = Overall.Survival..Months.,
    Survival_Status = Overall.Survival.Status,
    Race = Race.Category,
    TMB = TMB..nonsynonymous.
    )
summary(clinical)
str(clinical)
```
*1.05. Rearrange the columns
```{r}
col_order1 = c("Diagnosis_Age","Diagnosis_Age_Days","Cancer_Stage", "Time_To_Event","Ethnicity","Event_Type","Survival_Days","Survival_Months","Race","Sex","TMB","Survival_Status")
clinical = clinical[, col_order1]
```
* 1.06. Create separate dataset for Bi variate analysis.
```{r}
Multiclinical = cbind.data.frame(clinical)
```
* 1.07. To enhance uni-variate statistical analyses, the factor type variables will be encoded using numbers and converted to integer data type and stored in the "clinical" object.
* 1.08. Run table function on concerned variables to correctly pick out value entries.
* 1.09. Confirm that all factor types have been converted to numeric and view "clinical" object.
```{r}
attach(clinical)
library(tidyr)

"Cancer_Stage"
table(Cancer_Stage)
clinical$Cancer_Stage = as.character(clinical$Cancer_Stage)
clinical$Cancer_Stage[clinical$Cancer_Stage == "I"] <- 1
clinical$Cancer_Stage[clinical$Cancer_Stage == "II"] <- 2
clinical$Cancer_Stage[clinical$Cancer_Stage == "III"] <- 3
clinical$Cancer_Stage[clinical$Cancer_Stage == "IV"] <- 4
clinical$Cancer_Stage[clinical$Cancer_Stage == "V"] <- 5
clinical$Cancer_Stage[clinical$Cancer_Stage == "II/V"] <- 6
clinical$Cancer_Stage[clinical$Cancer_Stage == "III/V"] <- 7
clinical$Cancer_Stage[clinical$Cancer_Stage == "IV/V"] <- 8
clinical$Cancer_Stage[clinical$Cancer_Stage == "IIIB"] <- 9
clinical$Cancer_Stage[clinical$Cancer_Stage == "IIIB/V"] <- 10
clinical$Cancer_Stage[clinical$Cancer_Stage == "U"] <- 11
clinical$Cancer_Stage = as.integer(clinical$Cancer_Stage)

"Ethnicity"
table(Ethnicity)
clinical$Ethnicity = as.character(clinical$Ethnicity)
clinical$Ethnicity[clinical$Ethnicity == "Not reported"] <- 1
clinical$Ethnicity[clinical$Ethnicity == "Not Hispanic or Latino"] <- 2
clinical$Ethnicity[clinical$Ethnicity == "Hispanic or Latino"] <- 3
clinical$Ethnicity = as.integer(clinical$Ethnicity)

"Event_Type"
table(Event_Type)
clinical$Event_Type = as.character(clinical$Event_Type)
clinical$Event_Type[clinical$Event_Type == "Relapse"] <- 1
clinical$Event_Type[clinical$Event_Type == "None"] <- 2
clinical$Event_Type[clinical$Event_Type == "Progression"] <- 3
clinical$Event_Type = as.integer(clinical$Event_Type)

"Race"
table(Race)
clinical$Race = as.character(clinical$Race)
clinical$Race[clinical$Race == "White"] <- 1
clinical$Race[clinical$Race == "Black or African American"] <- 2
clinical$Race[clinical$Race == "Not reported"] <- 3
clinical$Race[clinical$Race == "Other"] <- 4
clinical$Race = as.integer(clinical$Race)

"Sex"
table(Sex)
clinical$Sex = as.character(clinical$Sex)
clinical$Sex[clinical$Sex == "Male"] <- 1
clinical$Sex[clinical$Sex == "Female"] <- 2
clinical$Sex = as.integer(clinical$Sex)

"Survival_Status"
table(Survival_Status)
clinical$Survival_Status = as.character(clinical$Survival_Status)
clinical$Survival_Status[clinical$Survival_Status == "1:DECEASED"] <- 0
clinical$Survival_Status[clinical$Survival_Status == "0:LIVING"] <- 1
clinical$Survival_Status = as.integer(clinical$Survival_Status)

str(clinical)
summary(clinical)
```
* 1.10. Check for missing values (NA's) in the records using the summary function.
* 1.11. Use DataExplorer & mice packages to further analyze missing records and fix values.
```{r}
summary(clinical)

library(DataExplorer) # automate visual exploration of data and treatment
plot_intro(clinical)
plot_missing(clinical)

library(mice) # Multivariate Imputation by Chained Equations
md.pattern(clinical)
mice_imputes = mice(clinical, m = 5, maxit = 5, seed = 10) 
mice_imputes$method

clinical = complete(mice_imputes, 5) #clinical dataset to be used for bi-variate analysis

md.pattern(clinical)
summary(clinical) #confirm absence of missing data and gain insigts on the data
```

#### 2.0  Univariate Analysis for Features

```{r}
# Function to visualize histogram and boxplot of numerical variables using ggplot
 
library(ggplot2) # For graphs and visualizations
library(gridExtra) # To plot multiple ggplot graphs in a grid

plot_histogram_n_boxplot = function(variable, variableNameString, binw){
  h = ggplot(data = clinical, aes(x= variable))+
    labs(x = variableNameString,y ='count')+
    geom_histogram(fill = 'dark green',col = 'black',binwidth = binw)+
    geom_vline(aes(xintercept = mean(variable)),color="red", linetype="dashed", size=1)
    
  b = ggplot(data = clinical, aes('',variable))+ 
    geom_boxplot(outlier.colour = 'blue',col = 'red',outlier.shape = 19)+
    labs(x = '',y = variableNameString)+ coord_flip()
  grid.arrange(h,b,ncol = 1)
}
```
* 2.01. Histogram and boxplot visualizations for the Diagnosis Age variable
```{r}
plot_histogram_n_boxplot(clinical$Diagnosis_Age, 'Diagnosis Age', 1)
```
* 2.02. Histogram and boxplot visualizations for the Diagnosis Age Days variable
```{r}
ggplot(clinical, aes(x=Diagnosis_Age_Days)) + geom_density() + geom_vline(aes(xintercept = mean(Diagnosis_Age_Days)),color="red", linetype="dashed", size=1)
ggplot(clinical, aes(group=1, x=Diagnosis_Age_Days, outlier.colour = 'blue',col = 'blue',outlier.shape = 19)) + geom_boxplot()
```
* 2.03. Histogram and boxplot visualizations for the Cancer Stage variable
```{r}
plot_histogram_n_boxplot(clinical$Cancer_Stage, 'Cancer Stage', 1)
```
* 2.04. Histogram and boxplot visualizations for the Time To Event variable
```{r}
ggplot(clinical, aes(x=Time_To_Event)) + geom_density() + geom_vline(aes(xintercept = mean(Time_To_Event)),color="red", linetype="dashed", size=1)
ggplot(clinical, aes(group=1, x=Time_To_Event, outlier.colour = 'blue',col = 'blue',outlier.shape = 19)) + geom_boxplot()
```
* 2.05. Histogram and boxplot visualizations for the Ethnicity variable
```{r}
plot_histogram_n_boxplot(clinical$Ethnicity, 'Ethnicity', 1)
```
* 2.06. Histogram and boxplot visualizations for the Event Type variable
```{r}
plot_histogram_n_boxplot(clinical$Event_Type, 'Event Type', 1)
```
* 2.07. Histogram and boxplot visualizations for the Survival Days variable
```{r}
ggplot(clinical, aes(x=Survival_Days)) + geom_density() + geom_vline(aes(xintercept = mean(Survival_Days)),color="red", linetype="dashed", size=1)
ggplot(clinical, aes(group=1, x=Survival_Days, outlier.colour = 'blue',col = 'blue',outlier.shape = 19)) + geom_boxplot()
```
* 2.08. Histogram and boxplot visualizations for the Survival Months variable
```{r}
plot_histogram_n_boxplot(clinical$Survival_Months, 'Survival Months', 1)
```
* 2.09. Histogram and boxplot visualizations for the Race variable
```{r}
plot_histogram_n_boxplot(clinical$Race, 'Race', 1)
```
* 2.10. Histogram and boxplot visualizations for the Sex variable
```{r}
plot_histogram_n_boxplot(clinical$Sex, 'Sex', 1)
```
* 2.11. Histogram and boxplot visualizations for the TMB variable
```{r}
ggplot(clinical, aes(x=TMB)) + geom_density() + geom_vline(aes(xintercept = mean(TMB)),color="red", linetype="dashed", size=1)
ggplot(clinical, aes(group=1, x=TMB, outlier.colour = 'blue',col = 'blue',outlier.shape = 19)) + geom_boxplot()
```
* 2.12. Histogram and boxplot visualizations for the Survival Status variable
```{r}
plot_histogram_n_boxplot(clinical$Survival_Status, 'Survival Status', 1)
```

#### Bivariate Analysis for Features

* 3.01. We go on to plot percent stacked bar chart to see the effect of independent variables on the probability of a transaction being fraudulent.
* 3.02. Create function to draw percent stacked bar chart to see the effect of independent variables on the probability of Survival_Status using ggplot.

```{r}
library(ggplot2)

plot_stacked_barchart = function(variable, variableNameString){
  ggplot(Multiclinical, aes(fill = Survival_Status, x = variable)) + 
    geom_bar(position="fill")+
    labs(title = variableNameString, y = '', x = '')+
    scale_fill_manual(values=c("#0073C2FF", "#EFC000FF"))
}
```
* 3.03. Survival_Status vs Diagnosis Age
```{R}
plot_stacked_barchart(Multiclinical$Diagnosis_Age, 'Diagnosis Age')
```
* 3.04. Survival_Status vs Diagnosis Age Days
```{R}
plot_stacked_barchart(Multiclinical$Diagnosis_Age_Days, 'Diagnosis Age Days')
```
* 3.05. Survival_Status vs Cancer Stage
```{R}
plot_stacked_barchart(Multiclinical$Cancer_Stage, 'Cancer Stage')
```
* 3.06. Survival_Status vs Time To Event
```{R}
plot_stacked_barchart(Multiclinical$Time_To_Event, 'Time To Event')
```
* 3.07. Survival_Status vs Survival Days
```{R}
plot_stacked_barchart(Multiclinical$Survival_Days, 'Survival Days')
```
* 3.08. Survival_Status vs Survival Months
```{R}
plot_stacked_barchart(Multiclinical$Survival_Months, 'Survival Months')
```
* 3.09. Survival_Status vs Race
```{R}
plot_stacked_barchart(Multiclinical$Race, 'Race')
```
* 3.10. Survival_Status vs Sex
```{R}
plot_stacked_barchart(Multiclinical$Sex, 'Sex')
```
* 3.11. Survival_Status vs TMB
```{R}
plot_stacked_barchart(Multiclinical$TMB, 'TMB')
```

#### Multivariate Analysis For Features

* 3.12. Use the psych package to carry out multivariate analysis
* By default corr.test produces pairwise "Pearson" correlation matrix for the entire dataset
```{r}
library(psych) # multivariate analysis, FA and PCA
library(corrplot)

corr.test(clinical) # for all features
M = cor(clinical)
corrplot(M, method = 'number')
```

* 3.13. Correlation tests significance level for the dataset features.
```{r}
# Check the correlation significance levels of the features

# Clinical features
cor.test(clinical$Survival_Status,clinical$Diagnosis_Age)$p.value #significant
cor.test(clinical$Survival_Status,clinical$Diagnosis_Age_Days)$p.value #significant
cor.test(clinical$Survival_Status,clinical$Cancer_Stage)$p.value #significant
cor.test(clinical$Survival_Status,clinical$Time_To_Event)$p.value #significant
cor.test(clinical$Survival_Status,clinical$Ethnicity)$p.value #Not significant
cor.test(clinical$Survival_Status,clinical$Event_Type)$p.value #significant
cor.test(clinical$Survival_Status,clinical$Survival_Days)$p.value #significant
cor.test(clinical$Survival_Status,clinical$Survival_Months)$p.value #significant
cor.test(clinical$Survival_Status,clinical$Race)$p.value #Not significant
cor.test(clinical$Survival_Status,clinical$Sex)$p.value #Not significant
cor.test(clinical$Survival_Status,clinical$TMB)$p.value #significant
```
* There exists correlation significance between label the 8 out of 11 features in the dataset at p = 0.05.

### Machine Learning Analysis for Dataset
* 4.01. Load mlbench and caret packages respectively.
* 4.02. Convert integer values to numeric and ensure that dependent variable "Survival_Status" is of the factor class
```{r}
library(mlbench)
library(caret)

clinical$Survival_Status = as.factor(as.integer(clinical$Survival_Status))
```
* 4.03. Perform feature selection.
* 4.04. Feature Selection carried out using Boruta package.
```{r}
library(Boruta)

# Feature Selection operation
set.seed(7)
boruta = Boruta(clinical$Survival_Status~.,data = clinical, doTrace = 5, maxRuns = 100)
plot(boruta, las = 2, cex.axis = 0.5)

# reclassify features that were designated as tentative
bor = TentativeRoughFix(boruta)
plot(bor, las = 2, cex.axis = 0.5)

attStats(boruta)
print(bor)

attStats(bor)

# create object and limit features to the 8 confirmed features from the boruta analysis + the dependent variable

col_order = c("Diagnosis_Age", "Diagnosis_Age_Days", "Cancer_Stage", "Time_To_Event","Event_Type","Survival_Days","Survival_Months","TMB","Survival_Status")
#View(clinical)
clinical = clinical[, col_order]
str(clinical)
levels(clinical$Survival_Status)
```
* 4.05. Split the features into training and validation groups (objects)
* 4.06. Split out validation dataset
* 4.07. Do this by creating a list of 70% of the rows in the original dataset to serve as training set
```{r}
#Not a fraudulent transaction
Deceased = clinical[clinical$Survival_Status == 0,]

#A fraudulent transaction
Living = clinical[clinical$Survival_Status == 1,]

set.seed(7)
validationIndex = createDataPartition(clinical$Survival_Status, p=0.70, list = FALSE)

#select 30% of the data for validation

validation = clinical[-validationIndex,]

#use the remaining 70% of data to train the model
dataset = clinical[validationIndex,]

# Check that the distribution of the dependent variable is similar in train and test sets
prop.table(table(clinical$Survival_Status))
prop.table(table(dataset$Survival_Status))
prop.table(table(validation$Survival_Status))
```
* The derived figures show that the ratio of the "Living":"Died of Disease" was proportionally split.

* 4.08. Evaluate 5 Machine Learning Algorithms
* 4.09. Linear Algorithms - Logistic Regression (LG), Linear Discriminate Analysis (LDA), Naive Bayes (NB)
* 4.10. Non-Linear Algorithms - K-Nearest Neighbors (KNN), Classification and Regression Trees (CART) and Support Vector Machine (SVM)
* 4.11. ENSEMBLE METHODS: Random Forest (RF) and XGBoost (XGB)
* 4.12. Analysis is done using 10-fold cross validation with 3 repeats
```{r}
trainControl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
metric = "Accuracy"

#1 LG
set.seed(7)
fit.glm = train(Survival_Status~., data=dataset, method="glm", metric=metric, trControl=trainControl)

#2 LDA
set.seed(7)
fit.lda = train(Survival_Status~., data=dataset, method="lda", metric=metric, trControl=trainControl)

#3 NB
set.seed(7)
fit.naive_bayes = train(Survival_Status~., data=dataset, method="naive_bayes", metric=metric, trControl=trainControl)

#4 SVM
set.seed(7)
fit.svmLinear = train(Survival_Status~., data=dataset, method="svmLinear", metric=metric, trControl=trainControl)

#5 XGB
set.seed(7)
fit.xgbTree = train(Survival_Status~., data=dataset, method="xgbTree", metric=metric, trControl=trainControl)

#6 KNN
set.seed(7)
fit.knn = train(Survival_Status~., data=dataset, method="knn", metric=metric, trControl=trainControl)

#7 CART
set.seed(7)
fit.cart = train(Survival_Status~., data=dataset, method="rpart", metric=metric, trControl=trainControl)

#8 Random Forest
set.seed(7)
fit.rf = train(Survival_Status~., data=dataset, method="rf", metric=metric, trControl = trainControl)

# Compare algorithms
results = resamples(list(LG=fit.glm, LDA=fit.lda, NB=fit.naive_bayes, SVM=fit.svmLinear, XGB=fit.xgbTree, KNN=fit.knn, CART=fit.cart, RF=fit.rf))

                          
summary(results)
dotplot(results)
```
* Top three performing algorithms are XGB, RF and NB with mean accuracies of 0.9437, 0.9300 and 0.9285 respectively

* Tune the top three learning algorithms

* 4.13. Tuning for the XGBoost (XGB) learning algorithm
```{r}
trainControl = trainControl(method="repeatedcv", number=10, repeats=3) 
metric = "Accuracy"

#XGB
set.seed(7)
grid = expand.grid(
  nrounds = 100,
  max_depth = 6,
  eta = 0.3,
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)
fit.XGB_tuned = train(Survival_Status~.,data = dataset, method="xgbTree", metric=metric, tuneGrid=grid, trControl=trainControl)
fit.XGB_tuned
```
* Tuning did not improve the accuracy oh the XGB model.

* 4.14. Tuning for the Random Forest (RF) learning algorithm
```{r}
trainControl = trainControl(method="repeatedcv", number=10, repeats=3) 
metric = "Accuracy"

#RF
set.seed(7)
grid = expand.grid(mtry = seq(1,10, by=1))
fit.RF_tuned = train(Survival_Status~.,data = dataset, method="rf", metric=metric, tuneGrid=grid, trControl=trainControl)
fit.RF_tuned
```
* Tuning improved the accuracy of the RF model.

* 4.15. Tuning for the Naive Bayes (NB) learning algorithm
```{r}
trainControl = trainControl(method="repeatedcv", number=10, repeats=3) 
metric = "Accuracy"

#NB
set.seed(7)
grid = expand.grid(
  laplace = 1,
  usekernel = T,
  adjust = 1
)
fit.NB_tuned = train(Survival_Status~.,data = dataset, method="naive_bayes", metric=metric, tuneGrid=grid, trControl=trainControl)
fit.NB_tuned
```
* Tuning did not improve the accuracy of the NB model.

* 4.16. Finalize by building the best 3 Model
* 4.17. Make predictions and build confusion matrix for XGB Model
```{r}
xgbPredict = predict(fit.xgbTree, newdata = validation)
confusionMatrix(factor(xgbPredict), factor(validation$Survival_Status))
```
* 4.18. Make predictions and build confusion matrix for RF Model
```{r}
rfPredict = predict(fit.rf, newdata = validation)
confusionMatrix(factor(rfPredict), factor(validation$Survival_Status))
```
* 4.19. Make predictions and build confusion matrix for NB Model
```{r}
nbPredict = predict(fit.naive_bayes, newdata = validation)
confusionMatrix(factor(nbPredict), factor(validation$Survival_Status))
```

* Calculate ROC and PR for all 3 models
  
* 4.20. Calculate AUC(ROC and PR) for XGB Tuned Model
```{r}
library(PRROC)
sapply(xgbPredict, class)
sapply(validation, class)
pred = as.numeric(as.character(xgbPredict))
valid = as.character(as.factor(validation$Survival_Status))
valid = as.numeric(as.character(validation$Survival_Status))
prroc_obj = roc.curve(scores.class0 = pred, weights.class0 = valid, curve = TRUE)
prroc_obj1 = pr.curve(scores.class0 = pred, weights.class0 = NULL, curve = TRUE)
plot(prroc_obj)
plot(prroc_obj1)
```

* 4.21. Calculate AUC(ROC and PR) for RF Tuned Model
```{r}
library(PRROC)
sapply(rfPredict, class)
sapply(validation, class)
pred = as.numeric(as.character(rfPredict))
valid = as.character(as.factor(validation$Survival_Status))
valid = as.numeric(as.character(validation$Survival_Status))
prroc_obj = roc.curve(scores.class0 = pred, weights.class0 = valid, curve = TRUE)
prroc_obj1 = pr.curve(scores.class0 = pred, weights.class0 = NULL, curve = TRUE)
plot(prroc_obj)
plot(prroc_obj1)
```

* 4.22. Calculate AUC(ROC and PR) for NB Tuned Model
```{r}
library(PRROC)
sapply(nbPredict, class)
sapply(validation, class)
pred = as.numeric(as.character(nbPredict))
valid = as.character(as.factor(validation$Survival_Status))
valid = as.numeric(as.character(validation$Survival_Status))
prroc_obj = roc.curve(scores.class0 = pred, weights.class0 = valid, curve = TRUE)
prroc_obj1 = pr.curve(scores.class0 = pred, weights.class0 = NULL, curve = TRUE)
plot(prroc_obj)
plot(prroc_obj1)
```

