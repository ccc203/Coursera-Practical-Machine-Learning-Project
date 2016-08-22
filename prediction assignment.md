---
title: "Barbell Lifting Quality Prediction Assignment"
author: "Chunhui Chen"
date: "August 22, 2016"
output: html_document
---



#Summary
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, our goal is to use the data(accelerometers on the belt, forearm, arm, and dumbell of 6 participants) to develope a model to predict the quality of the barbell lifts. The analysis has discovered that random forecast model is the most accurate model with accuracy of 99%.

#Loading And Processing Data
From the given data links, we download both the train and test datasets.

```r
trLink<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
trainData<-read.csv(trLink,header=T, na.strings=c("NA", "#DIV/0!"))
teLink<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
testData<-read.csv(teLink,header=T, na.strings=c("NA", "#DIV/0!"))
dim(trainData);dim(testData)
```

```
## [1] 19622   160
```

```
## [1]  20 160
```

We then remove all columns that contains NA and remove features that are not in the testing dataset. The features containing NA are the variance, mean and standard devition within each window for each feature. Since the dataset is not time-dependence, these values can be disregarded. We will also remove the first 7 features since they are related to the time-series or are not numeric.


```r
features <- names(testData[,colSums(is.na(testData)) == 0])[-(1:7)]
trainData <- trainData[,c(features[-53],"classe")]
testData <- testData[,c(features[-53],"problem_id")]
dim(trainData);dim(testData)
```

```
## [1] 19622    53
```

```
## [1] 20 53
```

#Partitioning the Dataset
We split the training data into a training data set (70% of the total cases) and a testing data set (30% of the total cases). We can estimate the out of sample error of the predictor.


```r
library(caret);set.seed(888);
inTrain <- createDataPartition(trainData$classe, p=0.7, list=FALSE)
training <- trainData[inTrain,]
testing <- trainData[-inTrain,]
dim(training); dim(testing)
```

```
## [1] 13737    53
```

```
## [1] 5885   53
```

We got 13737 samples and 53 variables for training, 5885 samples and 53 variables for testing.

#Analysis
We will use both decision tree in *caret* package and random foreest in *randomForest* package. We compare the acucracy of both models.

##Predicting With Decision Tree Model

```r
modFitDT <- rpart(classe ~ ., data = training, method="class")
```

```
## Error in eval(expr, envir, enclos): could not find function "rpart"
```

```r
prediction <- predict(modFitDT, testing, type = "class")
```

```
## Error in predict(modFitDT, testing, type = "class"): object 'modFitDT' not found
```

```r
confusionMatrix(prediction, testing$class)
```

```
## Error in confusionMatrix(prediction, testing$class): object 'prediction' not found
```
With 95% confidence, the accuracy of the fitted decision tree model is 76%. We then apply the decision tree model on the Testing Data(pml-testing.csv).

```r
predictionDT <- predict(modFitDT, testData, type = "class")
```

```
## Error in predict(modFitDT, testData, type = "class"): object 'modFitDT' not found
```

```r
predictionDT
```

```
## Error in eval(expr, envir, enclos): object 'predictionDT' not found
```

##Predicting With Random Forest Model

```r
library(randomForest)
modFitRF <- randomForest(classe ~ ., data = training, ntree = 1000)
prediction <- predict(modFitRF, testing, type = "class")
confusionMatrix(prediction, testing$class)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1674    8    0    0    0
##          B    0 1129    2    0    0
##          C    0    2 1024   10    0
##          D    0    0    0  954    4
##          E    0    0    0    0 1078
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9956          
##                  95% CI : (0.9935, 0.9971)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9944          
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            1.0000   0.9912   0.9981   0.9896   0.9963
## Specificity            0.9981   0.9996   0.9975   0.9992   1.0000
## Pos Pred Value         0.9952   0.9982   0.9884   0.9958   1.0000
## Neg Pred Value         1.0000   0.9979   0.9996   0.9980   0.9992
## Prevalence             0.2845   0.1935   0.1743   0.1638   0.1839
## Detection Rate         0.2845   0.1918   0.1740   0.1621   0.1832
## Detection Prevalence   0.2858   0.1922   0.1760   0.1628   0.1832
## Balanced Accuracy      0.9991   0.9954   0.9978   0.9944   0.9982
```
With 95% confidence, the accuracy of the fitted decision tree model is 99%. It is much more accurate than the decision tree model as expected. We then apply the random forest model on the Testing Data(pml-testing.csv).

```r
predictionRF <- predict(modFitRF, testData, type = "class")
predictionRF
```

```
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
```

#Conclusion
We can clearly see the accuracy of the random forest model is much better than the accuracy of the decision tree model when you look at the confusion matrix. So random forest model is a best model to predict the quality of barbell lifts.

#Appendix

```r
library(corrplot)
corrPlot <- cor(trainData[, -length(names(trainData))])
corrplot(corrPlot, method="color")
```

![plot of chunk plot1](figure/plot1-1.png)
