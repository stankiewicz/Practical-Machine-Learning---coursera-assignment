# Classe Prediction - Activity Tracker Data Prediction
Radoslaw Stankiewicz  
Tuesday, October 21, 2014  

##Background
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this analysis, I use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset). 

##Loading and cleaning data
First,I load required libraries and set seed for correct reproducibility.

```r
require(caret)
require(randomForest)
set.seed(3433)
```
I read data, If file is missing, let me download it from correct place.
The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har. 

```r
if(!file.exists("pml-training.csv")){
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", "pml-training.csv")
}
pmltraining <- read.csv("pml-training.csv")
```
While reviewing summary(pmltraining) I find that a lot of variables are mostly empty, for example kurtosis_*. I have choses 52 variables plus classe variable which vary accross observations. 

```r
data1 <- subset(pmltraining, select=c(roll_belt,pitch_belt,yaw_belt,total_accel_belt,gyros_belt_x,gyros_belt_y,gyros_belt_z,accel_belt_x,accel_belt_y,accel_belt_z,magnet_belt_x,magnet_belt_y,magnet_belt_z,roll_arm,pitch_arm,yaw_arm,total_accel_arm,gyros_arm_x,gyros_arm_y,gyros_arm_z,accel_arm_x,accel_arm_y,accel_arm_z,magnet_arm_x,magnet_arm_y,magnet_arm_z,roll_dumbbell,pitch_dumbbell,yaw_dumbbell,total_accel_dumbbell,gyros_dumbbell_x,gyros_dumbbell_y,gyros_dumbbell_z,accel_dumbbell_x,accel_dumbbell_y,accel_dumbbell_z,magnet_dumbbell_x,magnet_dumbbell_y,magnet_dumbbell_z,roll_forearm,pitch_forearm,yaw_forearm,total_accel_forearm,gyros_forearm_x,gyros_forearm_y,gyros_forearm_z,accel_forearm_x,accel_forearm_y,accel_forearm_z,magnet_forearm_x,magnet_forearm_y,magnet_forearm_z,classe))
```
I split our training data into training and testing parts. Proportions are standard, 75% of data is training data, 25% is validation data.

```r
inTrain <- createDataPartition(data1$classe, p = 3/4, list=FALSE)
training <- data1[ inTrain,]
testing <- data1[-inTrain,]
```

##Learning
As a predictor I'm choosing random forest as it is very accurate algorithm. It deals well with non linear data, it may generate model quite long time, but results may be very promising.

```r
modFit <- randomForest(classe ~ . , data = training, method ="rf", ntree=1024)
modFit
```

```
## 
## Call:
##  randomForest(formula = classe ~ ., data = training, method = "rf",      ntree = 1024) 
##                Type of random forest: classification
##                      Number of trees: 1024
## No. of variables tried at each split: 7
## 
##         OOB estimate of  error rate: 0.45%
## Confusion matrix:
##      A    B    C    D    E class.error
## A 4181    4    0    0    0   0.0009558
## B    9 2833    6    0    0   0.0052669
## C    0   15 2551    1    0   0.0062330
## D    0    0   22 2389    1   0.0095357
## E    0    0    3    5 2698   0.0029564
```
Error rate is very low, confusion matrix looks promising.
I validate it with our testing data.

```r
predicted <- predict(modFit,testing[-53])
confusionMatrix(testing$classe,predicted)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1395    0    0    0    0
##          B    4  944    1    0    0
##          C    0    2  853    0    0
##          D    0    0    4  799    1
##          E    0    0    0    5  896
## 
## Overall Statistics
##                                         
##                Accuracy : 0.997         
##                  95% CI : (0.994, 0.998)
##     No Information Rate : 0.285         
##     P-Value [Acc > NIR] : <2e-16        
##                                         
##                   Kappa : 0.996         
##  Mcnemar's Test P-Value : NA            
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity             0.997    0.998    0.994    0.994    0.999
## Specificity             1.000    0.999    1.000    0.999    0.999
## Pos Pred Value          1.000    0.995    0.998    0.994    0.994
## Neg Pred Value          0.999    0.999    0.999    0.999    1.000
## Prevalence              0.285    0.193    0.175    0.164    0.183
## Detection Rate          0.284    0.192    0.174    0.163    0.183
## Detection Prevalence    0.284    0.194    0.174    0.164    0.184
## Balanced Accuracy       0.999    0.998    0.997    0.996    0.999
```
Model has more than 99% accuracy. It is difficult to interpret how this tree look likes (running head(getTree(modFit,k=2)), checking variable importance etc). But still - I think I can safely apply it to 20 test cases.

##Answers
As I have well trained model with very good accuracy, I can now load assignment data and try to predict classe variable.

```r
if(!file.exists("pml-testing.csv")){
   download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", "pml-testing.csv")
}
```


```r
pmltesting <- read.csv("pml-testing.csv")
answers <- predict(modFit,pmltesting)
```
I save answers using snippet provided on site.

```r
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(answers)
```
##Summary
Random forest algorithm performed very well. It predicted data properly.
