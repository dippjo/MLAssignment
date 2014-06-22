APPLIED ML ASSIGNMENT - June 2014
========================================================

INTRODUCTION:

Regular physical activity is an important component to improving one's health and well-being, reducing burdensome chronic diseases such as coronary heart disease, type 2 diabetes obesity. Many people around the world are actively engaged in regular exercise and weight training regimes, but no always using the correct technique. Free weights exercises account for the majority of injuries in this exercise regime in the United States, with people at risk of fractures and dislocations from incorrectly using weights machines.

Exercise feedback has been developed by big brands like Nike (i.e. Nike FuelBand) amongst others to collect volumes of personal activity data. However, a study by Velloso et al (2013) (1) investigated the feasibility of automatically assessing the quality of how weights training exercises were performed with the potential to providing athletes with a real-time feedback mechanism called qualitative activity recognition. The authors of this study have kindly allowed the use of their data for this analysis.

METHODS:

Data from accelerometers on the belt, forearm, arm and dumbbell of six athletes was used in order to predict the manner in which they did the exercise. The participants were asked to perform barbell lifts correctly and incorrectly in five different ways (classe): 

A.  Correctly, exactly to the specification

B.	Throwing the elbows to the front

C.	Lifting the dumbbell only halfway

D.	Lowering the dumbbell only halfway

E.	Throwing the hips to the front


The actual frequence and plot of the distribution in the total sample for each classe is below: 

```r
PMLAss <- read.table("C:/PhD/PracticalMachineLearning-Coursera/Assignment/R-data/Applied-ML-Course/pml-training.csv", header=TRUE, sep=",", row.names=1)

summary(PMLAss$classe)
```

```
##    A    B    C    D    E 
## 5580 3797 3422 3216 3607
```

```r
plot(PMLAss$classe)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

As seen by the plot the outcome variable was categorical so classification algorithms were used to get the best predictions of classe.

METHODS:

Two machine learning algorithms were used. For this reason the initial data file was split into a training set and test set of the ratio 70% training and 30% test. Due to the outcome variable bein categorical, accuracy was used to assess the performance of each model. Out of sample error on the test data was expected to be higher than the in sample error on the training data.   

Exploratory analysis of the available features revealed that many of the features were redundant with very high missing data and/or near zero variance.

There were clear patterns in the data across each classe as can be seen by the plot below:


```r
PMLAss <- read.table("C:/PhD/PracticalMachineLearning-Coursera/Assignment/R-data/Applied-ML-Course/pml-training.csv", header=TRUE, sep=",", row.names=1)
library(caret); library(ggplot2);
```

```
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
inTrain <- createDataPartition(y=PMLAss$classe, p=0.70, list=FALSE)
#subset into train & test sets
trainingAss <- PMLAss[inTrain,]
testingAss <- PMLAss[-inTrain,]
```

```r
qplot(roll_belt,pitch_belt, data=trainingAss, color=classe)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

The final features used were:

- roll_belt+pitch_belt
- yaw_belt+total_accel_belt
- gyros_belt_x
- gyros_belt_y
- gyros_belt_z
- accel_belt_x
- accel_belt_y
- accel_belt_z
- magnet_belt_x
- magnet_belt_y
- magnet_belt_z
- roll_arm+pitch_arm
- yaw_arm
- total_accel_arm
- gyros_arm_x
- gyros_arm_y
- gyros_arm_z
- accel_arm_x
- accel_arm_y
- accel_arm_z
- magnet_arm_x
- magnet_arm_y
- magnet_arm_z
- roll_dumbbell
- pitch_dumbbell
- yaw_dumbbell
- gyros_dumbbell_x
- gyros_dumbbell_y
- gyros_dumbbell_z
- accel_dumbbell_x
- accel_dumbbell_y
- accel_dumbbell_z
- magnet_dumbbell_x
- magnet_dumbbell_y
- magnet_dumbbell_z
- roll_forearm
- pitch_forearm
- yaw_forearm
- total_accel_forearm
- gyros_forearm_x
- gyros_forearm_y
- gyros_forearm_z
- accel_forearm_x
- accel_forearm_y
- accel_forearm_z
- magnet_forearm_x
- magnet_forearm_y
- magnet_forearm_z

All features were centered and scaled.

Two models were fitted:

1 - Linear Discrinant Analysis (LDA)

2 - Decision Tree (DT)

The results from the LDA model on the test data is as follows: 


```r
PMLAss <- read.table("C:/PhD/PracticalMachineLearning-Coursera/Assignment/R-data/Applied-ML-Course/pml-training.csv", header=TRUE, sep=",", row.names=1)
library(caret); library(ggplot2);
inTrain <- createDataPartition(y=PMLAss$classe, p=0.70, list=FALSE)
#subset into train & test sets
trainingAss <- PMLAss[inTrain,]
testingAss <- PMLAss[-inTrain,]

#LDA
set.seed(32331)
modelFitLDA <-train(classe ~ roll_belt+pitch_belt+yaw_belt+total_accel_belt
                    +gyros_belt_x+gyros_belt_y+gyros_belt_z
                    +accel_belt_x+accel_belt_y+accel_belt_z
                    +magnet_belt_x+magnet_belt_y+magnet_belt_z
                    +roll_arm+pitch_arm+yaw_arm+total_accel_arm
                    +gyros_arm_x+gyros_arm_y+gyros_arm_z
                    +accel_arm_x+accel_arm_y+accel_arm_z
                    +magnet_arm_x+magnet_arm_y+magnet_arm_z
                    +roll_dumbbell+pitch_dumbbell+yaw_dumbbell
                    +gyros_dumbbell_x+gyros_dumbbell_y+gyros_dumbbell_z
                    +accel_dumbbell_x+accel_dumbbell_y+accel_dumbbell_z
                    +magnet_dumbbell_x+magnet_dumbbell_y+magnet_dumbbell_z
                    +roll_forearm+pitch_forearm+yaw_forearm+total_accel_forearm
                    +gyros_forearm_x+gyros_forearm_y+gyros_forearm_z
                    +accel_forearm_x+accel_forearm_y+accel_forearm_z
                    +magnet_forearm_x+magnet_forearm_y+magnet_forearm_z
                    , data=trainingAss, 
                    preProcess=c("center", "scale"), 
                    method="lda")
```

```
## Loading required package: MASS
```

```r
modelFitLDA
```

```
## Linear Discriminant Analysis 
## 
## 13737 samples
##   158 predictors
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## Pre-processing: centered, scaled 
## Resampling: Bootstrapped (25 reps) 
## 
## Summary of sample sizes: 13737, 13737, 13737, 13737, 13737, 13737, ... 
## 
## Resampling results
## 
##   Accuracy  Kappa  Accuracy SD  Kappa SD
##   0.7       0.6    0.006        0.007   
## 
## 
```

```r
pFitLDA <- predict(modelFitLDA, newdata=testingAss)
confusionMatrix(pFitLDA, testingAss$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1333  179   92   66   42
##          B   61  743  108   42  190
##          C  143  124  647  101  116
##          D  127   34  149  707   92
##          E   10   59   30   48  642
## 
## Overall Statistics
##                                        
##                Accuracy : 0.692        
##                  95% CI : (0.68, 0.704)
##     No Information Rate : 0.284        
##     P-Value [Acc > NIR] : <2e-16       
##                                        
##                   Kappa : 0.61         
##  Mcnemar's Test P-Value : <2e-16       
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity             0.796    0.652    0.631    0.733    0.593
## Specificity             0.910    0.916    0.900    0.918    0.969
## Pos Pred Value          0.779    0.649    0.572    0.638    0.814
## Neg Pred Value          0.918    0.916    0.920    0.946    0.914
## Prevalence              0.284    0.194    0.174    0.164    0.184
## Detection Rate          0.227    0.126    0.110    0.120    0.109
## Detection Prevalence    0.291    0.194    0.192    0.188    0.134
## Balanced Accuracy       0.853    0.784    0.765    0.826    0.781
```

The results from the DT model on the test data is as follows: 


```r
#DT
set.seed(32333)
modelFitDT <-train(classe ~ roll_belt+pitch_belt+yaw_belt+total_accel_belt
                    +gyros_belt_x+gyros_belt_y+gyros_belt_z
                    +accel_belt_x+accel_belt_y+accel_belt_z
                    +magnet_belt_x+magnet_belt_y+magnet_belt_z
                    +roll_arm+pitch_arm+yaw_arm+total_accel_arm
                    +gyros_arm_x+gyros_arm_y+gyros_arm_z
                    +accel_arm_x+accel_arm_y+accel_arm_z
                    +magnet_arm_x+magnet_arm_y+magnet_arm_z
                    +roll_dumbbell+pitch_dumbbell+yaw_dumbbell
                    +gyros_dumbbell_x+gyros_dumbbell_y+gyros_dumbbell_z
                    +accel_dumbbell_x+accel_dumbbell_y+accel_dumbbell_z
                    +magnet_dumbbell_x+magnet_dumbbell_y+magnet_dumbbell_z
                    +roll_forearm+pitch_forearm+yaw_forearm+total_accel_forearm
                    +gyros_forearm_x+gyros_forearm_y+gyros_forearm_z
                    +accel_forearm_x+accel_forearm_y+accel_forearm_z
                    +magnet_forearm_x+magnet_forearm_y+magnet_forearm_z
                    , data=trainingAss, 
                    preProcess=c("center", "scale"), 
                    method="rpart")
```

```
## Loading required package: rpart
```

```r
modelFitDT
```

```
## CART 
## 
## 13737 samples
##   158 predictors
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## Pre-processing: centered, scaled 
## Resampling: Bootstrapped (25 reps) 
## 
## Summary of sample sizes: 13737, 13737, 13737, 13737, 13737, 13737, ... 
## 
## Resampling results across tuning parameters:
## 
##   cp    Accuracy  Kappa  Accuracy SD  Kappa SD
##   0.03  0.5       0.4    0.02         0.04    
##   0.06  0.4       0.2    0.06         0.1     
##   0.1   0.3       0.07   0.04         0.06    
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was cp = 0.03.
```

```r
pFitDT <- predict(modelFitDT, newdata=testingAss)
confusionMatrix(pFitDT, testingAss$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1508  477  473  440  164
##          B   26  389   30  168  137
##          C  136  273  523  356  276
##          D    0    0    0    0    0
##          E    4    0    0    0  505
## 
## Overall Statistics
##                                        
##                Accuracy : 0.497        
##                  95% CI : (0.484, 0.51)
##     No Information Rate : 0.284        
##     P-Value [Acc > NIR] : <2e-16       
##                                        
##                   Kappa : 0.343        
##  Mcnemar's Test P-Value : NA           
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity             0.901   0.3415   0.5097    0.000   0.4667
## Specificity             0.631   0.9239   0.7858    1.000   0.9992
## Pos Pred Value          0.492   0.5187   0.3344      NaN   0.9921
## Neg Pred Value          0.941   0.8539   0.8836    0.836   0.8927
## Prevalence              0.284   0.1935   0.1743    0.164   0.1839
## Detection Rate          0.256   0.0661   0.0889    0.000   0.0858
## Detection Prevalence    0.520   0.1274   0.2658    0.000   0.0865
## Balanced Accuracy       0.766   0.6327   0.6478    0.500   0.7329
```


The best model chose was the one with the lowest out of sample accuracy being the LDA model (i.e. 70% accuracy for LDA versus 50% accuracy for DT). The positive and negative predictive values for all classes was highest for LDA.

However, it is acknowledged that the data may have a user-bias as there were bimodal distributions for many of the predictor for each classe so another model by be worth investigating.


ACKNOWLEDGEMENTS:

Thanks to the availability of the data for this assignment and lecture notes and videos from the course "Practical Machine Learning", by Jeff Leek, PhD, Brian Caffo, PhD, Roger D. Peng, PhD.

REFERENCE:
(1)  Velloso et al, "Qalitative Activity Recognition of Weight Lifting Exercises." Proceedings of 4th International Conference in Cooperation with SIGCHI (Augmented Human '13), Stuttgart, Germany: ACM SIGCHI, 2013. http://www.groupware.les.inf.puc-rio.br/har)
