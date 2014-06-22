Practical Machine Learning Project By RRK
========================================================

Read the data files into pmlTraining and pmlTesting:

```r
setwd("~/Box Sync/Coursera/PracticalMachineLearning/project")
pmlTraining<-read.csv("pml-training.csv") 
pmlTesting<-read.csv("pml-testing.csv") 
```

Clean the data from columns that have more than 19,000 NA

```r
#training
pmlIsNATraining<-is.na(pmlTraining)
arrSumNATraining<-apply(pmlIsNATraining,2,sum) #for each column, how many are NA
bIsNAColumnTraining<-(arrSumNATraining>19000)
pmlTrainingClean<-pmlTraining[,!bIsNAColumnTraining]
#testing
pmlTestingClean<-pmlTesting[,!bIsNAColumnTraining]
```

Clean the data from columns that have more than 19,000 ""

```r
#training
pmlEmptyTraining<-pmlTrainingClean==""
arrSumEmptyTraining<-apply(pmlEmptyTraining,2,sum)
bIsEmptyColumnTraining<-(arrSumEmptyTraining>19000)
pmlTrainingClean<-pmlTrainingClean[,!bIsEmptyColumnTraining]
#testing
pmlTestingClean<-pmlTestingClean[,!bIsEmptyColumnTraining]
```
After clearning the data, 60 variables remained:

```r
names(pmlTrainingClean)
```

```
##  [1] "X"                    "user_name"            "raw_timestamp_part_1"
##  [4] "raw_timestamp_part_2" "cvtd_timestamp"       "new_window"          
##  [7] "num_window"           "roll_belt"            "pitch_belt"          
## [10] "yaw_belt"             "total_accel_belt"     "gyros_belt_x"        
## [13] "gyros_belt_y"         "gyros_belt_z"         "accel_belt_x"        
## [16] "accel_belt_y"         "accel_belt_z"         "magnet_belt_x"       
## [19] "magnet_belt_y"        "magnet_belt_z"        "roll_arm"            
## [22] "pitch_arm"            "yaw_arm"              "total_accel_arm"     
## [25] "gyros_arm_x"          "gyros_arm_y"          "gyros_arm_z"         
## [28] "accel_arm_x"          "accel_arm_y"          "accel_arm_z"         
## [31] "magnet_arm_x"         "magnet_arm_y"         "magnet_arm_z"        
## [34] "roll_dumbbell"        "pitch_dumbbell"       "yaw_dumbbell"        
## [37] "total_accel_dumbbell" "gyros_dumbbell_x"     "gyros_dumbbell_y"    
## [40] "gyros_dumbbell_z"     "accel_dumbbell_x"     "accel_dumbbell_y"    
## [43] "accel_dumbbell_z"     "magnet_dumbbell_x"    "magnet_dumbbell_y"   
## [46] "magnet_dumbbell_z"    "roll_forearm"         "pitch_forearm"       
## [49] "yaw_forearm"          "total_accel_forearm"  "gyros_forearm_x"     
## [52] "gyros_forearm_y"      "gyros_forearm_z"      "accel_forearm_x"     
## [55] "accel_forearm_y"      "accel_forearm_z"      "magnet_forearm_x"    
## [58] "magnet_forearm_y"     "magnet_forearm_z"     "classe"
```



