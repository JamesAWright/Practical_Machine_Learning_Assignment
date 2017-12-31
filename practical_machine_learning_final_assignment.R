library(caret)
library(randomForest)

# Read training data

path_to_file = paste("C://Users/james/OneDrive/Documents/Data Science/",
            "Data Science Online Course/8.Practical_Machine_Learning/Assignment",
            sep="")

traindata = read.csv(paste(path_to_file,'pml-training.csv',sep="/"))

keepcols = c("classe","roll_belt","pitch_belt","yaw_belt","gyros_belt_x","gyros_belt_y","gyros_belt_z","accel_belt_x","accel_belt_y","accel_belt_z","magnet_belt_x","magnet_belt_y","magnet_belt_z","roll_arm","pitch_arm","yaw_arm","gyros_arm_x","gyros_arm_y","gyros_arm_z","accel_arm_x","accel_arm_y","accel_arm_z","magnet_arm_x","magnet_arm_y","magnet_arm_z","roll_dumbbell","pitch_dumbbell","yaw_dumbbell","gyros_dumbbell_x","gyros_dumbbell_y","gyros_dumbbell_z","accel_dumbbell_x","accel_dumbbell_y","accel_dumbbell_z","magnet_dumbbell_x","magnet_dumbbell_y","magnet_dumbbell_z","roll_forearm","pitch_forearm","yaw_forearm","gyros_forearm_x","gyros_forearm_y","gyros_forearm_z","accel_forearm_x","accel_forearm_y","accel_forearm_z","magnet_forearm_x","magnet_forearm_y","magnet_forearm_z")

traindata = traindata[,keepcols]

# Split into train and validation data

inTrain <- createDataPartition(y=traindata$classe,p=0.7,list=FALSE)
train_set <- traindata[inTrain,]
val_set <- traindata[-inTrain,]

# See how many observations are in each class

xtabs(~ classe, data=train_set)

# Fit random forest classifier

set.seed(415)
clf <- randomForest(as.factor(classe) ~ ., data=train_set, ntree=100, 
                    importance=TRUE)

# Validate

predicted <- predict(clf, val_set)

val_res <- confusionMatrix(data = predicted, reference = val_set$classe)


# Get test data

testdata = read.csv(paste(path_to_file,'pml-testing.csv',sep="/"))

# Make predictions

predicted <- predict(clf, testdata)


