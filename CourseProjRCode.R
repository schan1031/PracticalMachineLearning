library(caret)
library(rattle)
library(rpart)
library(randomForest)
library(ipred)
set.seed(12345)

traindata <- read.csv('pml-training.csv')
testdata <- read.csv('pml-testing.csv')

# Clean the Data from Columns of NAs and Blanks
headerindex <- c(4, 7:11, 37:49, 60:68, 84:86)

traindata2 <- traindata[,headerindex]
traindata2$class <- traindata$classe

testdata2 <- testdata[,headerindex]

# Split the Training Data into Temporary Training and Test Sets

train <- createDataPartition(traindata2$class, p = 0.75, list = FALSE)
training <- traindata2[train,]
testing <- traindata2[-train,]

# Trees Model

modtree <- train(class ~. , method = "rpart", data = training)
predicttree <- predict(modtree, testing)
print(confusionMatrix(predicttree, testing$class))


# Random Forest Model

model <- randomForest(class ~. , data = training)
head(getTree(model, 1, labelVar=TRUE),20)

# Use Model to predict on the Allocated Test Set

predictions <- predict(model, testing, type = "class")

# Create Confusion Matrix to Check Accuracy of Model

print(confusionMatrix(predictions, testing$class))


# Use Random Forest Model on the Real Test Data

finalpredictions <- predict(model, testdata2, type = "class")


