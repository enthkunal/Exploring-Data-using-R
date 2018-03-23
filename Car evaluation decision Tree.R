#Data import from UCI machine learning Repo
#Importing Liabrries
library(caret)
library(rpart.plot)
#Data Import 
#getting data f
data_url <- c("https://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data")
download.file(url = data_url, destfile = "car.data")

car_df <- read.csv("car.data", sep = ',', header = FALSE) #reading data

#step 2 - data evaluation

head(car_df)

set.seed(3033)

intrain <- createDataPartition(y = car_df$V7, p= 0.7, list = FALSE)
training <- car_df[intrain,]
testing <- car_df[-intrain,]

#check dimensions of train & test set
dim(training)
dim(testing)

anyNA(car_df) #no missing values
summary(car_df)

#implementing tree
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)
dtree_fit <- train(V7 ~., data = training, method = "rpart",
                   parms = list(split = "information"),
                   trControl=trctrl,
                   tuneLength = 10)

dtree_fit

#Ploting Tree
prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2)

testing[1,]


predict(dtree_fit, newdata = testing[1,])


test_pred <- predict(dtree_fit, newdata = testing)
confusionMatrix(test_pred,testing$V7) # checking accuracy


#Training Decision tree
trctrl <- trainControl(method = "repeatedcv", number=10,repeats = 3)
set.seed(3333)

dtree_fit_gini <- train(V7 ~., data = training, method = "rpart",
                   parms = list(split = "gini"),
                   trControl=trctrl,
                   tuneLength = 10)

dtree_fit_gini

prp(dtree_fit_gini$finalModel, box.palette = "Blues", tweak = 1.2)
testing[1,]

test_pred_gini <- predict(dtree_fit_gini, newdata = testing)
confusionMatrix(test_pred_gini, testing$V7 )
