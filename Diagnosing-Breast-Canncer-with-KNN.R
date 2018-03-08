#This task has work related to diagnosing breast cancer with KNN algorithm
#Step 1  -  collecting data 
#Data source - https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/

#Step 2 exploring and preparing the data
wbcd <-read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"), header=FALSE) 
names(wbcd) <- c("ID","Diagnosis","radius_mean","texture_mean","perimeter_mean","area_mean","smoothness","compactness","concavity","concave_points","symmetry","fractal_dimension")
str(wbcd) # confirming the data
wbcd <- wbcd[-1] # Dropping the first id coulumn

str(wbcd)

table(wbcd$Diagnosis) # checking the v2(diagnostic) Benign or malignant
wbcd$V2 <- factor(wbcd$Diagnosi, levels = c("B","M"), labels = c("Benign","Malignant")) # recoding the diagnosis variable
round(prop.table(table(wbcd$V2)) * 100,  digits = 1)  #checking percentile

summary(wbcd[c("radius_mean", "area_mean", "smoothness")] # getting summary for these features, 
        
# looking at the min values of each individual feature we can state that impact of the area_mean feture is going to be much larger than 
# sommthness, Hence there is need of transformation of the data


#Transforming the data-  Normalizing the numeric data.

normalize <- function(x){return((x-min(x)) / (max(x)-min(x)))} 
normalize(c(1,2,3,4,5))
normalize(c(10,20,30,40,50))   # Function for normalizing performing well


#applying this function to  out data frame using lapply
wbcd_n <- as.data.frame(lapply(wbcd[2:31],normalize))

summary(wbcd_n$area_mean) #now every feature is normalized 

# Data preperation -  Creating traing and test data set.
wbcd_train = wbcd_n[1:469, ]
wbcd_test = wbcd_n[470:569, ]

wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_lables <- wbcd[470:569, 1]


#Step 3 - Training a model on the data (class package required for implementing KNN )
library(class)
cl = wbcd[1:469,1]
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl, k=21)

#Evaluluating the model performance
library(gmodels)
CrossTable(x= cl, y = wbcd_test_pred, prop.chisq = FALSE)


#Step 4 Improving the Model Performance

wbcd_z <- as.data.frame(scale(wbcd[-1]))
summary(wbcd_z$area_mean)
wbcd_train = wbcd_n[1:469, ]
wbcd_test = wbcd_n[470:569, ]

wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_lables <- wbcd[470:569, 1]
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl, k=21)
CrossTable(x= cl, y = wbcd_test_pred, prop.chisq = FALSE)


