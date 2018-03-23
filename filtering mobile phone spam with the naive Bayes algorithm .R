#Step 1- collecting data

#data is collected from http://www.dt.fee.unicamp.br/~tiago/smsspamcollection/

#step 2 - exploring and preparing the data

sms_raw <- read.csv("sms_spam.csv", stringsAsFactors = FALSE)
str(sms_raw)
#sms_raw contains $ type: chr  "ham" "ham" "spam" "ham" ...
#$ text: chr  "Go until jurong point, crazy..

# converting type variable to categorical variable
sms_raw$type <- factor(sms_raw$type)

str(sms_raw$type) #Factor w/ 2 levels "ham","spam": 1 1 2 1 1 2 1 1 2 2
table(sms_raw$type)


#Data Preperation - Processing data for analysis
#text minining package for R
library(tm)
sms_corpus <- Corpus(VectorSource(sms_raw$text)) #creating corpus for storing sms(document)
print(sms_corpus)
inspect(sms_corpus[1:10])
#cleaning data
corpus_clean <- tm_map(sms_corpus,removeNumbers)
corpus_clean <- tm_map(corpus_clean,tolower)
corpus_clean <- tm_map(corpus_clean,removeWords,stopwords())
corpus_clean <- tm_map(corpus_clean,removePunctuation)
corpus_clean <- tm_map(corpus_clean,stripWhitespace)
inspect(corpus_clean[1:10])
# creating a sparse matrix
sms_dtm <- DocumentTermMatrix(corpus_clean)


#Data Preperation
#buidlding raw data frame
sms_raw_train <- sms_raw[1:4169,]
sms_raw_test <- sms_raw[4170:5572,]
#document-term matrix
sms_dtm_train <- sms_dtm[1:4169,]
sms_dtm_test <- sms_dtm[1:5572,]
#corpus
sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test <- corpus_clean[4170:5572]

prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))
#Ham are distributed equally over train and test data 

library(wordcloud)
wordcloud(sms_corpus_train, min.freq = 40,random.order = FALSE)
spam <- subset(sms_raw_train, type =="spam")
ham <- subset(sms_raw_train, type == "ham")

wordcloud(spam$text, max.words = 40, scale = c(3,0.5))
wordcloud(ham$text,max.words = 40, scale = c(3,0.5))

# Data Preperation
#finding Frequency

findFreqTerms(sms_dtm_train, 5)
sms_dict <- Dictionary(findFreqTerms(sms_dtm_train,5))
Dictionary <- function(x) {
  if( is.character(x) ) {
    return (x)
  }
  stop('x is not a character vector')
}

sms_train <- DocumentTermMatrix(sms_corpus_train,list(Dictionary(sms_dict)))
sms_test <- DocumentTermMatrix(sms_corpus_test,list(Dictionary(sms_dict)))
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
  return(x)
}


sms_train <- apply(sms_train,MARGIN = 2,convert_counts)
sms_test <- apply(sms_test, MARGIN = 2,convert_counts)


#Training the model on data
library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)

sms_test_pred <-predict(sms_classifier,sms_test)
library(gmodels)

CrossTable(sms_test_pred, sms_raw_test$type,prop.chisq = FALSE,prop.t = FALSE,dnn = c("predicted","actual"))
