############################ Data ###################################

#loading the required libraries and the data.

#install.packages("readr")
#install.packages("kernalab")
#install.packages("SnowballC")

library(readr)
library(dplyr)
library(readxl)

#Text mining packages
library(tm)
library(SnowballC)
t1 <-read_excel("/Users/user/Desktop/testdemo.xlsx")
glimpse(t1)# print the dataSet

############################Preparing Data for Modeling ###################################
 #Step 1 - Create the Text Corpus

corpus = Corpus(VectorSource(t1$Text))# A corpus is a collection of documents(texts)
corpus[[1]][1]#show the 1st document(text)
t1$Sentiment[1]


#Step 2 - Conversion to Lowercase


corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, tolower)
corpus[[1]][1] 

#Step 3 - Removing Punctuation(!,#<$΄)

corpus = tm_map(corpus, removePunctuation)
corpus[[1]][1]

 #Step 4 - Removing Stopwords
corpus = tm_map(corpus, removeWords, c("cloth", stopwords("english")))
corpus[[1]][1] 

 #Step 5 - Stemming
corpus = tm_map(corpus, stemDocument)
corpus[[1]][1]  

###################### Create Document Term Matrix #######################

# step 1 Create Document Term Matrix

frequencies = DocumentTermMatrix(corpus)# colect the frequences
#frequencies
sparse = removeSparseTerms(frequencies, 0.995)#remove the words with many 0 frequency
#sparse

tSparse = as.data.frame(as.matrix(sparse))#convert the matrix into a data frame
#tSparse
colnames(tSparse) = make.names(colnames(tSparse))#makes all the variable names R-friendly,

tSparse$recommended_id = t1$Sentiment#adds the dependent variable to the data set
tSparse$recommended_id[1:200]
prop.table(table(tSparse$recommended_id)) #prints the proportion(ποσοστο) of the labels in the target variable, 'recommended_id'.

#Creating Training and Test Data for Machine Learning
#install.packages("caTools")
library(caTools)

######## split##############################
trainSparse = tSparse#create the training ('trainSparse') dataset
testSparse=tSparse
testSparse <- testSparse[-1:-150,]
trainSparse <- trainSparse[-(nrow(trainSparse)+1):-150,]#configure the dinmensions
set.seed(100)
trainSparse$recommended_id = as.factor(trainSparse$recommended_id)
testSparse$recommended_id = as.factor(testSparse$recommended_id )
trainSparse$recommended_id
testSparse$recommended_id

##################     random Forest  ##################################

library(randomForest)

RF_model = randomForest(recommended_id ~ ., data=trainSparse)

predictRF = predict(RF_model, newdata=testSparse)

confucion_Mtr<-table(testSparse$recommended_id, predictRF)

confucion_Mtr

Accurancy_RF<-(diag(confucion_Mtr))/sum(confucion_Mtr)

Accurancy_RF

Accurancy<-(confucion_Mtr[1]+confucion_Mtr[4])/sum(confucion_Mtr)

Accurancy


################### Suport Vector Machine ##################
library(kernlab)
library(e1071)
SVM_Model<-svm(trainSparse$recommended_id ~ ., data=trainSparse, type="C",kernel="linear")
summary(SVM_Model)
#plot(myModel,data=testSparse,actor~actual)
predict_svm<-predict(SVM_Model,newdata=testSparse)
predict_svm
confucion_Mtr_SVM<-table(testSparse$recommended_id,predict_svm)
confucion_Mtr_SVM
ac_svm=(diag(confucion_Mtr_SVM))/sum(confucion_Mtr_SVM)
ac_svm
Accurancy_svm<- (confucion_Mtr_SVM[1]+confucion_Mtr_SVM[4])/sum(confucion_Mtr_SVM)
Accurancy_svm
######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################

#**********test**************
t2 <-read_excel("/Users/user/Desktop/RssNewData.xlsx")
glimpse(t2)# print the dataSet


corpus2 = Corpus(VectorSource(t2$Text))# A corpus is a collection of documents(texts)
corpus2[[1]][1]#show the 1st document(text)
t2$Sentiment[1]



corpus2 = tm_map(corpus2, PlainTextDocument)
corpus2= tm_map(corpus2, tolower)
corpus2[[1]][1] 
corpus2 = tm_map(corpus2, removePunctuation)
corpus2[[1]][1]
corpus2 = tm_map(corpus2, removeWords, c("cloth", stopwords("english")))
corpus2[[1]][1] 
corpus2 = tm_map(corpus2, stemDocument)
corpus2[[1]][1] 



###################### Create Document Term Matrix #######################
frequencies2 = DocumentTermMatrix(corpus2)# colect the frequences
sparse2 = removeSparseTerms(frequencies2, 0.995)#remove the words with many 0 frequency
tSparse2 = as.data.frame(as.matrix(sparse2))#convert the matrix into a data frame


colnames(tSparse2) = make.names(colnames(tSparse2))#makes all the variable names R-friendly,
tSparse2$recommended_id = t2$Sentiment#adds the dependent variable to the data set
prop.table(table(tSparse2$recommended_id)) #prints the proportion(ποσοστο) of the labels in the target variable, 'recommended_id'.
#prepare DataSet
tSparse3=tSparse #New DataFrame
tSparse3 <- tSparse3[-(nrow(tSparse2)+1):-200,]#configure the dinmensions
#tSparse3[1:10,1:4983]=0# set zero values
tSparse3[1:nrow(tSparse2),1:4983]=0# set zero values
tSparse3[1:nrow(tSparse2),4984]=''
x=intersect(names(tSparse3), names(tSparse2))#keep the valus of same attributes
x
length(x)  
tSparse3[1:(length(x)-1)]=tSparse2[1:(length(x)-1)]# set the values in the new test set
print(tSparse3[,1:length(x)])
tSparse3['recommended_id']  

#**************************************************************
#Lines 5 to 7
predictRF3= predict(SVM_Model, newdata=tSparse3)
predictRF3[1:50]#here we cal see the predictions
reality=tSparse3[1:50,4984]
reality


