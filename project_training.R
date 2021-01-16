##################################################################################
#Processing the raw data and build the model
##################################################################################

#Checking and installing the required packages
wants <- c("caret", "quanteda","stringr","nnet","syuzhet")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])


library(caret)
library(stringr)
library(quanteda)
library(nnet)
library(syuzhet)
library(e1071)
library(randomForest)

#####Pre-processing function to convert raw Review into dfm data frame format
preprocessing_train <-function(x,y){
  
  
  x = str_replace_all(x," ok","average")
  x = str_replace_all(x,"okay","average")
  x = str_replace_all(x,"ordinary","average")
  x = str_replace_all(x,"nothing special","average")
  x = str_replace_all(x,"satisfactory","average")
  x = str_replace_all(x,"bland","average")
  x = str_replace_all(x," fine","average")
  x = str_replace_all(x,"mixed","average")
  x = str_replace_all(x," bit","average")
  x = str_replace_all(x,"decent","average")
  x = str_replace_all(x,"so-so","average")
  x = str_replace_all(x,"quite","average")
  x = str_replace_all(x,"adequate","average")
  x = str_replace_all(x,"basic","average")
  x = str_replace_all(x,"mediocre","average")
  x = str_replace_all(x,"n't","not")
  
  ################ Adding the Text Length as one of the feature #######
  TextLength <- nchar(x)
  
  ################# Calculating Sentiment score #######################
  x <- sapply(x,
              function(review) 
              { gsub("[\r\n]", "", review) 
              }
  )
  
  score<-get_sentiment(x, method="bing")
  score.df<- data.frame(score)
  colnames(score.df)<- 'score'
  
  ###### Data frame with review,rating,Length, sentiment score   #################
  
  review.df<- data.frame(Review=x,Rating=y,Length=TextLength,Score=score.df$score)
  review.df$Review<-as.character(review.df$Review)
  
  ######## Up-sampling as the dataset is imbalanced ###########################
  
  review.up.df<- upSample(review.df[,-2],review.df$Rating)
  names(review.up.df)[names(review.up.df)=="Class"] <- "Rating"
  
  
  ############### Cleaning the review ##############################
  
  token<- tokens(review.up.df$Review,what="word",
                 remove_numbers=TRUE,remove_punct=TRUE,
                 remove_symbols=TRUE,remove_hyphens=TRUE)
  tokens.clean<- tokens_tolower(token)
  tokens.clean<- tokens_select(tokens.clean,stopwords(),
                               selection='remove')
  tokens.clean<- tokens_wordstem(tokens.clean,language="english")
  tokens.clean<-tokens_select(tokens.clean, pattern = c("hote*", "room*","resort","just","place"), selection='remove')
  
  ##################### Creating train document frequency matrix #######
  tokens.clean<-tokens_ngrams(tokens.clean,1:2)
  dfm.data<- dfm(tokens.clean,tolower=FALSE)
  
  ##################### Extracting top 100 features from DFM ###########
  dfm.top<-dfm_select(dfm.data,names(topfeatures(dfm.data,n=100)))
  saveRDS(dfm.top,file ="dfm.rds")
  
  ##################### Converting DFM to Dataframe ###############
  dfm.df.data <- convert(dfm.top, to = "data.frame")
  dfm.df.data<- data.frame(Rating=review.up.df$Rating,Score=review.up.df$Score,Length=review.up.df$Length,dfm.df.data[,-1])
  
}
#########################End of function preprocessing###################

######################## Read dataset####################################

data_review <- read.csv("C:/Lekshmi/Excelr/Project/Hotel Review/Dataset/train (2).csv/train (2).csv",strip.white = T)
data_review$Rating = str_replace_all(data_review$Rating,"2","1")
data_review$Rating = str_replace_all(data_review$Rating,"4","5")
data_review$Rating[data_review$Rating == "1"] <- "Bad"
data_review$Rating[data_review$Rating == "3"] <- "Average"
data_review$Rating[data_review$Rating == "5"] <- "Good"
data_review$Rating<- as.factor(data_review$Rating)
data_review$Review<- as.character(data_review$Review)


################ Passing entire dataset for preprocessing #######

review.pp <- preprocessing_train(data_review$Review,data_review$Rating)

################# Model Building #################################

modelfun<-function(x) {
  
  randomForest(Rating~., data=x,mtry=20,maxnodes=250)

}

model.rf<-modelfun(review.pp) 

##################### Saving model RDS #########################
saveRDS(model.rf,file ="fitmodel.rds")



