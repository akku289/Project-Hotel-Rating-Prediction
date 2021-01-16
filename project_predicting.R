#Checking and installing the required packages
wants <- c("caret", "quanteda","stringr","randomForest","syuzhet")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])


library(caret)
library(stringr)
library(quanteda)
library(syuzhet)
library(randomForest)


preprocessing_test<<-function(x){
  
  
  x = str_replace_all(x,"ok","average")
  x = str_replace_all(x,"okay","average")
  x = str_replace_all(x,"ordinary","average")
  x = str_replace_all(x,"nothing special","average")
  x = str_replace_all(x,"satisfactory","average")
  x = str_replace_all(x,"bland","average")
  x = str_replace_all(x,"fine","average")
  x = str_replace_all(x,"mixed","average")
  x = str_replace_all(x,"bit","average")
  x = str_replace_all(x,"decent","average")
  x = str_replace_all(x,"so-so","average")
  x = str_replace_all(x,"quite","average")
  x = str_replace_all(x,"adequate","average")
  x = str_replace_all(x,"basic","average")
  x = str_replace_all(x,"mediocre","average")
  x = str_replace_all(x,"n't","not")
  
  ################ Adding the Text Length as one of the feature ##########
  TextLength <- nchar(x)
  
  ################# Calculating Sentiment score #####################
  x <- sapply(x,
              function(y) 
              { gsub("[\r\n]", "", y) 
              }
  )
  
  score<-get_sentiment(x, method="bing")
  score.df<- data.frame(score)
  colnames(score.df)<- 'score'
  
  token<- tokens(x,what="word",
                 remove_numbers=TRUE,remove_punct=TRUE,
                 remove_symbols=TRUE,remove_hyphens=TRUE)
  tokens.clean<- tokens_tolower(token)
  tokens.clean<- tokens_select(tokens.clean,stopwords(),
                               selection='remove')
  tokens.clean<- tokens_wordstem(tokens.clean,language="english")
  tokens.clean<-tokens_select(tokens.clean, pattern = c("hote*", "room*","resort","just","place"), selection='remove')
  
  ##################### Creating train document frequency matrix ###############
  tokens.clean<-tokens_ngrams(tokens.clean,1:2)
  dfm.data<- dfm(tokens.clean,tolower=FALSE)
  
  dfm.top<-readRDS("./dfm.rds")
  dfm.top <-dfm_match(dfm.data, features = featnames(dfm.top))
  
  ##################### Converting DFM to Dataframe ###############
  dfm.df.data <- convert(dfm.top, to = "data.frame")
  dfm.df.data<- data.frame(Score=score.df$score,Length=TextLength,dfm.df.data[,-1])
  
}
######################## End of preprocessing function ##################
