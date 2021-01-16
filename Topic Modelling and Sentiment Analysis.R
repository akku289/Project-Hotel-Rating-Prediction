library(tidytext)
library(tm)
library(textclean)
library(stringr)
library(topicmodels)
library(ggplot2)
library(dplyr)

data<- read.csv(file.choose())
data$Rating = str_replace_all(data$Rating,"2","1")
data$Rating = str_replace_all(data$Rating,"4","5")
data$Rating[data$Rating == "1"] <- "Bad"
data$Rating[data$Rating == "3"] <- "Average"
data$Rating[data$Rating == "5"] <- "Good"
head(data)

#Text Cleaning
textcleaner <- function(x){
  x <- as.character(x)
  
  x <- x %>%
    str_to_lower() %>%
    replace_contraction() %>%
    replace_internet_slang() %>%
    replace_emoji() %>%
    replace_emoticon() %>%
    replace_hash(replacement = "") %>%
    replace_word_elongation() %>%
    replace_number(remove = T) %>%
    replace_date(replacement = "") %>%
    replace_time(replacement = "") %>%
    str_remove_all(pattern = "[[:punct:]]") %>%
    str_remove_all(pattern = "[^\\s]*[0-9][^\\s]*") %>%
    str_squish() %>%
    str_trim()
  
  xdtm <- VCorpus(VectorSource(x)) %>%
    tm_map(removeWords,stopwords("en")) %>%
    tm_map(removeWords,c("hotel","room"))
  
  #convert corpus to document term matrix
  return(DocumentTermMatrix(xdtm))
}

data_1 <- data %>% filter(Rating == "Bad")
data_3 <- data %>% filter(Rating == "Average")
data_5 <- data %>% filter(Rating == "Good")
table(data$Rating)

##Topic Modelling Rating = 5
#apply text cleaner function for Review
dtm_5 <- textcleaner(data_5$Review)
#find most frequent word with frequency 100
freqterm_5 <- findFreqTerms(dtm_5,100) #950 words
#subset dtm to choose only those selected words out of 950 words
dtm_5 <- dtm_5[,freqterm_5]
#choose only those words that appear once in each rows
rownum_5 <- apply(dtm_5, 1, sum)
dtm_5 <- dtm_5[rownum_5>0,]
#apply to LDA function, set k=6 means we want to build 6 topics
#install.packages("topicmodels")

lda_5 <- LDA(dtm_5, k=6, control = list(seed = 1602))
#apply auto tidy using tidy and use beta as per-topic-per-word probablities
topic_5 <- tidy(lda_5,matrix="beta")
#choose 15 words with highest beta value from each topic
top_terms_5 <- topic_5 %>%
  group_by(topic) %>%
  top_n(15,beta) %>%
  ungroup() %>%
  arrange(topic,-beta)
#plot the topic and words 
plot_topic_5 <- top_terms_5 %>%
  mutate(term=reorder_within(term,beta,topic)) %>%
  ggplot(aes(term,beta,fill = factor(topic))) + 
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  labs(x="Word \n", y= "\n Beta", title = "Topic Modelling for Rating:5 \n")+
  coord_flip() +
  scale_x_reordered() 

plot_topic_5

#Topic Modelling Rating = 3
dtm_3 <- textcleaner(data_3$Review)
freqterm_3 <- findFreqTerms(dtm_3, 100) #282 words
dtm_3 <- dtm_3[,freqterm_3]
rownum_3 <- apply(dtm_3,1, sum)
dtm_3 <- dtm_3[rownum_3>0,]

lda_3 <- LDA(dtm_3, k=6, control = list(seed = 1602))
topic_3 <- tidy(lda_3,matrix="beta")
top_terms_3 <- topic_3 %>%
  group_by(topic) %>%
  top_n(15,beta) %>%
  ungroup() %>%
  arrange(topic,-beta)
plot_topic_3 <- top_terms_3 %>%
  mutate(term = reorder_within(term,beta, topic)) %>%
  ggplot(aes(term,beta,fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic,scales = "free") +
  labs(x="Word \n", y= "\n Beta", title = "Topic Modelling for Rating:3 \n")+
  coord_flip() +
  scale_x_reordered()
plot_topic_3

#Topic Modelling Rating = 1
dtm_1 <- textcleaner(data_1$Review)
freqterm_1 <- findFreqTerms(dtm_1, 100) #157 words
dtm_1 <- dtm_1[,freqterm_1]
rownum_1 <- apply(dtm_1,1, sum)
dtm_1 <- dtm_1[rownum_1>0,]

lda_1 <- LDA(dtm_1, k=6, control = list(seed = 1602))
topic_1 <- tidy(lda_1,matrix="beta")
top_terms_1 <- topic_1 %>%
  group_by(topic) %>%
  top_n(15,beta) %>%
  ungroup() %>%
  arrange(topic,-beta)
plot_topic_1 <- top_terms_1 %>%
  mutate(term = reorder_within(term,beta, topic)) %>%
  ggplot(aes(term,beta,fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic,scales = "free") +
  labs(x="Word \n", y= "\n Beta", title = "Topic Modelling for Rating:1 \n")+
  coord_flip() +
  scale_x_reordered()
plot_topic_1

#Topic Modelling over entire data
dtm_entire <- textcleaner(data$Review)
freqterm_entire <- findFreqTerms(dtm_entire, 500) #480 words
dtm_entire <- dtm_entire[,freqterm_entire]
rownum_entire <- apply(dtm_entire,1, sum)
dtm_entire <- dtm_entire[rownum_entire>0,]

lda_entire <- LDA(dtm_entire, k=6, control = list(seed = 1602))
topic_entire <- tidy(lda_entire,matrix="beta")
top_terms_entire <- topic_entire %>%
  group_by(topic) %>%
  top_n(15,beta) %>%
  ungroup() %>%
  arrange(topic,-beta)
plot_topic_entire <- top_terms_entire %>%
  mutate(term = reorder_within(term,beta, topic)) %>%
  ggplot(aes(term,beta,fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic,scales = "free") +
  labs(x="Word \n", y= "\n Beta", title = "Topic Modelling for entire data \n")+
  coord_flip() +
  scale_x_reordered()
plot_topic_entire

##Working with MULTIGRAMS
library(tokenizers)

multigram_textcleaner <- function(x){
  x <- as.character(x)
  
  x <- x %>%
    str_to_lower() %>%
    replace_contraction() %>%
    replace_internet_slang() %>%
    replace_emoji() %>%
    replace_emoticon() %>%
    replace_hash(replacement = "") %>%
    replace_word_elongation() %>%
    replace_number(remove = T) %>%
    replace_date(replacement = "") %>%
    replace_time(replacement = "") %>%
    str_remove_all(pattern = "[[:punct:]]") %>%
    str_remove_all(pattern = "[^\\s]*[0-9][^\\s]*") %>%
    str_squish() %>%
    str_trim()
  
  xdtm <- VCorpus(VectorSource(x)) %>%
    tm_map(removeWords,stopwords("en")) 
}

#Rating=5 bigram
bigram_clean <- multigram_textcleaner(data_5$Review)

library(RWeka)
BigramTokeziner <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
bigram_dtm <- DocumentTermMatrix(bigram_clean, control = list(tokenize = BigramTokeziner))
library(udpipe)
bigram_dtm<- dtm_remove_terms(bigram_dtm,terms=c("tongue sticking","ca nt","e tongue","e tongue","hotel e","hotel nt","nt know","nt wait","wo nt","nt want","great e"),remove_emptydocs = FALSE)

freqterm_bigram <- findFreqTerms(bigram_dtm, 100) #148 words
bigram_dtm <- bigram_dtm[,freqterm_bigram]
rownum_bigram <- apply(bigram_dtm,1, sum)
bigram_dtm <- bigram_dtm[rownum_bigram>0,]

lda_bigram <- LDA(bigram_dtm, k=6, control = list(seed = 1602))

topic_bigram <- tidy(lda_bigram,matrix="beta")
top_terms_bigram <- topic_bigram %>%
  group_by(topic) %>%
  top_n(15,beta) %>%
  ungroup() %>%
  arrange(topic,-beta)
plot_topic_bigram <- top_terms_bigram %>%
  mutate(term = reorder_within(term,beta, topic)) %>%
  ggplot(aes(term,beta,fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic,scales = "free") +
  labs(x="Word \n", y= "\n Beta", title = "Topic Modelling for Bigrams, Rating=5 \n")+
  coord_flip() +
  scale_x_reordered()
plot_topic_bigram

#Rating=5 trigram
trigram_clean <- multigram_textcleaner(data_5$Review)

library(RWeka)
TrigramTokeziner <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
trigram_dtm <- DocumentTermMatrix(trigram_clean, control = list(tokenize = TrigramTokeziner))
library(udpipe)

freqterm_trigram <- findFreqTerms(trigram_dtm, 30) #82 words
trigram_dtm <- trigram_dtm[,freqterm_trigram]
rownum_trigram <- apply(trigram_dtm,1, sum)
trigram_dtm<- dtm_remove_terms(trigram_dtm,terms=c("best e tongue","better e tongue","ca nt beat",
                                                   "ca nt imagine","ca nt say","ca nt wait","e tongue sticking",
                                                   "empire state building","exceeded e tongue",
                                                   "excellence punta cana","flat screen tv","good e tongue",
                                                   "great e tongue","great hotel just","hotel ca nt",
                                                   "great hotel great","great location great",
                                                   "hotel e tongue","husband just returned","ine tongue sticking",
                                                   "just returned night","just returned week","just short walk",
                                                   "know e tongue","new york city","nt e tongue","nt wait return",
                                                   "old san juan","quite e tongue","sticking erience hotel",
                                                   "tongue sticking ect","tongue sticking ectations",
                                                   "tongue sticking ected","tongue sticking ecting",
                                                   "tongue sticking edia","tongue sticking ensive",
                                                   "tongue sticking erience","tongue sticking erienced",
                                                   "tongue sticking eriences","tongue sticking lore",
                                                   "tongue sticking loring","tongue sticking ress",
                                                   "wonderful e tongue"),remove_emptydocs = FALSE)
trigram_dtm <- trigram_dtm[rownum_trigram>0,]

lda_trigram <- LDA(trigram_dtm, k=3, control = list(seed = 1602))

topic_trigram <- tidy(lda_trigram,matrix="beta")
top_terms_trigram <- topic_trigram %>%
  group_by(topic) %>%
  top_n(15,beta) %>%
  ungroup() %>%
  arrange(topic,-beta)
plot_topic_trigram <- top_terms_trigram %>%
  mutate(term = reorder_within(term,beta, topic)) %>%
  ggplot(aes(term,beta,fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic,scales = "free") +
  labs(x="Word \n", y= "\n Beta", title = "Topic Modelling for Trigrams, Rating=5 \n")+
  coord_flip() +
  scale_x_reordered()
plot_topic_trigram

#Rating=1
bigram_clean2 <- bigram_textcleaner(data_1$Review)

library(RWeka)
BigramTokeziner <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
bigram_dtm2 <- DocumentTermMatrix(bigram_clean2, control = list(tokenize = BigramTokeziner))
library(udpipe)


freqterm_bigram2 <- findFreqTerms(bigram_dtm2, 15) #157 words
bigram_dtm2 <- bigram_dtm2[,freqterm_bigram2]
rownum_bigram2 <- apply(bigram_dtm2,1, sum)
bigram_dtm2 <- bigram_dtm2[rownum_bigram2 > 0,]

bigram_dtm2<- dtm_remove_terms(bigram_dtm2,terms=c("air conditioner","air conditioning","arrived hotel","bad e","booked hotel","booked room","breakfast buffet","ca nt","called desk","called hotel","change rooms","check time","clean room","credit card","customer service","day stay","desk clerk","desk staff","desk told","dominican republic","door room","e tongue","early morning","entered room","felt like","finally got","food good","given room","good thing","got hotel","got room","hotel e","hotel hotel","hotel just","hotel manager","hotel night","hotel nt","hotel room","hotel rooms","hotel staff","hotel stay","hotel stayed","just got","just nt","just returned","late night","left room","let know","long time","looked like","make sure","minutes later","needless say","new orleans","new room","new york","night room","night stay","nt believe","nt care","nt e","nt know","nt make","nt speak","nt stay","nt think","nt want","nt work","parking lot","place stay","pool area","puerto rico","punta cana","read reviews","reception desk","recommend hotel","room booked","room clean","room cleaned","room door","room hotel","room nt","room rate","room ready" ,"room room","room service","room small","san francisco","san juan","save money","second day","second night","service hotel","single beds","smelled like","smoking room","speak english","speak spanish","staff friendly","staff member","star hotel","star resort","stay hotel","stayed hotel","stayed night","stayed nights","staying hotel","terrible e","told hotel","told room","travel agent","valet parking","view room","web site","went desk","wo nt","worst e","year old","years ago"),remove_emptydocs = FALSE)

lda_bigram2 <- LDA(bigram_dtm2, k=6, control = list(seed = 1602))

topic_bigram2 <- tidy(lda_bigram2,matrix="beta")
top_terms_bigram2 <- topic_bigram2 %>%
  group_by(topic) %>%
  top_n(15,beta) %>%
  ungroup() %>%
  arrange(topic,-beta)
plot_topic_bigram2 <- top_terms_bigram2 %>%
  mutate(term = reorder_within(term,beta, topic)) %>%
  ggplot(aes(term,beta,fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic,scales = "free") +
  labs(x="Word \n", y= "\n Beta", title = "Topic Modelling for Bigrams, Rating=1 \n")+
  coord_flip() +
  scale_x_reordered()
plot_topic_bigram2

#---------------Sentiment Analysis---------------------------------------------
# Calculating the count and percentage of total positive and negative words in each review and
# Labeling each review as either negative or positive
total_pos_count <- 0
total_neg_count <- 0
pos_count_vector <- c()
neg_count_vector <- c()

## lOADING +VE AND -VE dictionaries
pos_words = scan(file.choose(), what="character", comment.char=";")	# read-in positive-words.txt
neg_words = scan(file.choose(), what="character", comment.char=";") # read-in negative-words.txt

size <- length(review_clean_final) #14343

for(i in 1:size){
  corpus_words<- list(strsplit(review_clean_final[[i]]$content, split = " "))
  pos_count <- length(intersect(unlist(corpus_words), unlist(pos_words)))
  neg_count <- length(intersect(unlist(corpus_words), unlist(neg_words)))
  if(pos_count>neg_count){
    #print("It's a positive review")
  } else{
    #print("It's a negative review")
  }
  total_count_for_current_review <- pos_count + neg_count ## current positive and negative count
  pos_percentage <- (pos_count*100)/total_count_for_current_review 
  neg_percentage <- (neg_count*100)/total_count_for_current_review
  total_pos_count <- total_pos_count + pos_count ## overall positive count
  total_neg_count <- total_neg_count + neg_count ## overall negative count
  pos_count_vector <- append(pos_count_vector, pos_count)
  neg_count_vector <- append(neg_count_vector, neg_count)
}


# Sentiment score of each review and visualizing using boxplot
counts <- data.frame(pos_count_vector, neg_count_vector) #13843
# sentiment <- data.frame(c(1:size),(pos_count_vector - neg_count_vector) / (pos_count_vector + neg_count_vector))
# boxplot(sentiment$X.pos_count_vector...neg_count_vector...pos_count_vector...neg_count_vector.[0:100]~sentiment$c.1.size.[0:100])

# Visualiztion of positive and negative count of single review
singe_review <- c(counts$pos_count_vector[3], counts$neg_count_vector[3])
barplot(t(as.data.frame(singe_review)), ylab = "Count", xlab = "Positve v/s Negative",  
        main = "Positive and Negative words in Review")



# Calculating overall percentage of positive and negative words of all the reviews
total_pos_count                                  ## overall positive count
total_neg_count                                  ## overall negative count
total_count <- total_pos_count + total_neg_count #177664
overall_positive_percentage <- (total_pos_count*100)/total_count
overall_negative_percentage <- (total_neg_count*100)/total_count
overall_positive_percentage                      ## overall positive percentage
overall_negative_percentage                      ## overall negative percentage

# Visualization of positive and negative word count for all the reviews
review_count_frame <- data.frame(matrix(c(pos_count_vector, neg_count_vector), nrow = 100, ncol = 2))
colnames(review_count_frame) <- c("Positive Word Count", "Negative Word Count")
barplot(review_count_frame$`Positive Word Count`, ylab = "Positive Word Count", xlab = "Reviews from 1 to 100",  
        main = "Positive words in Reviews", col="lightblue")
barplot(review_count_frame$`Negative Word Count`, ylab = "Negative Word Count", xlab = "Reviews from 1 to 100",  
        main = "Negative words in Reviews", col="lightblue")

# Visualization of Overall positive and negative reviews
percent_vec <- c(overall_positive_percentage, overall_negative_percentage)
percent_frame <- as.data.frame(percent_vec)
rownames(percent_frame) <- c("Positive Reviews","Negative Reviews")
colnames(percent_frame) <- c("Percentage")
percentage <- t(percent_frame)
barplot(percentage, ylab = "Percentage", main = "Sentiment Analysis of Hotel Reviews", col="lightblue")



