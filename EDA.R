# Install packages
install.packages("tm")  # for text mining
install.packages(c("SnowballC","textstem")) # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
install.packages("tokenizers")
install.packages("tidytext")

library('tm')
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library('textstem')
library('tokenizers')
library('tidytext')
library('dbplyr')
library('tidyr')
library('tidytext')
library('ggplot2')
library('tidyr')

#Load file 
review <- read.csv(file.choose())
review$Rating = str_replace_all(review$Rating,"2","1")
review$Rating = str_replace_all(review$Rating,"4","5")
review$Rating[review$Rating == "1"] <- "Bad"
review$Rating[review$Rating == "3"] <- "Average"
review$Rating[review$Rating == "5"] <- "Good"
head(review)

#Load data as a corpus
review_corp <- Corpus(VectorSource(review$Review))
inspect(review_corp[1])

#Removing punctuation, stopwords, numbers, white spaces and converting text to lower case
review_corp_clean <- tm_map(review_corp, tolower)
review_corp_clean <- tm_map(review_corp_clean, removePunctuation)
review_corp_clean <- tm_map(review_corp_clean, removeNumbers)
review_corp_clean <- tm_map(review_corp_clean, removeWords, stopwords('english'))

#Read stop-words.txt words
stop_words <- scan(file.choose(), what= "character")

#Remove stop words of the external file from the corpus and whitespaces again and inspect
stop_words_data <- as.data.frame(stop_words)
review_corp_clean <- tm_map(review_corp_clean, removeWords, stop_words_data$stop_words)
review_clean_final <- tm_map(review_corp_clean, stripWhitespace)
inspect(review_clean_final[1])

#Text Lemmatize
review_clean_final <- lemmatize_words(review_clean_final)

#converting unstructured data to structured format using TERM DOCUMENT MATRIX
review_tdm <- TermDocumentMatrix(review_clean_final)
review_tdm <- as.matrix(review_tdm)

#frequency
v <- sort(rowSums(review_tdm),decreasing = TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

#Plotting Bar Plot
b <- rowSums(review_tdm)
b_sub <- subset(b,b>=3000)
barplot(b_sub, las = 3, col = rainbow(20), sub = "Most occured words")

word_counts=d %>%
  filter(freq > 100) %>%
  mutate(word <- reorder(word, freq))
word_counts_df<-head(word_counts,25)
#Plotting most occurring words

ggplot(data=word_counts_df, aes(x = word, y= freq))+
  geom_col(fill="lightblue",colour="grey")+
  coord_flip()+
  labs(x="Word \n", y= "\n Frequency", title = "Most Occuring words \n")+
  geom_text(aes(label=freq),hjust=0.5, colour="black",fontface="italic",size=2.5)

#Creating Word Clouds
wordcloud(words = names(head(b,100)), freq = b, 
          random.order = F, colors = rainbow(20), 
          scale=c(2,.4), rot.per = 0.3)

pos.words = scan(file.choose(), what="character", comment.char=";")
neg.words = scan(file.choose(), what="character", comment.char=";")

#Positive Word Cloud
pos.matches = match(names(v), c(pos.words))
pos.matches = !is.na(pos.matches)
pwords.freq <- v[pos.matches]
pwords <- names(pwords.freq)

wordcloud(pwords,pwords.freq,scale=c(3.5,.5),colors = rainbow(20),
          random.order='F',min.freq=1000)

#Negative Word Cloud
neg.matches = match(names(v), c(neg.words))
neg.matches = !is.na(neg.matches)
nwords.freq <- v[neg.matches]
nwords <- names(nwords.freq)

wordcloud(nwords,nwords.freq,scale=c(3.5,.5),colors = rainbow(20),
          random.order='F',min.freq=200 )

#Identifying & Plotting Bigrams 
review_bigrams=review %>%
  unnest_tokens(bigram, Review, token = "ngrams", n = 2, collapse = FALSE) 

bigrams_separated <- review_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words_data) %>%
  filter(!word2 %in% stop_words_data) 

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

word_counts_bigram <- bigrams_united %>%
  count(bigram, sort = TRUE)

word_counts_bigram<-word_counts_bigram[!(word_counts_bigram$bigram=="did n't"),]
word_counts_bigram<-word_counts_bigram[!(word_counts_bigram$bigram=="did not"),]
word_counts_bigram<-word_counts_bigram[!(word_counts_bigram$bigram=="ca n't"),]

word_counts_bigram %>%
  head(25) %>%
  mutate(word = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col(fill = "darkorange",colour="lightblue") +
  scale_y_continuous(labels = comma_format()) +
  coord_flip() +
  labs(title = "Most commonly occuring descriptive bigrams in Reviews",
       y = "Frequency")+
  geom_text(aes(label=n),hjust=0.5, colour="black",fontface="italic",size=2.5)

wordcloud(word_counts_bigram$bigram,word_counts_bigram$n,max.words=100,random.order = F, colors = rainbow(20), 
          scale=c(2,.4), rot.per = 0.3)

#Identifying and Plotting Trigrams
review_trigrams <- review %>%
  unnest_tokens(trigram, Review, token = "ngrams", n = 3)

trigrams_separated <- review_trigrams %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")

trigrams_filtered <- trigrams_separated %>%
  filter(!word1 %in% stop_words_data) %>%
  filter(!word2 %in% stop_words_data) %>%
  filter(!word3 %in% stop_words_data)

trigram_counts <- trigrams_filtered %>% 
  count(word1, word2, word3, sort = TRUE)

trigrams_united <- trigrams_filtered %>%
  unite(trigram, word1, word2, word3, sep = " ")

word_counts_trigram <- trigrams_united %>%
  count(trigram, sort = TRUE)

word_counts_trigram %>%
  head(25) %>%
  mutate(word = reorder(trigram, n)) %>%
  ggplot(aes(trigram, n)) +
  geom_col(fill = "violet", colour="grey50") +
  scale_y_continuous(labels = comma_format()) +
  coord_flip() +
  labs(title = "Most commonly occuring descriptive trigram in Reviews",
       y = "Frequency")+
  geom_text(aes(label=n),hjust=0.5, colour="black",fontface="italic",size=2.5)
  
wordcloud(word_counts_trigram$trigram,word_counts_trigram$n,max.words=100,random.order = F, colors = rainbow(20), 
          scale=c(2,.4), rot.per = 0.3)

