#Diedre Brown; dbrow207@pratt.edu
#INFO 640 - Fall 2019 - Pratt Institute
#Lab 8 - Text Analysis

#####install packages and call libraries####
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("gmodels")
install.packages("broom")
install.packages("GGally")

install.packages("tidytext")
install.packages("gutenbergr")#project gutenberg
install.packages("tm")#to create and work with corpora
install.packages("topicmodels")#for LDA topic models
install.packages("Rpoppler") 
install.packages("data.table")
install.packages("stringr")
#install.packages("qdqp") NOT AVAILABLE FOR R V. 3.6.1

library(ggthemes)
library(tidyverse)
library(data.table)
library(dplyr)
library(broom)
library(GGally)

library(tidytext)
library(tm)
library(stringr)
library(topicmodels)
library(gutenbergr)

####DIRECT TEXT####
#Unstructured Emily Dickenson poem from txt file
EDtext <- "Because I could not stop for         Death -
          He kindly stopped for me -
          The Carriage held but just Ourselves -
          and Immortality"
EDtext

#text cleaning
##make everything lowercase, remove excess punctuation, remove extraneous whitespace
text1 <- tolower(EDtext) #lowercase
removePunctuation(text1) #remove punctuation
stripWhitespace(text1) #remove extra whitespace
#none of the text cleaning was saved but will be done in next section

#remove stopwords
stopwords("en")
removeWords(text1, stopwords("en"))
my_stops<- c(stopwords("en"),"death")
removeWords(text1, my_stops)

####EXERCISE 1 - text cleaning via saving to new variables####
eDTlow <- tolower(EDtext)
eDTpunc <-removePunctuation(eDTlow)
eDTwhite <-stripWhitespace(eDTpunc)
text_new <-eDTwhite
#remove stopwords
text_newnodeath <-removeWords(text_new, my_stops)
text_newnodeath

####STEMMING - When we are interested in meaning (rather than tense, genre, or inflection), we want to treat these as the same word make a vector to split text_newnodeath on the spaces, then turn it from a list to a vector####
nvec <-unlist(strsplit(text_newnodeath, split = " "))
stem_text <-stemDocument(nvec)
print(stem_text)
#pass the vector of the stemmed words and the "dictionary" for look up (nvec) to completed_text
completed_text <-stemCompletion(stem_text, nvec)
completed_text

####STRUCTURED TEXT i.e.) a collection of tweets, social media posts, user comments, and other short formats with metadata####
peace_res <- read.csv("Desktop/Pratt/Fall2019/DataAnalysis_Info640_McSweeney/INFO640-WDB/Fa19-Info640-WDB/Lab8-TextAnalysis/pax_20_02_2018_1_CSV.csv", encoding = "utf-8", header = TRUE, stringsAsFactors = FALSE)
str(peace_res)
glimpse(peace_res)

#data cleaning
#retain metadata identifier by changing names, then specify Dataframe Source and transforming the corpus
#transforming text column into a corpus so that it can be analyzed with tm
names(peace_res)[names(peace_res)=="AgtId"] <- "doc_id"
peace_res$doc_id <-as.character(peace_res$doc_id)
names(peace_res)[names(peace_res)=="OthAgr"] <- "text"
colnames(peace_res)
peace_source <-DataframeSource(peace_res)
peace_corpus <-VCorpus(peace_source)
print(peace_corpus)

peace_corpus[[10]]
peace_corpus[[10]] [1]
peace_corpus[[10]] [2]
print(peace_corpus[[10]] [10])
print(peace_corpus[[12]] [1])

#make a new corpus for cleaning
#tm_map() function maps functions. takes corpus and a function
#map function onto the corpus and remove numbers
peace_cleaned <-tm_map(peace_corpus, removeNumbers)
peace_cleaned [[10]] [1]
peace_cleaned <-tm_map(peace_cleaned, content_transformer(tolower))
stpwrds = c(stopwords("en"), "peace", "agreement", "shall", "will","government", "page", "parties")
peace_cleaned = tm_map(peace_cleaned, removeWords, stpwrds)
peace_cleaned <- tm_map(peace_cleaned, removePunctuation)
peace_cleaned <- tm_map(peace_cleaned, stripWhitespace)
peace_cleaned [[10]] [1]

####TOPIC MODELING####
peace_dtm<- DocumentTermMatrix(peace_cleaned) #document-term matrix
peace_dtm
#find and select unique indexes
unique_indexes <- unique(peace_dtm$i) #find empty rows
peace_dtm<-peace_dtm[unique_indexes,] #select unique indexes
peace_dtm
peace_dtm_tidy<-tidy(peace_dtm) #tidy the result
peace_dtm_tidy


#cleaned_peace_res <- peace_dtm_tidy%>%
#  group_by(document)%>%
#  mutate(terms = toString(rep(term, count))) %>%
#  select(document, terms) %>%
#  unique()
#head(cleaned_peace_res)

###LDA Analysis
k <- 6 #number of topics we want
peace_lda <-LDA(peace_dtm, k=k, control = list(seed=1234))
peace_lda 
peace_lda_words <-terms(peace_lda,5)
peace_lda_words

peace_lda_topics <-as.matrix(peace_lda_words)
head(peace_lda_topics)
write.csv(peace_lda_topics, file = paste("Desktop/Pratt/Fall2019/DataAnalysis_Info640_McSweeney/INFO640-WDB/Fa19-Info640-WDB/Lab8-TextAnalysis/peace_LDA_", k, ".csv"))
head(peace_lda_topics)

#tidy group
peace_lda_tidy <- tidy(peace_lda)
head(peace_lda_tidy)

#order words from most prominent to least for each topic
###NOTE: THIS ONLY SEEMS TO WORK WITH THE '' AROUND TOPIC OR WHEN TOPIC IS CALLED AS top_terms$topic###
top_terms <- peace_lda_tidy %>%
  group_by(top_terms$topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(top_terms$topic, -beta)
head(top_terms)

#reorder terms based on the term and its prevalance and use ggplot to display it as a bar chart
#where the topic is the color and the size of the bar is the beta (or prevalance of the term)
#facet it by topic so each topic is its own subplot
###NOTES: mutate  ISN'T WORKING AT ALL and TERM AND TOPIC ONLY WORK IF CALLED WITH $###
top_terms%>%
  #mutate(term = reorder(top_terms$term, beta)) %<%
  ggplot(aes(top_terms$term, beta, fill=factor(top_terms$topic))) +
  geom_col(show.legend = FALSE)+
  facet_wrap(-top_terms$topic, scales = "free") +
  coord_flip()


#LDA FUNCTION ###NOT WORKING###
get_LDA_topics_terms_by_topic <- function(input_corpus, plot=TRUE, number_of_topics = 6, number_of_words=5,
                                          path="peace_LDA_document_topics_")
{
  my_dtm <- DocumentTermMatrix(input_corpus)
  
  unique_indexes <- unique(my_dtm$i)
  my_dtm <- my_dtm[unique_indexes,]
  
  my_lda<-LDA(my_dtm, k=number_of_topics, control = list(seed=1234))
  my_topics <- tidy(my_lda, matrix="beta")
  
  my_lda_words <- terms(my_lda, number_of_words)
  my_lda_topics<- as.matrix(my_lda_words)
  write.csv(my_lda_topics, file = paste(path,number_of_topics, ".csv"))
  
  my_top_terms <- my_topics %>%
    group_by(my_topics$term)%>%
    top_n(number_of_words, beta)%>%
    ungroup()%>%
    arrange(my_topics$topic, -beta)
  
  if(plot==TRUE){ ###NOTE: NOT RUNNING B/C CANNOT READ TERM OR TOPIC AS VARIABLES?
    my_top_terms%>%
      mutate(term=reorder(my_top_terms$term,beta))%>%
      ggplot(aes(my_top_terms$term, beta, fill=factor(my_top_terms$topic))) +
      geom_col(show.legend=FALSE) + 
      facet_wrap(-my_top_terms$topic, scales = "free") +
      coord_flip()
  }else{
    return(my_top_terms)
  }
}

get_LDA_topics_terms_by_topic(peace_cleaned)

get_LDA_topics_terms_by_topic(peace_cleaned,number_of_topics = 4, number_of_words=4)

#see what topics are associated with which documents - LDA Vector "gamma"
peace_lda_document_topics<-tidy(peace_lda, matrix="gamma")
peace_lda_document_topics

write.csv(peace_lda_document_topics, file = paste())

head(peace_lda_document_topics)
dim(peace_lda_document_topics)

#join to perform other analyses, use spread() to turn dataframe into a regular, more usable dataframe
peace_lda_document <-spread(peace_lda_document_topics, peace_lda_document_topics$topic, gamma)
dim(peace_lda_document)
head(peace_lda_document)

#id the max topic for each document; assign a new column name; select 2nd through 7th columns only, and apply a 
#function that takes the dataframe and reads it by rows (axis=1), and asks "which one is the max"
peace_lda_document$max_topic <- colnames(peace_lda_document[2:7]) [apply(peace_lda_document,)]
head(peace_lda_document)

#visualize the relationship between topics and other variables
#merge original peace_res dataframe to the topic weights using a specified key for each dataframe. 
#specify key; merge data frames
dt1 <-data.table(peace_lda_document, key = "document")
dt2 <-data.class(peace_res, key="doc_id")

peace_merged <- dt1[dt2]
dim(peace_merged)
colnames(peace_merged)

#select just the first variables 
peace_analyze<-select(peace_merged, c(Con, Contp, Reg, Dat, Status, Lgt, Agtp))
head(peace_analyze)


####UNSTRUCTURED TEXT####
#Don Quixote by Miguel de Cervantes [Saavedra] #996 from Project Gutenberg - LESS STRUCTURE THAN A CSV, BUT MORE COMPLEX TEXT
dq <- gutenberg_download(996)
dq

#create a corpus
dq_source <- VectorSource(dq)
dq_corpus <- VCorpus(dq_source)


####SENTIMENT ANALYSIS####
sentiments

get_sentiments("afinn")
get_sentiments("nrc")
get_sentiments("bing")


lyrics_raw <- read.csv("Desktop/Pratt/Fall2019/DataAnalysis_Info640_McSweeney/INFO640-WDB/Fa19-Info640-WDB/Lab8-TextAnalysis/songdata.csv", encoding="utf-8", header = TRUE, stringsAsFactors = FALSE)
summary(lyrics_raw)

#tidy the dataframe and treat it as a corpus
tidy_lyrics <- lyrics_raw %>%
  ungroup() %>%
  unnest_tokens(word, text)
summary(tidy_lyrics)
head(tidy_lyrics)

#find "joy" in david bowie lyrics; use and inner join on the "word" column for both the sentiment and the tidy lyrics
#count the number of times each word appears & sort the list. use dplyr pkg
nrc_sent <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")

tidy_lyrics_bowie <- tidy_lyrics %>%
  filter(artist == "David Bowie")

tidy_lyrics_bowie %>%
  inner_join(nrc_sent) %>%
  dplyr::count(word, sort = TRUE)

bowie_sentiment <- tidy_lyrics_bowie %>%
  inner_join(get_sentiments("bing")) %>%
  dplyr::count(song, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

head(bowie_sentiment)

#plot songs and a function of their positive and negative sentiment to see if there are any clusters or interesting patterns
ggplot(bowie_sentiment, aes(negative, positive, color = song)) +
  geom_jitter(show.legend = FALSE)

#positive-negative sentiment score of david bowie over his career
bowie_career_sentiment <- mean(bowie_sentiment$sentiment)
bowie_career_sentiment

#sentiment analysis with other artists
unique(lyrics_raw$artist)
#sentiment of billie holiday vs. ella fitzgerald

##BILLIE HOLIDAY##
tidy_lyrics_bh <- tidy_lyrics %>%
  filter(artist == "Billie Holiday")

tidy_lyrics_bh%>%
  inner_join(nrc_sent) %>%
  dplyr::count(word, sort = TRUE)

bh_sentiment <- tidy_lyrics_bh %>%
  inner_join(get_sentiments("bing")) %>%
  dplyr::count(song, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

head(bh_sentiment)

ggplot(bh_sentiment, aes(negative, positive, color = song)) +
  geom_jitter(show.legend = FALSE)  

bh_career_sentiment <- mean(bh_sentiment$sentiment)
bh_career_sentiment #3.09
  
##ELLA FITZGERALD##  
tidy_lyrics_ef <- tidy_lyrics %>%
  filter(artist == "Ella Fitzgerald")

tidy_lyrics_ef%>%
  inner_join(nrc_sent) %>%
  dplyr::count(word, sort = TRUE)

ef_sentiment <- tidy_lyrics_ef %>%
  inner_join(get_sentiments("bing")) %>%
  dplyr::count(song, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

head(ef_sentiment)

ggplot(ef_sentiment, aes(negative, positive, color = song)) +
  geom_jitter(show.legend = FALSE)    

ef_career_sentiment <- mean(ef_sentiment$sentiment)
ef_career_sentiment  #2.82

#surprise! on average Billie Holiday's lyrics are more positive than Ella Fitzgerald
  


