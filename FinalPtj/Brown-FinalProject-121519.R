####PROJECT TITLE AND CONTACT####
#Diedre Brown | dbrow207@pratt.edu
#INFO 640 Data Analysis | Pratt Institute
#Final Project
#Text Analysis of Lewis Carroll's Alice in Wonderland
#15 December 2019

####LOAD PACKAGES AND LIBRARIES####
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("ggthemes")
#install.packages("gmodels")
#install.packages("broom")
#install.packages("GGally")

#install.packages("tidytext")
#install.packages("gutenbergr")#project gutenberg
#install.packages("tm")#to create and work with corpora
#install.packages("topicmodels")#for LDA topic models
#install.packages("Rpoppler") 
#install.packages("data.table")
#install.packages("stringr")

#install.packages("qdap")
#install.packages("RSQLite") #SQLite Interface 
#install.packages("SnowballC") #text stemming library
#install.packages("wordcloud") #for wordcloud visualizations
#install.packages("syuzhet") #for text sentiment analysis
#install.packages("quanteda") #for N-grams
#install.packages("textdata") #required for sentiment dictionaries

library(ggthemes)
library(ggplot2)
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
library(qdap)

library(RSQLite)
library(SnowballC)
library(wordcloud)
library(syuzhet)
library(quanteda)
library(textdata)


####DOWNLOAD LEWIS CARROLL'S ALICE IN WONDERLAND (AIW) FROM PROJECT GUTENBERG####
aiw_book <- gutenberg_download(gutenberg_id = 19033)
aiw_book #a tibble of 1299 x2 (this includes the pjt. gutenberg id's) which can be turned into a tidy text dataset
#remove gutenberg_id variable
aiw_book_text <- aiw_book %>%
  select(-gutenberg_id)
aiw_book_text
summary(aiw_book_text)
    #text          
    #Length:1299 rows       
    #Class :character  
    #Mode  :character 
dim(aiw_book_text)

####CLEAN DOCUMENT AND CREATE CORPUS, DTM/TDM####
#create aiw corpus from book. 
aiw_source <- VectorSource(aiw_book_text)
aiw_corpus <- VCorpus(aiw_source)#aiw_corpus
aiw_corpus
    #Metadata:  corpus specific: 0, document level (indexed): 0
    #Content:  documents: 1

#specify stopwords
#in addition to stopwords("en"), add illustration, york, sons, company, 1916, gabirel, sam'l, v, vi, vii, viii, alice, dinah, sister, storyland, series, copyright, saml, alice's, alices
new_stops<-c("series_","_the","well", "way","now","illustration", "york", "sons", "company", "1916", "gabriel", "sam'l", "v", "vi", "vii", "viii", "alice", "dinah", "sister","storyland", "series", "copyright", "saml", "alice's", "alices", "said","like", "little", "went", "came", "one","just", stopwords("en"))
#also need a stopword list that doesn't include alice
wastops<-c("series_","_the","well", "way","now","illustration", "york", "sons", "company", "1916", "gabriel", "sam'l", "v", "vi", "vii", "viii","dinah", "sister","storyland", "series", "copyright", "saml","said","like", "little", "went", "came", "one","just", "alices", stopwords("en"))


#clean corpus
#create a function to clean the corpus
clean_corp <- function(corp){
  #lowercase {base r}
  corp<-tm_map(corp, content_transformer(tolower))
  #remove punctuation {tm}
  corp<-tm_map(corp, removePunctuation)
  #remove stopwords
  corp<-tm_map(corp, removeWords, words=new_stops)
  #strip whitespace {tm}
  corp<-tm_map(corp,stripWhitespace)
  return(corp)
}

#clean_corp function for the stopwords that do not include 'alice'
clean_wacorp <-function(corp){
  #lowercase {base r}
  corp<-tm_map(corp, content_transformer(tolower))
  #remove punctuation {tm}
  corp<-tm_map(corp, removePunctuation)
  #remove stopwords
  corp<-tm_map(corp, removeWords, words=wastops)
  #strip whitespace {tm}
  corp<-tm_map(corp,stripWhitespace)
  return(corp)
}

#clean the aiw_corpus with the clean_corp function and place in aiw_cleancorpus
aiw_cleancorpus <- clean_corp(aiw_corpus)
summary(aiw_cleancorpus)
str(aiw_cleancorpus)

#clean the aiw_corpus with the clean_wacorp function and place in aiw_cleanwacorpus
aiw_cleanwacorpus <- clean_wacorp(aiw_corpus)
summary(aiw_cleanwacorpus)
str(aiw_cleanwacorpus)

#create a term document matrix from aiw_cleancorpus
aiw_tdm <- TermDocumentMatrix(aiw_cleancorpus)
aiw_dtm <- DocumentTermMatrix(aiw_cleancorpus)
#term document matrix from aiw_cleanwacorpus
wa_aiw_tdm <- TermDocumentMatrix(aiw_cleanwacorpus)
wa_aiw_dtm <- DocumentTermMatrix(aiw_cleanwacorpus)

####TERM FREQUENCY VISUALIZATIONS####
#Previous review of the corpus showed that "alice" had the highest term frequency (163 counts). With "alice" removed, let's look at term frequency.
#convert tdm to matrix
aiw_mat <- as.matrix(aiw_tdm)

#sum rows and sort by frequency
aiw_termfreq <- rowSums(aiw_mat)
aiw_termfreq <- sort(aiw_termfreq, decreasing = TRUE)
summary(aiw_termfreq)
    #Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    #1.000   1.000   1.000   3.075   3.000 144.000 
glimpse(aiw_termfreq)

#plot frequency
#since the mean was 3.075, i want to see the top 50 terms that were used 3 or more times
barplot(aiw_termfreq[1:50],
        col = "dodgerblue",
        las = 3)

#let's see the next 50 for comparison and what characters are included
barplot(aiw_termfreq[51:100],
        col = "dodgerblue",
        las = 3)

#view top 100 words as a wordcloud and wordnetwork
#sum rows and sort by frequency to create aiw data frame from aiw_termfreq
aiw_freqsum<-rowSums(aiw_mat)
aiw_wordFreq <- data.frame(term=names(aiw_freqsum), num=aiw_freqsum)
#make a word cloud of top 100
wordcloud(aiw_wordFreq$term, 
          aiw_wordFreq$num,
          max.words = 100,
          colors = c("#416B8C","#6AAFE6","#8EC0E4"))
#wordcloud with "said" removed
glimpse(aiw_wordFreq)
aiw_wordFreq_sanssaid <- aiw_wordFreq %>%
  filter(term != "said")
wordcloud(aiw_wordFreq_sanssaid$term, 
           aiw_wordFreq_sanssaid$num,
           max.words = 100,
           colors = c("#416B8C","#6AAFE6","#8EC0E4"))
#add said to stopword list? yes.

####TOPIC MODELING####
###Topics###
##non-alice lda function
wonderland_tm_terms_by_topic <-function(input_corpus, plot=TRUE, number_of_topics=6, number_of_words=7, 
                                   path="lda-121519/aiw_lda_norm_topics") {
  aiw_dtm <- DocumentTermMatrix(input_corpus)
  #unique indexes
  unique_indexes<-unique(aiw_dtm$i)
  aiw_dtm <-aiw_dtm[unique_indexes,]
  #lda
  aiw_lda <-LDA(aiw_dtm, k=number_of_topics, control = list(seed=1234))
  aiw_topics<-tidy(aiw_lda, matrix="beta")
  aiw_lda_words <-terms(aiw_lda, number_of_words)
  aiw_lda_topics <-as.matrix(aiw_lda_words)
  write.csv(aiw_lda_topics, file = paste(path, number_of_topics,".csv"))
  aiw_top_terms_2<-aiw_topics%>%
    group_by(topic)%>%
    top_n(number_of_words, beta)%>%
    ungroup()%>%
    arrange(topic, -beta)
  
  if(plot==TRUE){
    aiw_top_terms_2%>%
      mutate(term=reorder(term,beta))%>%
      ggplot(aes(term, beta, fill=factor(topic)))+
      geom_col(show.legend = FALSE)+
      facet_wrap(~topic, scales="free")+
      coord_flip()+
      labs(title = "Topic Model for Alice in Wonderland (Alice Omitted)") 
  }
}

#alice_tm_function
alice_tm_terms_by_topic <-function(input_corpus, plot=TRUE, number_of_topics=6, number_of_words=7, 
                                   path="lda-121519/with-alice/alice_lda_norm_topics") {
  wa_aiw_dtm <- DocumentTermMatrix(input_corpus)
  #unique indexes
  unique_indexes<-unique(wa_aiw_dtm$i)
  wa_aiw_dtm <-wa_aiw_dtm[unique_indexes,]
  #lda
  wa_aiw_lda <-LDA(wa_aiw_dtm, k=number_of_topics, control = list(seed=1234))
  wa_topics<-tidy(wa_aiw_lda, matrix="beta")
  wa_aiw_lda_words <-terms(wa_aiw_lda, number_of_words)
  wa_aiw_lda_topics <-as.matrix(wa_aiw_lda_words)
  write.csv(wa_aiw_lda_topics, file = paste(path, number_of_topics,".csv"))
  wa_aiw_top_terms_2<-wa_topics%>%
    group_by(topic)%>%
    top_n(number_of_words, beta)%>%
    ungroup()%>%
    arrange(topic, -beta)
  
  if(plot==TRUE){
    wa_aiw_top_terms_2%>%
      mutate(term=reorder(term,beta))%>%
      ggplot(aes(term, beta, fill=factor(topic)))+
        geom_col(show.legend = FALSE)+
        facet_wrap(~topic, scales="free")+
        coord_flip()+
        labs(title = "Topic Model for Alice in Wonderland") 
  }
}

#functions run for 6 topics -- previous running showed 6 topics with 7 words to be a good balance of themes from the book both with and without 'alice'
alice_tm_terms_by_topic (aiw_cleanwacorpus, number_of_topics = 6, number_of_words = 7)
wonderland_tm_terms_by_topic (aiw_cleancorpus, number_of_topics = 6, number_of_words = 7)

#functions run for 2 topics--
alice_tm_terms_by_topic (aiw_cleanwacorpus, number_of_topics = 2, number_of_words = 7)
wonderland_tm_terms_by_topic (aiw_cleancorpus, number_of_topics = 2, number_of_words = 7)


#alice_tm but normalized for alice
#normalize for alice six topics
k<-6
wa_aiw_lda <-LDA(wa_aiw_dtm, k=k, control = list(seed=1234))
wa_aiw_lda
wa_aiw_lda_words <-terms(wa_aiw_lda, 7)
wa_aiw_lda_topics <-as.matrix(wa_aiw_lda_words)
head(wa_aiw_lda_topics)
write.csv(wa_aiw_lda_topics, file = paste("lda-121519/with-alice/wa_lda_norm",k,".csv"))
#visualize
wa_aiw_lda_tidy<-tidy(wa_aiw_lda, matrix="beta")
wa_aiw_lda_tidy
wa_aiw_lda_tidy_norm <- wa_aiw_lda_tidy%>%
  mutate(betanorm=((beta - min(beta)) / (max(beta) - min(beta))))
wa_aiw_lda_tidy_norm
#order words from most prominent to least
wa_aiw_top_terms_norm<-wa_aiw_lda_tidy_norm%>%
  group_by(topic)%>%
  top_n(7,betanorm)%>%
  ungroup()%>%
  arrange(topic, -betanorm)
wa_aiw_top_terms_norm
#plot
wa_aiw_top_terms_norm%>%
  mutate(term=reorder(term,betanorm))%>%
  ggplot(aes(term, betanorm, fill=factor(topic)))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~topic, scales="free")+
  coord_flip()+
  labs(title = "Topic Model for Alice in Wonderland (Normalized for Alice)")

#normalize for alice 12 topics
k<-12
wa_aiw_lda12 <-LDA(wa_aiw_dtm, k=k, control = list(seed=1234))
wa_aiw_lda12
wa_aiw_lda12_words <-terms(wa_aiw_lda12, 7)
wa_aiw_lda12_topics <-as.matrix(wa_aiw_lda12_words)
head(wa_aiw_lda12_topics)
write.csv(wa_aiw_lda12_topics, file = paste("lda-121519/with-alice/wa_lda_norm",k,".csv"))
#visualize
wa_aiw_lda12_tidy<-tidy(wa_aiw_lda12, matrix="beta")
wa_aiw_lda12_tidy
wa_aiw_lda12_tidy_norm <- wa_aiw_lda12_tidy%>%
  mutate(betanorm=((beta - min(beta)) / (max(beta) - min(beta))))
wa_aiw_lda12_tidy_norm
#order words from most prominent to least
wa_aiw_top_terms_norm12<-wa_aiw_lda12_tidy_norm%>%
  group_by(topic)%>%
  top_n(7,betanorm)%>%
  ungroup()%>%
  arrange(topic, -betanorm)
wa_aiw_top_terms_norm12
#plot
wa_aiw_top_terms_norm12%>%
  mutate(term=reorder(term,betanorm))%>%
  ggplot(aes(term, betanorm, fill=factor(topic)))+
  geom_col(show.legend = FALSE)+
    facet_wrap(~topic, scales="free")+
    coord_flip()+
    labs(title = "Twelve Topics for Alice in Wonderland (Normalized for Alice)")


####SENTIMENT ANALYSIS####
#After reviewing the four dictionaries (NRC, AFINN, Loughran, and Bing) available with R
#the most suited to the this material is NRC. 
#NRC had a good mix of emotional terms that related to the story. 

# Count the number of words associated with each sentiment in nrc
nrc<-get_sentiments("nrc")%>%
  count(sentiment)%>%
  arrange(desc(n))
#All terms of NRC are good descriptions of Alice's emotions throughout the story.

#APPENDING DICTIONARIES
#create new tidy dataframes for  texts (w/o alice term) from previous dfs
aiwsenttidy <-aiw_wordFreq_sanssaid%>%
  mutate(word=term, count=num)%>%
  select(-term,-num)%>%
  arrange(desc(count))
head(aiwsenttidy)  
#append the nrc dictionary 
aiwsenttidynrc<-aiwsenttidy%>%
  inner_join(get_sentiments("nrc"))
 
#-------------------------------------------------------------------#
  #CANNOT USE THIS AS CHARACTERS WERE REMOVED DURING THE JOIN TO THE SENTIMENT LEXICONS-----------
  #create new tidy dataframes for  texts (w/alice term) from previous dfs
  #convert wa_aiw_tdm to matrix
  #wa_aiw_mat <- as.matrix(wa_aiw_tdm)
  #sum rows and sort by frequency
  #wa_aiw_termfreq <- rowSums(wa_aiw_mat)
  #wa_aiw_termfreq <- sort(wa_aiw_termfreq, decreasing = TRUE)
  #wa_aiw_termfreq
  #sum rows and sort by frequency to create aiw data frame from aiw_termfreq
  #wa_aiw_freqsum<-rowSums(wa_aiw_mat)
  #wa_aiw_wordFreq <- data.frame(term=names(wa_aiw_freqsum), num=wa_aiw_freqsum)
  #wa_aiw_wordFreq
  #create new tidy dataframes for  texts (w/alice term) from previous dfs
  #wa_aiwsenttidy <-wa_aiw_wordFreq%>%
  #  mutate(word=term, count=num)%>%
  #  select(-term,-num)%>%
  #  arrange(desc(count))
  #head(wa_aiwsenttidy)  
  
  #append the nrc dictionary 
  #wa_aiwsenttidynrc<-wa_aiwsenttidy%>%
  #  inner_join(get_sentiments("nrc"))
  #many of the characters were removed during the join
  
  #wa_aiwsenttidyafinn<-wa_aiwsenttidy%>%
  #  inner_join(get_sentiments("afinn"))
  #many of the characters were removed during the join
  #-------------------------------------------------------------------#
####SENTIMENT ANALYSIS CONTINUED####
#visualize positive and negative sentiment in the nrc
aiwnrp_n <- aiwsenttidynrc %>%
  filter(sentiment %in% c("positive", "negative"))%>%
  group_by(sentiment)
summary(aiwnrp_n) #mean count = 2.335

aiwnrp_n <- aiwsenttidynrc %>%
  filter(sentiment %in% c("positive", "negative"))%>%
  group_by(sentiment)%>%
  filter(count>=2.335)%>%
  ungroup()%>%
  mutate(word=reorder(word, count))
ggplot(aiwnrp_n, aes(word, count, fill=sentiment))+
    geom_col(show.legend = FALSE)+
    facet_wrap(~sentiment, scales = "free_y")+
    labs(y= "Contribution to Sentiment as measured by NRC (Average Sentiment=2.335)", x=NULL, title = "Overall Sentiment of the Most Frequented Terms in Alice in Wonderland (Not including the term Alice)" )+
    coord_flip()

#remove positive and negative sentiment and visualize each of the eight emotions
#corresponding to plutchik's wheel of emotion
aiwemotion <- aiwsenttidynrc%>%
  filter(!grepl("positive|negative",sentiment))%>%
  group_by(sentiment)%>%
  ungroup()
summary(aiwemotion) #mean 2.39, length=467

aiwemoplot <- aiwemotion%>%
  group_by(sentiment)%>%
  filter(count>=2.39)%>%
  ungroup()%>%
  mutate(word=reorder(word, count))
ggplot(aiwemoplot, aes(word, count, fill=sentiment))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y= "Contribution to Sentiment as measured by NRC (Average Sentiment=2.39)", x=NULL, title = "Plutchik's Sentiments in Alice in Wonderland (Not including the term Alice)" )+
  coord_flip()


####N-GRAMS####
#  wa_aiw_bigrams_sep <-wa_aiw_bigrams%>%
#    separate(bigram, c("word1", "word2"), sep = " ")
#  wa_aiw_bigrams_filt <- wa_aiw_bigrams_sep %>%#filter for stop words
#    filter(!word1 %in% wastops)%>%
#    filter(!word2 %in% wastops)
#  wa_aiw_bigrams_count<-wa_aiw_bigrams_filt%>%#count bigrams
#    count(word1, word2, sort = TRUE)
wa_aiw_dtm_tidy<-tidy(wa_aiw_dtm)
wa_aiw_tfidf<-bind_tf_idf(wa_aiw_dtm_tidy, term, document, count)%>%#find words that are important but not too common
  select(-document) %>% #there's only 1 document so let's elim the document column
  arrange(desc(tf)) #look at terms with high tf_idf
head(wa_aiw_tfidf, 20)
summary(wa_aiw_tfidf)#mean count 2.919
write.table(wa_aiw_tfidf,file = "INFO640-Brown-FinalProject-121519/alice-tfterms.txt", sep = ",", quote = FALSE, row.names = F)
#bigrams part 1
wa_aiw_bigrams<- wa_aiw_tfidf%>%
  unnest_tokens(bigram, term, token = "ngrams", n=2)%>%
  arrange(desc(tf))
wa_aiw_bigrams #all characters turned into NA values
write.table(wa_aiw_bigrams, file = "INFO640-Brown-FinalProject-121519/alice-bigrams.txt", sep = ",", quote = FALSE, row.names = F)
summary(wa_aiw_bigrams) #mean count = 2.795
wa_aiw_bigrams 
#trigrams part 1
wa_aiw_trigrams<-wa_aiw_tfidf%>%
  unnest_tokens(trigram, term, token = "ngrams", n=3)%>%
  arrange(desc(tf))
wa_aiw_trigrams #all characters turned into NA values
write.table(wa_aiw_trigrams, file = "INFO640-Brown-FinalProject-121519/alice-trigrams.txt", sep = ",", quote = FALSE, row.names = F)
summary(wa_aiw_trigrams) #mean count = 2.681

#bigrams part 2 (keep characters?)  
wa_aiw_bigrams2<-aiw_book_text %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)
wa_aiw_bigrams2
wa_aiw_bigrams2_sep <-wa_aiw_bigrams2%>%
  separate(bigram, c("word1", "word2"), sep = " ")
wa_aiw_bigrams2_filt <- wa_aiw_bigrams2_sep %>%#filter for stop words
  filter(!word1 %in% wastops)%>%
  filter(!word2 %in% wastops)
wa_aiw_bigram2_count <-wa_aiw_bigrams2_filt%>%#count bigrams
  count(word1, word2, sort = TRUE)
wa_aiw_bigram2_count
write.table(wa_aiw_bigram2_count, file = "INFO640-Brown-FinalProject-121519/alice-bigrams2.txt", sep = ",", quote = FALSE, row.names = F)
summary(wa_aiw_bigram2_count) #mean count = 1.163
#trigrams part 2 
wa_aiw_trigrams2<-aiw_book_text %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3)
  wa_aiw_trigrams2
wa_aiw_trigrams2_sep <-wa_aiw_trigrams2%>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")
wa_aiw_trigrams2_filt <- wa_aiw_trigrams2_sep %>%#filter for stop words
  filter(!word1 %in% wastops)%>%
  filter(!word2 %in% wastops)%>%
  filter(!word3 %in% wastops)
wa_aiw_trigram2_count <-wa_aiw_trigrams2_filt%>%#count bigrams
  count(word1, word2, word3, sort = TRUE)
wa_aiw_trigram2_count
write.table(wa_aiw_trigram2_count, file = "INFO640-Brown-FinalProject-121519/alice-trigrams2.txt", sep = ",", quote = FALSE, row.names = F)
summary(wa_aiw_trigram2_count) #mean count = 1.037



