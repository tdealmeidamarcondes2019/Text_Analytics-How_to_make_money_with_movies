######################## MOVIES WITH HIGHEST REVENUES ###########################

# Loading packages: ####
library(tidyverse)
library(textreadr)
library(tidytext)
library(wordcloud)
library(RColorBrewer)
library(reshape2)
library(plotly)

# Importing top 5 highest revenues per decade: ####
## 2010's:
setwd("C:/Users/thiag/Thiago/Hult/Text Analytics/Individual Assignment/2010")
mov_10 <- list.files(path="C:/Users/thiag/Thiago/Hult/Text Analytics/Individual Assignment/2010")
# Using read document to import the data:
mov_10_1 <- read_document(file=mov_10[1])
mov_10_2 <- read_document(file=mov_10[2])
mov_10_3 <- read_document(file=mov_10[3])
mov_10_4 <- read_document(file=mov_10[4])
mov_10_5 <- read_document(file=mov_10[5])

## 2000's:
setwd("C:/Users/thiag/Thiago/Hult/Text Analytics/Individual Assignment/2000")
mov_00 <- list.files(path="C:/Users/thiag/Thiago/Hult/Text Analytics/Individual Assignment/2000")
# Using read document to import the data:
mov_00_1 <- read_document(file=mov_00[1])
mov_00_2 <- read_document(file=mov_00[2])
mov_00_3 <- read_document(file=mov_00[3])
mov_00_4 <- read_document(file=mov_00[4])
mov_00_5 <- read_document(file=mov_00[5])

## 1990's:
setwd("C:/Users/thiag/Thiago/Hult/Text Analytics/Individual Assignment/1990")
mov_90 <- list.files(path="C:/Users/thiag/Thiago/Hult/Text Analytics/Individual Assignment/1990")
# Using read document to import the data:
mov_90_1 <- read_document(file=mov_90[1])
mov_90_2 <- read_document(file=mov_90[2])
mov_90_3 <- read_document(file=mov_90[3])
mov_90_4 <- read_document(file=mov_90[4])
mov_90_5 <- read_document(file=mov_90[5])

## 1980's:
setwd("C:/Users/thiag/Thiago/Hult/Text Analytics/Individual Assignment/1980")
mov_80 <- list.files(path="C:/Users/thiag/Thiago/Hult/Text Analytics/Individual Assignment/1980")
# Using read document to import the data:
mov_80_1 <- read_document(file=mov_80[1])
mov_80_2 <- read_document(file=mov_80[2])
mov_80_3 <- read_document(file=mov_80[3])
mov_80_4 <- read_document(file=mov_80[4])
mov_80_5 <- read_document(file=mov_80[5])

# Converting each movie in a dataframe: ####

## 2010's:
df_mov_10_1 <- data_frame(line=1:7945, text=mov_10_1)
df_mov_10_2 <- data_frame(line=1:5896, text=mov_10_2)
df_mov_10_3 <- data_frame(line=1:5202, text=mov_10_3)
df_mov_10_4 <- data_frame(line=1:5803, text=mov_10_4)
df_mov_10_5 <- data_frame(line=1:5191, text=mov_10_5)

## 2000's:
df_mov_00_1 <- data_frame(line=1:4993, text=mov_00_1)
df_mov_00_2 <- data_frame(line=1:4597, text=mov_00_2)
df_mov_00_3 <- data_frame(line=1:4628, text=mov_00_3)
df_mov_00_4 <- data_frame(line=1:6253, text=mov_00_4)
df_mov_00_5 <- data_frame(line=1:5926, text=mov_00_5)

## 1990's:
df_mov_90_1 <- data_frame(line=1:5308, text=mov_90_1)
df_mov_90_2 <- data_frame(line=1:3864, text=mov_90_2)
df_mov_90_3 <- data_frame(line=1:5352, text=mov_90_3)
df_mov_90_4 <- data_frame(line=1:3890, text=mov_90_4)
df_mov_90_5 <- data_frame(line=1:7901, text=mov_90_5)

## 1980's:
df_mov_80_1 <- data_frame(line=1:3792, text=mov_80_1)
df_mov_80_2 <- data_frame(line=1:4017, text=mov_80_2)
df_mov_80_3 <- data_frame(line=1:4580, text=mov_80_3)
df_mov_80_4 <- data_frame(line=1:4735, text=mov_80_4)
df_mov_80_5 <- data_frame(line=1:3875, text=mov_80_5)

# Tokenizing each dataframe: ####
mov_10_1_tok <- df_mov_10_1 %>%
  unnest_tokens(word,text)
mov_10_2_tok <- df_mov_10_2 %>%
  unnest_tokens(word,text)
mov_10_3_tok <- df_mov_10_3 %>%
  unnest_tokens(word,text)
mov_10_4_tok <- df_mov_10_4 %>%
  unnest_tokens(word,text)
mov_10_5_tok <- df_mov_10_5 %>%
  unnest_tokens(word,text)
mov_00_1_tok <- df_mov_00_1 %>%
  unnest_tokens(word,text)
mov_00_2_tok <- df_mov_00_2 %>%
  unnest_tokens(word,text)
mov_00_3_tok <- df_mov_00_3 %>%
  unnest_tokens(word,text)
mov_00_4_tok <- df_mov_00_4 %>%
  unnest_tokens(word,text)
mov_00_5_tok <- df_mov_00_5 %>%
  unnest_tokens(word,text)
mov_90_1_tok <- df_mov_90_1 %>%
  unnest_tokens(word,text)
mov_90_2_tok <- df_mov_90_2 %>%
  unnest_tokens(word,text)
mov_90_3_tok <- df_mov_90_3 %>%
  unnest_tokens(word,text)
mov_90_4_tok <- df_mov_90_4 %>%
  unnest_tokens(word,text)
mov_90_5_tok <- df_mov_90_5 %>%
  unnest_tokens(word,text)
mov_80_1_tok <- df_mov_80_1 %>%
  unnest_tokens(word,text)
mov_80_2_tok <- df_mov_80_2 %>%
  unnest_tokens(word,text)
mov_80_3_tok <- df_mov_80_3 %>%
  unnest_tokens(word,text)
mov_80_4_tok <- df_mov_80_4 %>%
  unnest_tokens(word,text)
mov_80_5_tok <- df_mov_80_5 %>%
  unnest_tokens(word,text)

# Creating dictionary to remove numbers: ####
num_1 <- as.character(c(1:100))
stop_num_1 <- data_frame(word=num_1,lexicon="cust")
num_2 <- as.character(c("00","01","02","03","04","05","06","07","08","09",
                        "font", "color", "ffff00", "ff0000",
                        "ª", "â", "d900d9", "ff2424", "bb", "elliott", "e.t",
                        "luke", "yoda", "batman", "alfred", "jack", "marcus",
                        "leia", "jabba", "chewie", "r2", "rose", "simba",
                        "pumba", "jedi", "david", "thanos", "rey", "hakuna",
                        "mufasa", "zach", "hermione", "harry", "jake", "lord", 
                        "harvey", "tony", "gamora","groot", "stark", "claire",
                        "gray","pumbaa","zazu","grace", "neytiri", "eywa","norm",
                        "avatar", "potter","ron","hagrid","hogwarts","dumbledore",
                        "sparrow","elizabeth","jones","turner","dent","wayne",
                        "joker","frodo","agol","gondor","gandalf","mesa","scar",
                        "dawson","gotham","mary","gertie","mike","vader","skywalker",
                        "jones","solo", "hulk"))
stop_num_2 <- data_frame(word=num_2,lexicon="cust")


# Removing stop words per movie: ####
tidy_mov_10_1 <- df_mov_10_1 %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(stop_num_1) %>%
  anti_join(stop_num_2)
  
tidy_mov_10_2 <- df_mov_10_2 %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(stop_num_1) %>%
  anti_join(stop_num_2)
tidy_mov_10_3 <- df_mov_10_3 %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(stop_num_1) %>%
  anti_join(stop_num_2)
tidy_mov_10_4 <- df_mov_10_4 %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(stop_num_1) %>%
  anti_join(stop_num_2)
tidy_mov_10_5 <- df_mov_10_5 %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(stop_num_1) %>%
  anti_join(stop_num_2)
tidy_mov_00_1 <- df_mov_00_1 %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(stop_num_1) %>%
  anti_join(stop_num_2)
tidy_mov_00_2 <- df_mov_00_2 %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(stop_num_1) %>%
  anti_join(stop_num_2)
tidy_mov_00_3 <- df_mov_00_3 %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(stop_num_1) %>%
  anti_join(stop_num_2)
tidy_mov_00_4 <- df_mov_00_4 %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(stop_num_1) %>%
  anti_join(stop_num_2)
tidy_mov_00_5 <- df_mov_00_5 %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(stop_num_1) %>%
  anti_join(stop_num_2)
tidy_mov_90_1 <- df_mov_90_1 %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(stop_num_1) %>%
  anti_join(stop_num_2)
tidy_mov_90_2 <- df_mov_90_2 %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(stop_num_1) %>%
  anti_join(stop_num_2)
tidy_mov_90_3 <- df_mov_90_3 %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(stop_num_1) %>%
  anti_join(stop_num_2)
tidy_mov_90_4 <- df_mov_90_4 %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(stop_num_1) %>%
  anti_join(stop_num_2)
tidy_mov_90_5 <- df_mov_90_5 %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(stop_num_1) %>%
  anti_join(stop_num_2)
tidy_mov_80_1 <- df_mov_80_1 %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(stop_num_1) %>%
  anti_join(stop_num_2)
tidy_mov_80_2 <- df_mov_80_2 %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(stop_num_1) %>%
  anti_join(stop_num_2)
tidy_mov_80_3 <- df_mov_80_3 %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(stop_num_1) %>%
  anti_join(stop_num_2)
tidy_mov_80_4 <- df_mov_80_4 %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(stop_num_1) %>%
  anti_join(stop_num_2)
tidy_mov_80_5 <- df_mov_80_5 %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(stop_num_1) %>%
  anti_join(stop_num_2)

# Counting frequency per movie: ####
freq_mov_10_1 <- df_mov_10_1 %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(stop_num_1) %>%
  anti_join(stop_num_2) %>%
  count(word, sort=TRUE)
freq_mov_10_2 <- df_mov_10_2 %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(stop_num_1) %>%
  anti_join(stop_num_2) %>%
  count(word, sort=TRUE)
freq_mov_10_3 <- df_mov_10_3 %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(stop_num_1) %>%
  anti_join(stop_num_2) %>%
  count(word, sort=TRUE)
freq_mov_10_4 <- df_mov_10_4 %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(stop_num_1) %>%
  anti_join(stop_num_2) %>%
  count(word, sort=TRUE)
freq_mov_10_5 <- df_mov_10_5 %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(stop_num_1) %>%
  anti_join(stop_num_2) %>%
  count(word, sort=TRUE)
freq_mov_00_1 <- df_mov_00_1 %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(stop_num_1) %>%
  anti_join(stop_num_2) %>%
  count(word, sort=TRUE)
freq_mov_00_2 <- df_mov_00_2 %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(stop_num_1) %>%
  anti_join(stop_num_2) %>%
  count(word, sort=TRUE)
freq_mov_00_3 <- df_mov_00_3 %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(stop_num_1) %>%
  anti_join(stop_num_2) %>%
  count(word, sort=TRUE)
freq_mov_00_4 <- df_mov_00_4 %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(stop_num_1) %>%
  anti_join(stop_num_2) %>%
  count(word, sort=TRUE)
freq_mov_00_5 <- df_mov_00_5 %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(stop_num_1) %>%
  anti_join(stop_num_2) %>%
  count(word, sort=TRUE)
freq_mov_90_1 <- df_mov_90_1 %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(stop_num_1) %>%
  anti_join(stop_num_2) %>%
  count(word, sort=TRUE)
freq_mov_90_2 <- df_mov_90_2 %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(stop_num_1) %>%
  anti_join(stop_num_2) %>%
  count(word, sort=TRUE)
freq_mov_90_3 <- df_mov_90_3 %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(stop_num_1) %>%
  anti_join(stop_num_2) %>%
  count(word, sort=TRUE)
freq_mov_90_4 <- df_mov_90_4 %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(stop_num_1) %>%
  anti_join(stop_num_2) %>%
  count(word, sort=TRUE)
freq_mov_90_5 <- df_mov_90_5 %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(stop_num_1) %>%
  anti_join(stop_num_2) %>%
  count(word, sort=TRUE)
freq_mov_80_1 <- df_mov_80_1 %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(stop_num_1) %>%
  anti_join(stop_num_2) %>%
  count(word, sort=TRUE)
freq_mov_80_2 <- df_mov_80_2 %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(stop_num_1) %>%
  anti_join(stop_num_2) %>%
  count(word, sort=TRUE)
freq_mov_80_3 <- df_mov_80_3 %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(stop_num_1) %>%
  anti_join(stop_num_2) %>%
  count(word, sort=TRUE)
freq_mov_80_4 <- df_mov_80_4 %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(stop_num_1) %>%
  anti_join(stop_num_2) %>%
  count(word, sort=TRUE)
freq_mov_80_5 <- df_mov_80_5 %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words) %>%
  anti_join(stop_num_1) %>%
  anti_join(stop_num_2) %>%
  count(word, sort=TRUE)
# Sentiments: ####
afinn <- get_sentiments("afinn")
nrc <- get_sentiments("nrc")
bing <- get_sentiments("bing")

sentiments <- bind_rows(mutate(afinn, lexicon="afinn"),
                        mutate(nrc, lexicon= "nrc"),
                        mutate(bing, lexicon="bing")
)

unique(nrc$sentiment)

# Analyzing frequency per decade (binding): ####

# Creating empty dataframe:
a <- 7945
b <- 3
df_mov_10 <- as.data.frame(matrix(nrow=a, ncol=b))
c <- 6253
df_mov_00 <- as.data.frame(matrix(nrow=c, ncol=b))
d <- 7901
df_mov_90 <- as.data.frame(matrix(nrow=d, ncol=b))
e <- 4735
df_mov_80 <- as.data.frame(matrix(nrow=e, ncol=b))


# Binding rows:
df_mov_10 <- bind_rows(
  mutate(freq_mov_10_1, movie='1'),
  mutate(freq_mov_10_2, movie='2'),
  mutate(freq_mov_10_3, movie='3'),
  mutate(freq_mov_10_4, movie='4'),
  mutate(freq_mov_10_5, movie='5')
)
df_mov_00 <- bind_rows(
  mutate(freq_mov_00_1, movie='1'),
  mutate(freq_mov_00_2, movie='2'),
  mutate(freq_mov_00_3, movie='3'),
  mutate(freq_mov_00_4, movie='4'),
  mutate(freq_mov_00_5, movie='5')
)
df_mov_90 <- bind_rows(
  mutate(freq_mov_90_1, movie='1'),
  mutate(freq_mov_90_2, movie='2'),
  mutate(freq_mov_90_3, movie='3'),
  mutate(freq_mov_90_4, movie='4'),
  mutate(freq_mov_90_5, movie='5')
)
df_mov_80 <- bind_rows(
  mutate(freq_mov_80_1, movie='1'),
  mutate(freq_mov_80_2, movie='2'),
  mutate(freq_mov_80_3, movie='3'),
  mutate(freq_mov_80_4, movie='4'),
  mutate(freq_mov_80_5, movie='5')
)

# Including proportion per word:
df_mov_10 <- df_mov_10 %>%
  bind_tf_idf(word, movie, n)
df_mov_00 <- df_mov_00 %>%
  bind_tf_idf(word, movie, n)
df_mov_90 <- df_mov_90 %>%
  bind_tf_idf(word, movie, n)
df_mov_80 <- df_mov_80 %>%
  bind_tf_idf(word, movie, n)

# Plotting frequecy (movies per decade): ####
df_mov_10 %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(movie) %>%
  top_n(5) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=movie))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~movie, ncol=2, scales="free")+
  coord_flip()
df_mov_00 %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(movie) %>%
  top_n(5) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=movie))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~movie, ncol=2, scales="free")+
  coord_flip()
df_mov_90 %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(movie) %>%
  top_n(5) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=movie))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~movie, ncol=2, scales="free")+
  coord_flip()
df_mov_80 %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(movie) %>%
  top_n(5) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=movie))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~movie, ncol=2, scales="free")+
  coord_flip()


# Consolidating analysis per decade: ####
## Frequency consolidating top 5 2010's movies:
cons_freq_10 <- df_mov_10 %>%
  group_by(word) %>%
  summarise(sum_word = sum(n)) %>%
  arrange(desc(sum_word))
cons_freq_10

## Frequency consolidating top 5 2000's movies:
cons_freq_00 <- df_mov_00 %>%
  group_by(word) %>%
  summarise(sum_word = sum(n)) %>%
  arrange(desc(sum_word))
cons_freq_00

## Frequency consolidating top 5 1990's movies:
cons_freq_90 <- df_mov_90 %>%
  group_by(word) %>%
  summarise(sum_word = sum(n)) %>%
  arrange(desc(sum_word))
cons_freq_90

## Frequency consolidating top 5 1980's movies:
cons_freq_80 <- df_mov_80 %>%
  group_by(word) %>%
  summarise(sum_word = sum(n)) %>%
  arrange(desc(sum_word))
cons_freq_80

# Getting frequency of sentiments per decade: ####
## NRC | 2010's movies
nrc_mov_10 <- df_mov_10 %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

sent_freq_10<-nrc_mov_10 %>%
  group_by(sentiment) %>%
  summarise(sum_n = sum(n),
            "share(%)" = round((sum_n/4053)*100,digits=1))%>%
  arrange(desc(sum_n))
sent_freq_10

## NRC | 2000's movies
nrc_mov_00 <- df_mov_00 %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

sent_freq_00<-nrc_mov_00 %>%
  group_by(sentiment) %>%
  summarise(sum_n = sum(n),
            "share(%)" = round((sum_n/4764)*100,digits=1))%>%
  arrange(desc(sum_n))
sent_freq_00

## NRC | 1990's movies
nrc_mov_90 <- df_mov_90 %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

sent_freq_90<-nrc_mov_90 %>%
  group_by(sentiment) %>%
  summarise(sum_n = sum(n),
            "share(%)" = round((sum_n/4364)*100,digits=1))%>%
  arrange(desc(sum_n))
sent_freq_90

## NRC | 1980's movies
nrc_mov_80 <- df_mov_80 %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

sent_freq_80<-nrc_mov_80 %>%
  group_by(sentiment) %>%
  summarise(sum_n = sum(n),
            "share(%)" = round((sum_n/3523)*100,digits=1))%>%
  arrange(desc(sum_n))
sent_freq_80

# Plotting sentiments frequency per decade: ####
# Barplot (ranking of NRC sentiments): ####

## 2010's movies
plot_sent_10 <-sent_freq_10 %>%
  mutate(sentiment = reorder(sentiment,sum_n)) %>%
  ggplot(aes(sentiment, sum_n))+
  geom_col()+
  xlab(NULL)+
  ylab(NULL)+
  coord_flip() +
  ggtitle("Top 5 2010's movies")
ggplotly(plot_sent_10)

## 2000's movies
plot_sent_00 <-sent_freq_00 %>%
  mutate(sentiment = reorder(sentiment,sum_n)) %>%
  ggplot(aes(sentiment, sum_n))+
  geom_col()+
  xlab(NULL)+
  ylab(NULL)+
  coord_flip() +
  ggtitle("Top 5 2000's movies")
ggplotly(plot_sent_00)

## 1990's movies
plot_sent_90 <-sent_freq_90 %>%
  mutate(sentiment = reorder(sentiment,sum_n)) %>%
  ggplot(aes(sentiment, sum_n))+
  geom_col()+
  xlab(NULL)+
  ylab(NULL)+
  coord_flip() +
  ggtitle("Top 5 1990's movies")
ggplotly(plot_sent_90)

## 1980's movies
plot_sent_80 <-sent_freq_80 %>%
  mutate(sentiment = reorder(sentiment,sum_n)) %>%
  ggplot(aes(sentiment, sum_n))+
  geom_col()+
  xlab(NULL)+
  ylab(NULL)+
  coord_flip() +
  ggtitle("Top 5 1980's movies")
ggplotly(plot_sent_80)

# Wordcloud (consolidated per decade): ####
cons_freq_10 %>%
  with(wordcloud(word, sum_word, max.words = 100, random.order=FALSE, 
                 rot.per=0.35, use.r.layout=FALSE, 
                 colors=brewer.pal(8,"Dark2")))

cons_freq_00 %>%
  with(wordcloud(word, sum_word, max.words = 100, random.order=FALSE, 
                 rot.per=0.35, use.r.layout=FALSE, 
                 colors=brewer.pal(8,"Dark2")))

cons_freq_90 %>%
  with(wordcloud(word, sum_word, max.words = 100, random.order=FALSE, 
                 rot.per=0.35, use.r.layout=FALSE, 
                 colors=brewer.pal(8,"Dark2")))

cons_freq_80 %>%
  with(wordcloud(word, sum_word, max.words = 100, random.order=FALSE, 
                 rot.per=0.35, use.r.layout=FALSE, 
                 colors=brewer.pal(8,"Dark2")))
