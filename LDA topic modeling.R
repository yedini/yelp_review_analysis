library(tidyverse)
rm(list=ls())
setwd("C:/Users/dcng/Documents/비타민_프로젝트")


#===clustering 결과에 따른 business열 분류=====
review <- readRDS("review2.rds", refhook=NULL)
business <- readRDS("business.rds", refhook=NULL)
review <- review %>% select(review_id, business_id, text)

c1_business <- business %>% filter(categories %in% c('Bars', 'Food', 'Restaurants')) %>%
  select(business_id, categories)
c2_business <- business %>% filter(categories %in% c('Active Life', 'Beauty & Spas', 'Health & Medical', 'Hotels & Travel',
                                                     'Self Services', 'Services', 'Shopping')) %>%
  select(business_id, categories)
c3_business <- business %>% filter(categories %in% c('Education', 'Entertainment', 'Mass Media',
                                                     'Religious Organizations', 'Transportation', 'null')) %>%
  select(business_id, categories)

rm(business)

#join
c1 <- review %>% inner_join(c1_business, by="business_id")


library(tm)


####FindTopicNumber####
c1_doc <- c1 %>% select(business_id, text)
str(c1_doc)
#random sampling data
library(caret)
num <- createDataPartition(c1$categories, p=0.0038091987, list=FALSE)
c1_1 <- c1[num,]
c1_doc <- c1_1 %>% select(business_id, text)
#return NA instead of tolower error
tryTolower <- function(x) {
  y=NA
  try_error <- tryCatch(tolower(x), error=function(e) e)
  if(!inherits(try_error, 'error'))
    y=tolower(x)
  return(y)
}

#custom preprocessing function
clean.corpus <- function(corpus) {
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, stopwords('english'))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  return(corpus)
}

names(c1_doc) <- c("doc_id", "text")
corpus <- VCorpus(DataframeSource(c1_doc))
corpus <- clean.corpus(corpus)
dtm <- DocumentTermMatrix(corpus, control=list(weighting=weightTf))

library(ldatuning)
result <- FindTopicsNumber(
  dtm,
  topics=seq(6, 26, by=2),
  metrics=c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method="Gibbs",
  control=list(seed=77),
  mc.cores=8,
  verbose=TRUE
)
FindTopicsNumber_plot(result)