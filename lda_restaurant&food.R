library(tidyverse)
setwd("C:/Users/dcng/Documents/비타민_프로젝트")
library(tm)
library(qdap)
library(caret)
library(pbapply)
library(textstem)      
library(lda)

####restaurant, food 전처리####

business <- readRDS("business.rds")
review1_text <- readRDS("review1_text.rds")
review1_score <- readRDS("review1_score.rds")
review1_text$business_id <- review1_score$business_id
rm(review1_score)
##joining
texts <- business %>%
  select(c("business_id","categories","name","city")) %>%
  inner_join(review1_text)
table(texts$categories)
##seperation
restaurant <- texts %>%
  filter(categories == "Restaurants") %>% as.tibble()
food <- texts %>%
  filter(categories == "Food") %>% as.tibble()

##sampling
num1 <- createDataPartition(restaurant$categories, p=0.02, list=FALSE)
rest1 <- restaurant[num1,]
num2 <- createDataPartition(food$categories, p=0.2, list=FALSE)
food1 <- food[num1,]



##removing "\\n"
rest1$text <- mgsub(c("\\\\n","\\n","\\"),"",rest1$text)
food1$text <- mgsub(c("\\\\n","\\n",'\\'),"",food1$text)


##business name -> 문제있음
names <- removeNumbers(tolower(business$name))
##cleansing -> 동일한 상호명이 너무 많음 
head(duplicated(names),1000)
head(names == "subway", 1000)
length(which(names == "subway"))
sum(duplicated(names))

## categories name
rest1_names <- rest1$name
sum(duplicated(rest1_names))
rest1_names <- rest1_names[!duplicated(rest1_names)]
rest1_names <- removeNumbers(tolower(rest1_names))

food1_names <- food1$name
sum(duplicated(food1_names))
food1_names <- food1_names[!duplicated(food1_names)]
food1_names <- removeNumbers(tolower(food1_names))




####restaurant LDA####

##전처리 1차
rest_text <- removePunctuation(removeWords(removeNumbers(tolower(rest1$text)), 
                                           c(stopwords('en'),stopwords('danish'), stopwords('dutch'), stopwords('finnish'), 
                                             stopwords('french'), stopwords('german'), stopwords('hungarian'), 
                                             stopwords('italian'), stopwords('norwegian'), stopwords('portuguese'), 
                                             stopwords('russian'),stopwords('swedish'))))

rest_text <- lemmatize_strings(rest_text)
rest_text <- removeWords(rest_text, rest1_names[1:1500])
rest_text <- removeWords(rest_text, rest1_names[1501:3000])
rest_text <- removeWords(rest_text, rest1_names[3001:4500])
rest_text <- removeWords(rest_text, rest1_names[4501:6000])
rest_text <- removeWords(rest_text, rest1_names[6001:7500])
rest_text <- removeWords(rest_text, rest1_names[7501:9000])
rest_text <- removeWords(rest_text, rest1_names[9001:10245])


blank.removal <- function(x) {
  x <- unlist(strsplit(x, ' '))
  x <- subset(x, nchar(x)>0)
  x <- paste(x, collapse = ' ')
}

rest_text <- pblapply(rest_text, blank.removal)

documents <- lexicalize(rest_text)


#특정단어가 문서 전체에서 몇 번 사용되었는지
wc <- word.counts(documents$documents, documents$vocab)
head(wc)

set.seed(1234)
fit <- lda.collapsed.gibbs.sampler(documents = documents$documents, K=5, vocab=documents$vocab,
                                   num.iterations = 20, alpha=0.02, eta=0.02, initial=NULL,
                                   burnin = 0, compute.log.likelihood = TRUE)

top.topic.words(fit$topics, 40, by.score=T)

top.topic.documents(fit$document_sums, 4)



##전처리 2차
rest_text <- unlist(rest_text)
rest_text <- removeWords(rest_text, c("guy", "chocolate", "cream", "oyster", "vega", "seafood", "cake",
                                      "pasta", "always", "indian", "butter", "cheese", "pork", "good",
                                      "beef", "횪", "tomato", "noodle", "onion", "garlic", "bean",
                                      "tender", "fish", "potato", "peanut", "shrimp","sausage",
                                      "burgers", "mahi", "bacon", "sandwich", "egg", "asada", "burrito",
                                      "taco", "chicken", "curry", "tortilla", "salsa", "thai", "carne"))

documents <- lexicalize(rest_text)
set.seed(1234)
fit <- lda.collapsed.gibbs.sampler(documents = documents$documents, K=5, vocab=documents$vocab,
                                   num.iterations = 20, alpha=0.02, eta=0.02, initial=NULL,
                                   burnin = 0, compute.log.likelihood = TRUE)

top.topic.words(fit$topics, 40, by.score=T)

top.topic.documents(fit$document_sums, 4)


##전처리 3차
rest_text <- removeWords(rest_text, c("ramen", "crab", "lobster", "really", "mushroom", "dumpling",
                                      "just", "donut", "get", "one", "wing", "deli", "never", "actually",
                                      "completely", "like", "great", "mexican", "margarita", "",
                                      "us", "tr챔s", "챕tait", "boba", "cheesecake", "highly", "bagel"))
rest_text <- pblapply(rest_text, blank.removal)

documents <- lexicalize(rest_text)
set.seed(1234)
fit <- lda.collapsed.gibbs.sampler(documents = documents$documents, K=5, vocab=documents$vocab,
                                   num.iterations = 20, alpha=0.02, eta=0.02, initial=NULL,
                                   burnin = 0, compute.log.likelihood = TRUE)
top.topic.words(fit$topics, 40, by.score=T)

top.topic.documents(fit$document_sums, 4)


##4차
rest_text <- unlist(rest_text)
rest_text <- removeWords(rest_text, c("tuna", "tofu", "dim", "sashimi", "chinese", "waffle", "much", "use",
                                      "perfectly", "buffet", "love", "scallop", "korean", "definitely", "amaze",
                                      "calamari", "rib", "ever", "lamb", "greek", "salmon", "take", "even", "review",
                                      "know", "another", "horrible", "away", "can", "star", "hash", "tao", "coconut",
                                      "chunk", "seem", "thing", "cannoli", "nice", "happy", "amaze", "gras", "foie",
                                      "perfect", "patio", "truffle", "lemon"))

rest_text <- pblapply(rest_text, blank.removal)
documents <- lexicalize(rest_text)
set.seed(1234)
fit <- lda.collapsed.gibbs.sampler(documents = documents$documents, K=5, vocab=documents$vocab,
                                   num.iterations = 20, alpha=0.02, eta=0.02, initial=NULL,
                                   burnin = 0, compute.log.likelihood = TRUE)

top.topic.words(fit$topics, 40, by.score=T)

top.topic.documents(fit$document_sums, 4)


##5차..
rest_text <- unlist(rest_text)
rest_text <- removeWords(rest_text, c("pretty", "italian", "cool", "soy", "asian", "person", "part", "puotine",
                                      "bun", "doughnut", "every", "vermicelli", "go", "say", "make", "okay",
                                      "see", "now", "back", "naan", "two", "people", "seriously", "someone",
                                      "cookie", "cinnamon", "gelato", "mozzarella", "bakery", "look"))

rest_text <- pblapply(rest_text, blank.removal)
rest_documents <- lexicalize(rest_text)
set.seed(1234)
fit <- lda.collapsed.gibbs.sampler(documents = rest_documents$documents, K=5, vocab=documents$vocab,
                                   num.iterations = 20, alpha=0.02, eta=0.02, initial=NULL,
                                   burnin = 0, compute.log.likelihood = TRUE)

top.topic.words(fit$topics, 40, by.score=T)

top.topic.documents(fit$document_sums, 4)


##6차(0307)

rest_text <- unlist(rest_text)
rest_text <- removeWords(rest_text, c("bellagio", "grit", "pretzel", "pudding", "olive", "spaghetti", "meatball",
                                      "pie", "marinara", "ravioli", "bass", "gnocchi", "bbq", "crepe", "gyoza","mango",
                                      "pineapple", "crepes", "ahi", "uuu", "way", "give"))
rest_text <- removeWords(rest_text, c("great", "cookie", "good", "plenty", "cinnamon", "meat", "yogurt", "wine","tea",
                                      "sometimes", "everything", "etc", "beer", "custard", "still", "snack", "doughnut",
                                      "matcha", "us", "juice", "shrimp", "butter", "caramel", "jelly"))

rest_text <- pblapply(rest_text, blank.removal)
rest_documents <- lexicalize(rest_text)
set.seed(1234)
fit <- lda.collapsed.gibbs.sampler(documents = rest_documents$documents, K=5, vocab=rest_documents$vocab,
                                   num.iterations = 20, alpha=0.02, eta=0.02, initial=NULL,
                                   burnin = 0, compute.log.likelihood = TRUE)

top.topic.words(fit$topics, 50, by.score=T)

top.topic.documents(fit$document_sums, 4)