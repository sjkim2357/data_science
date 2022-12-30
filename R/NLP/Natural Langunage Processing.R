# Natural Language Processing

# Importing the dataset
setwd("C:/Users/sungj/Documents/Datasets")
dataset = read.delim('Restaurant_Reviews.tsv', 
                     header = TRUE, 
                     stringsAsFactors = FALSE, #it won't identify text as factors
                     sep = '\t', 
                     quote = '') #ignore any kind of quote in the text


dataset
View(dataset)

# Cleaning the text
## we do not want to have too many columns because it makes problems in dimesionality.
## we should reduce the maximum number of columns in making the sparse matrix
# install.packages('tm')
library(tm)

### building corpus
#### corpus is a data structure for text data.
#### it is used to do statistical analysis and hypothesis testing, and checking occurrences.
corpus = VCorpus(VectorSource(dataset$Review)) #except the 'Liked'
corpus
corpus[[1]]
as.character(corpus[[1]]) #access the written review #1
View(corpus) #we don't want this.

#### we want to clean the text in the corpus 
##### lower-casing
corpus = tm_map(corpus, content_transformer(tolower)) #lower-casing
as.character(corpus[[1]])

##### remove numbers
as.character(corpus[[841]])
corpus = tm_map(corpus, removeNumbers) #remove numbers for the efficient sparse matrix
as.character(corpus[[841]])

##### remove Punctuation
as.character(corpus[[1]])
corpus = tm_map(corpus, removePunctuation)
as.character(corpus[[1]]) #three little dots disappeard.

##### remove stopwords
# install.packages('SnowballC')
library(SnowballC)
as.character(corpus[[1]])
corpus = tm_map(corpus, removeWords, stopwords())
as.character(corpus[[1]]) #removed non-relevant words 'this'.

##### stemming: getting  the root of each word
###### e.g., loved, will love, loves -> love
corpus = tm_map(corpus, stemDocument)
as.character(corpus[[1]])

##### remove extra spaces: this should be done after removing numbers, punctuation, and stopwords
corpus = tm_map(corpus, stripWhitespace)

# Building a sparse matrix of features: the Bag of Words model(dtm)
## what is the dependent variable? Likes 0 or 1
## what is the independent variable? the sparse matrix - the bag of words
dtm = DocumentTermMatrix(corpus)
dtm 
### terms: 1577 
### i.e., 1000 rows and 1577 columns: a big table
### Sparsity is 100% because we have too many 0s in the table

#### now we want to filter non-frequent words to tackle too high sparsity
dtm1 = removeSparseTerms(dtm, 0.999) 
##### we want to keep 99.9% of the most frequent terms in this matrix

dtm1 #terms: 691
##### by doing so, we've removed almost 1000 words 

# Building a classification model using Random Forest
View(dtm1) #document-term matrix is not a matrix we know. We should coerce it. 
dataset = as.data.frame(as.matrix(dtm1))
View(dataset) #this is still incomplete due to the lack of DV

## now we need to add our dependent variable to the dataset
dataset_original = read.delim('Restaurant_Reviews.tsv', header = TRUE, stringsAsFactors = FALSE, sep = '\t', quote = '')
dataset_original
View(dataset_original) #we're going to pick the last column
dataset$Liked = dataset_original$Liked
dataset
View(dataset)

# Classification using Random Forest
# Encoding the target feature as factor
dataset$Liked = factor(dataset$Liked, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Liked, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling: we only have zeros and ones, so we don't need this
# training_set[-3] = scale(training_set[-3])
# test_set[-3] = scale(test_set[-3])

# Fitting Random Forest classifier to the Training set
require(randomForest)
classifier = randomForest(x = training_set[-692], #Xs are all columns except 692
                          y = training_set$Liked, #y is the Liked column
                          ntree = 10)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-692])
y_pred
View(test_set)

# Making the Confusion Matrix
cm = table(test_set[, 692], y_pred)
cm
cm.result = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
cm.result


###############text visualization############
dtm #remember: dtm is not a data.frame
findFreqTerms(dtm)

# Count the frequency of each word
# Use col_sums because it is a document-term matrix(DTM). 
# If it is a term-document matrix(TDM) use row_sums
frequency <- slam::col_sums(dtm)

# sort the top hundred words
sort.word.freq <- sort(frequency,decreasing=TRUE)
sort.word.freq[1:100]

# wordcloud  
library('wordcloud')
wordcloud(names(frequency),freq=frequency,scale=c(4,0.2),
          rot.per=0.0,min.freq=5,random.order=FALSE)

library('RColorBrewer') 
pal <- brewer.pal(4, "Dark2")
wordcloud(names(frequency),freq=frequency,scale=c(4,0.2),
          rot.per=0.0,min.freq=5,random.order=FALSE,col=pal)

# Find words that occur more. 
frequency <- frequency[frequency >= 20]
frequency

# turn into data.frame if needed:
frequency_df <- data.frame(words = names(frequency), freq = frequency , row.names = NULL)
View(frequency_df)

# Visualize with ggplot
library(ggplot2)
ggplot(frequency_df, aes(x = words, y = freq, fill = words)) + geom_col() 


#######################################################################
###########################QUIZ(additional)############################
#######################################################################

# words in Liked Vs words in Not-liked
library(tidyverse)
# Importing the dataset
dataset = read.delim('Restaurant_Reviews.tsv', 
                     header = TRUE, 
                     stringsAsFactors = FALSE, #it won't identify text as factors
                     sep = '\t', 
                     quote = '') #ignore any kind of quote in the text

# Spliting into Liked and Not-Liked
View(dataset)

## Liked == 1
Liked.dataset <- dataset %>% filter(Liked == 1)
View(Liked.dataset)

## Liked == 0
Not.Liked.dataset <- dataset %>% filter(Liked == 0)
Not.Liked.dataset
View(Not.Liked.dataset)

########## Liked == 1
# Cleaning the text
library(tm)

##### building corpus: we want to clean the text in the corpus
corpus = VCorpus(VectorSource(Liked.dataset$Review))

##### lower-casing
corpus = tm_map(corpus, content_transformer(tolower)) #lower-casing

##### remove numbers
corpus = tm_map(corpus, removeNumbers) #remove numbers for the efficient sparse matrix

##### remove Punctuation
corpus = tm_map(corpus, removePunctuation)

##### remove stopwords
# install.packages('SnowballC')
library(SnowballC)
corpus = tm_map(corpus, removeWords, stopwords())

##### stemming: getting  the root of each word
corpus = tm_map(corpus, stemDocument)

##### remove extra spaces
corpus = tm_map(corpus, stripWhitespace)

##### building a dtm
dtm = DocumentTermMatrix(corpus)
dtm 

dtm #remember: dtm is not a data.frame
findFreqTerms(dtm)

# using col_sums since it is a document term matrix. If it is a term document matrix use row_sums
frequency <- slam::col_sums(dtm)

# sort the top hundred words
sort.word.freq <- sort(frequency,decreasing=TRUE)
sort.word.freq[1:100]

#wordcloud  
library('wordcloud')
wordcloud(names(frequency),freq=frequency,scale=c(4,0.2),
          rot.per=0.0,min.freq=5,random.order=FALSE)

library('RColorBrewer') 
pal <- brewer.pal(4, "Dark2")
wordcloud(names(frequency),freq=frequency,scale=c(4,0.2),
          rot.per=0.0,min.freq=5,random.order=FALSE,col=pal)

# Filtering like findFreqTerms. Find words that occur more. 
frequency <- frequency[frequency >= 20]
frequency

# turn into data.frame if needed:
frequency_df <- data.frame(words = names(frequency), freq = frequency , row.names = NULL)
View(frequency_df)

#  Visualize with ggplot
library(ggplot2)
ggplot(frequency_df, aes(x = words, y = freq, fill = words)) + geom_col() 


########## Liked == 0
# Importing the dataset
dataset = read.delim('Restaurant_Reviews.tsv', 
                     header = TRUE, 
                     stringsAsFactors = FALSE, #it won't identify text as factors
                     sep = '\t', 
                     quote = '') #ignore any kind of quote in the text

# Spliting into Liked and Not-Liked
View(dataset)

## Liked == 0
Not.Liked.dataset <- dataset %>% filter(Liked == 0)
Not.Liked.dataset
View(Not.Liked.dataset)

# Cleaning the text
library(tm)

##### building corpus: we want to clean the text in the corpus
corpus = VCorpus(VectorSource(Not.Liked.dataset$Review))

##### lower-casing
corpus = tm_map(corpus, content_transformer(tolower)) #lower-casing

##### remove numbers
corpus = tm_map(corpus, removeNumbers) #remove numbers for the efficient sparse matrix

##### remove Punctuation
corpus = tm_map(corpus, removePunctuation)

##### remove stopwords
# install.packages('SnowballC')
library(SnowballC)
corpus = tm_map(corpus, removeWords, stopwords())

##### stemming: getting  the root of each word
corpus = tm_map(corpus, stemDocument)

##### remove extra spaces
corpus = tm_map(corpus, stripWhitespace)

##### building a dtm
dtm = DocumentTermMatrix(corpus)
dtm 

dtm #remember: dtm is not a data.frame
findFreqTerms(dtm)

# using col_sums since it is a document term matrix. If it is a term document matrix use row_sums
frequency <- slam::col_sums(dtm)

# sort the top hundred words
sort.word.freq <- sort(frequency,decreasing=TRUE)
sort.word.freq[1:100]

#wordcloud  
library('wordcloud')
wordcloud(names(frequency),freq=frequency,scale=c(4,0.2),
          rot.per=0.0,min.freq=5,random.order=FALSE)

library('RColorBrewer') 
pal <- brewer.pal(4, "Dark2")
wordcloud(names(frequency),freq=frequency,scale=c(4,0.2),
          rot.per=0.0,min.freq=5,random.order=FALSE,col=pal)

# Filtering like findFreqTerms. Find words that occur more. 
frequency <- frequency[frequency >= 20]
frequency

# turn into data.frame if needed:
frequency_df <- data.frame(words = names(frequency), freq = frequency , row.names = NULL)
View(frequency_df)

#  Visualize with ggplot
library(ggplot2)
ggplot(frequency_df, aes(x = words, y = freq, fill = words)) + geom_col()

