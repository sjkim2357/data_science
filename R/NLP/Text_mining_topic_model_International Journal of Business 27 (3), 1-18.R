#LDA Topic model#
rm(list=ls())

library(NLP)
library(openNLP)
library(tm)
library(stringr)
library(caret)
library(e1071)
library(quanteda)
library(irlba)
library(randomForest)
library(ggplot2)
library(RWeka)
library(wordcloud)
library(data.table)
library(dplyr)
library(rJava)
library(SnowballC)

setwd("C:/Users/Sung Kim/Desktop/data mining")

mydata2<-read.csv(file.choose(), stringsAsFactors = FALSE)
corpus <- Corpus(VectorSource(mydata2$Contents))

###data preprocessing
mycorpus <- tm_map(corpus, tolower)
mycorpus <- tm_map(mycorpus, removePunctuation)
mycorpus <- tm_map(mycorpus, stripWhitespace)
mycorpus <- tm_map(mycorpus, stemDocument, language="en")
mycorpus <- tm_map(mycorpus, stripWhitespace)
mycorpus<-tm_map(mycorpus, removeWords, words=stopwords("SMART"))
mystopwords <- c("seoul","aircraft", "korea","korean","south","military","fighter",
                 "jet","defense","ministry","minister","official","project","defence","billion",
                 "air","ministri","offici")
mycorpus <- tm_map(mycorpus, removeWords, mystopwords)
mycorpus <- tm_map(mycorpus, stripWhitespace)
#mycorpus <- tm_map(mycorpus, removeNumbers)

#count 
sapply(str_split(mycorpus, " "),length)

#stemming 
mycorpus <- tm_map(mycorpus, stemDocument, language="en")
mycorpus <- tm_map(mycorpus, stripWhitespace)
as.character(mycorpus[[1]])


#see number/digit data 
mydigits <- lapply(mycorpus, function(x) (str_extract_all(x, "[[:digit:]]{1,}")))
table(unlist(mydigits))

#see punct
mypuncts <- lapply(mycorpus, function(x) (str_extract_all(x, "\\b[[:alpha:]]{1,}[[:punct:]]{1,}
[[:alpha:]]{1,}\\b")))
table(unlist(mypuncts))

#ngram process
for(i in seq_along(mycorpus)){
  mycorpus[[i]]$content <- gsub("boe", "boeing", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("air force", "airforce", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("north korea", "northkorea", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("united states", "unitedstates", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("st loui", "stlouis", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("stlouissbuilt", "stlouis", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("fighter jet\\b(s)", "fighter", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("jet fighter\\b(s)", "fighter", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("jet", "fighter", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("fighter\\b(s)", "fighter", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("jetfighter\\b(s)", "fighter", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("jet fighter\\b(s)", "fighter", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("war plane\\b(s)", "fighter", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("thoma schwartz", "thomasschwartz", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("army gen", "thomasschwartz", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("b[[:alpha:]]ryan\\[[:alpha:]]b", "georgeryan", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("ryan", "georgeryan", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("culloton", "georgeryan", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("sen christopher kit", "kitbond", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("sen kitbond", "kitbond", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("bond", "kitbond", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("sen kit bond", "kitbond", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("christoph kit bond", "kitbond", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("christoph kit bond rmo", "kitbond", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("kit bond rmo", "kitbond", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("vice president", "vicepresident", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("air force", "airforce", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("korean air force", "koreanairforce", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("us air force", "usairforce", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("lockh\\b[[:alpha:]]", "lockheedmartin", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("defens ministry", "defenseministry", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("joint strike fighter", "jointstrikefighter", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("strike fighter", "jointstrikefighter", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("joint strike", "jointstrikefighter", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("defens ministri", "defenseministry", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("kim dongshin", "koreanminister", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("kim dong shin", "koreanminister", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("cha young koo", "koreandeputyminister", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("cha youngkoo", "koreandeputyminister", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("defens minist kim", "koreanminister", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("dassault aviation", "dassault", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("dassault aviation france", "dassault", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("militari aircraft missile", "aircraftmissle", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("f15k fighter jet", "f15k", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("f15 eagl", "f15", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("keep line open", "productionline", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("south korean president", "koreanpresident", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("kim dae jung", "koreanpresident", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("presid kim", "koreanpresident", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("kim daejung", "koreanpresident", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("daejung", "koreanpresident", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("richard gephardt", "richardgephardt", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("militans secretari", "militansesecretary", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("donald rumsfeld", "donaldrumsfeld", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("rumsfeld", "donaldrumsfeld", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("yves robin", "yvesrobin", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("colin powel", "colinpowell", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("powel", "colinpowell", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("euidon", "mndspokesman", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("jongki", "mnddirectorgeneral", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("inter relat", "internationalrelation", mycorpus[[i]]$content);
  mycorpus[[i]]$content <- gsub("dongjin", "deputyministerforacquisition", mycorpus[[i]]$content);
}

mycorpus <- tm_map(mycorpus, stripWhitespace)
as.character(mycorpus[[5]])
as.character(mycorpus[[1]])
as.character(mycorpus[[8]])

#count
sapply(str_split(mycorpus, " "),length)

#Build DTM 
dtm.e <- DocumentTermMatrix(mycorpus) 
dtm.e

#See matrix
rownames(dtm.e[,])
colnames(dtm.e[,])

#See partial matrix 
inspect(dtm.e[1:3,5:33])

#Build TF-IDF 
dtm.e.tfidf <- DocumentTermMatrix(mycorpus,
                                  control=list(weighting=function(x) weightTfIdf(x, normalize=FALSE))) 
dtm.e.tfidf

#Investigate TF-IDF  
value.tf.dtm <- as.vector(as.matrix(dtm.e[,]))
value.tfidf.dtm <- as.vector(as.matrix(dtm.e.tfidf[,])) 
word.label.dtm <- rep(colnames(dtm.e[,]),each=dim(dtm.e[,])[1])
doc.label.dtm <- rep(rownames(dtm.e[,]),dim(dtm.e[,])[2])
mydata <- data.frame(word.label.dtm,doc.label.dtm,value.tf.dtm,value.tfidf.dtm)
colnames(mydata) <- c('word','doc','tf','tfidf')

#Print the result
mydata2 <- subset(mydata, tf>0&tfidf>0)
mydata3 <- subset(mydata2, tf>median(mydata2$tf) & tfidf<median(mydata2$tfidf))
table(mydata3$word)[table(mydata3$word)>0]

#dtm.e object  
word.freq <- apply(dtm.e[,],2,sum) 
head(word.freq)
length(word.freq)

#sorting 
sort.word.freq <- sort(word.freq,decreasing=TRUE)
sort.word.freq[1:100]

#cumulative frequency 
cumsum.word.freq <- cumsum(sort.word.freq)
cumsum.word.freq[1:20]
prop.word.freq <- cumsum.word.freq/cumsum.word.freq[length(cumsum.word.freq)]
prop.word.freq[1:20]

#word frequency analysis  
plot(1:length(word.freq),prop.word.freq,type='l',
     xlab='Order of word frequency',ylab='Cumulative proportion',
     main="",axes=FALSE)
axis(1,at=round(446.1*(0:10)),labels=paste(10*(0:10),"%",sep=""))
axis(2,at=0.20*(0:5),labels=paste(20*(0:5),"%",sep=""))
for (i in 1:9) {
  text(44.61*10*i,0.05+prop.word.freq[44.61*10*i],
       labels=paste(round(100*prop.word.freq[44.61*10*i]),"%",sep=""))
  points(44.61*10*i,prop.word.freq[44.61*10*i],pch=9)
}

#wordcloud  
library('wordcloud')
wordcloud(names(word.freq),freq=word.freq,scale=c(4,0.2),
          rot.per=0.0,min.freq=5,random.order=FALSE)
library('RColorBrewer') 
pal <- brewer.pal(4, "Dark2")
wordcloud(names(word.freq),freq=word.freq,scale=c(4,0.2),
          rot.per=0.0,min.freq=5,random.order=FALSE,col=pal)

#findAssocs function
findAssocs(dtm.e,"polit",0.10)
findAssocs(dtm.e,"technolog",0.50)
findAssocs(dtm.e,"rafael",0.50)
findAssocs(dtm.e,"interoperab",0.50)
findAssocs(dtm.e,"russia",0.50)
findAssocs(dtm.e,"kitbond",0.50)
findAssocs(dtm.e,"koreanpresident",0.50)
findAssocs(dtm.e,"koreanminister",0.50)
findAssocs(dtm.e,"ministri",0.50)
findAssocs(dtm.e,"offici",0.50)
findAssocs(dtm.e,"competit",0.50)
findAssocs(dtm.e,"decid",0.50)
findAssocs(dtm.e,"powel",0.50)
findAssocs(dtm.e,"seoul",0.50)
findAssocs(dtm.e,"forc",0.50)
findAssocs(dtm.e,"interop",0.50)
findAssocs(dtm.e,"dassault",0.50)
findAssocs(dtm.e,"french",0.50)
findAssocs(dtm.e,"franc",0.50)
findAssocs(dtm.e,"line",0.50)
findAssocs(dtm.e,"russian",0.50)
findAssocs(dtm.e,"sukhoi",0.50)
findAssocs(dtm.e,"offer",0.50)
findAssocs(dtm.e,"stlouis",0.50)
findAssocs(dtm.e,"rafal",0.50)
findAssocs(dtm.e,"georgeryan",0.50)
findAssocs(dtm.e,"cash",0.50)
findAssocs(dtm.e,"interop",0.30)
findAssocs(dtm.e,"usfk",0.50)
findAssocs(dtm.e,"debt",0.10)
findAssocs(dtm.e,"eurofight",0.50)

#findAssocs between specific words
var1 <- as.vector(dtm.e[,"debt"])
var2 <- as.vector(dtm.e[,"russia"])
cor.test(var1,var2)

#the above function application
my.assoc.func <- function(dtm.e,term1,term2){
  myvar1 <- as.vector(dtm.e[,term1])
  myvar2 <- as.vector(dtm.e[,term2])
  cor.test(myvar1,myvar2)
}
my.assoc.func(dtm.e,"interop","usfk")


#Corrlation between documents
my.assoc.func(t(dtm.e), "125", "126")
my.assoc.func(t(dtm.e), "125", "126")

#Matrix of corrlation between documents
length.doc <- length(rownames(dtm.e))
my.doc.cor <- matrix(NA,nrow=length.doc,ncol=length.doc)
for (i in 1:length.doc) {
  for (j in 1:length.doc) {
    my.doc.cor[i,j] <- my.assoc.func(t(dtm.e),rownames(dtm.e)[i],rownames(dtm.e)[j])$est
  }
}
rownames(my.doc.cor) <- colnames(my.doc.cor) <- rownames(dtm.e)
#See how it looks like
round(my.doc.cor[121:130,121:130],2)

#See how it looks like
round(my.doc.cor[,125],2)
#doc#125 is most similar to doc#130(0.92)
round(my.doc.cor[,2],2)
#doc#2 is most similar to doc#55(0.52)

#Build histogram
hist(my.doc.cor[lower.tri(my.doc.cor)],breaks=30,
     col='lightblue',xlim=c(-0.1,0.8),xlab="correlations",
     main="Correlations between Korean news articles")
summary(my.doc.cor[lower.tri(my.doc.cor)])

####Euclidian distance######
dist.dtm.e <- dist(dtm.e)
as.matrix(dist.dtm.e)[121:130,121:130]

#Clustering analysis using Ward's method
myclusters <- hclust(dist.dtm.e,method="ward.D2")
#plot it
plot(myclusters)
mydata5<-read.csv(file.choose(), stringsAsFactors = FALSE)
corpus1 <- Corpus(VectorSource(mydata5$Contents))
#Convert our class label into a factor.
Date <- as.factor(mydata5$Date)
myclusters$labels <- str_extract(mydata5$Date, "[[:digit:]]{1}")
#plot it
plot(myclusters)
myclusters$labels <- str_extract(mydata5$Date, "[[:digit:]]{4}")
#plot it
plot(myclusters)
#we can input the number of clusters
mygroup <- cutree(myclusters,k=10)
mygroup
table(mygroup)

#Color the clusters
library('dendextend')
dend <- as.dendrogram(myclusters)
#input the number of clusters
myk <- 9
#Color allocation
dend <- dend %>%
  color_branches(k = myk) %>%
  color_labels(dend, k=myk) %>%
  set("branches_lwd", 2) %>%
  set("branches_lty", 1)
plot(dend,main="Clustering documents",ylab="Height",
     ylim=c(0,30))

#See the frequency of each cluster
mytable <- table(str_extract(myclusters$labels,"[[:digit:]]*"),
                 +                  cutree(myclusters,k=9))
mytable

#ggplot
library('ggplot2')
cluster.by.year <- data.frame(mytable)
cluster.by.year
colnames(cluster.by.year) <- c('year','cluster','freq.')
cluster.by.year$cluster <- paste('cluster',cluster.by.year$cluster,sep='')
cluster.by.year$year <- as.numeric(as.character(cluster.by.year$year))
#See how it transform over time
ggplot(data=cluster.by.year, aes(x=year, y=freq., fill=cluster)) + 
  geom_bar(stat="identity")+
  scale_x_continuous(breaks=1996:2002,labels=1996:2002)+
  scale_fill_manual(values=1:9,labels=paste('cluster',1:9,sep=''))+
  labs(x="year",y="freq.",fill='cluster')

#Correlated Topic Model#
###number of topics###
library(ldatuning)
FindTopicsNumber(dtm.e, topics = 2:10, metrics = "Arun2010", mc.cores = 1L)
system.time(
  topic_number_lemma <- FindTopicsNumber(
    dtm.e,
    topics = c(seq(from = 2, to = 9, by = 1), seq(10, 20, 2), seq(25, 50, 5)),
    metrics = c( "Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
    method = "Gibbs",
    mc.cores = 2L,
    verbose = TRUE
  )
)
###topic model###
#control=list(seed=11) 
library('topicmodels')
lda.out <- LDA(dtm.e,control=list(seed=12345),k=9)
dim(lda.out@gamma)
dim(lda.out@beta)
#see the result
terms(lda.out,40)

#calculate the probability of each document 
posterior_lda <- posterior(lda.out)
round(posterior_lda$topics,3)

#build dataframe
lda.topics <- data.frame(posterior_lda$topics)
apply(lda.topics,1,sum)
apply(lda.topics,2,sum)

#see how it change over time
tempyear <- rownames(lda.topics)
pubyear <- as.numeric(unlist(str_extract_all(mydata5$Date,"[[:digit:]]{4}")))
topic.by.year <- round(aggregate(as.matrix(lda.topics)~pubyear,lda.topics,sum),5)
topic.by.year

##ggplot
library('ggplot2')
topic.by.year <- reshape(topic.by.year, idvar = "pubyear", varying = list(2:10),
                         v.names = "X", direction = "long")
colnames(topic.by.year) <- c('year','topic_i','score')
topic.by.year$topic_i[topic.by.year$topic_i==1] <- '1.Business scandal'
topic.by.year$topic_i[topic.by.year$topic_i==2] <- '2.Technology transfer'
topic.by.year$topic_i[topic.by.year$topic_i==3] <- '3.Political situation awareness'
topic.by.year$topic_i[topic.by.year$topic_i==4] <- '4.Bidder attraction'
topic.by.year$topic_i[topic.by.year$topic_i==5] <- '5.Competition status'
topic.by.year$topic_i[topic.by.year$topic_i==6] <- '6.ROAKF modernization'
topic.by.year$topic_i[topic.by.year$topic_i==7] <- '7.Political pressure'
topic.by.year$topic_i[topic.by.year$topic_i==8] <- '8.Interest on suppliers'
topic.by.year$topic_i[topic.by.year$topic_i==9] <- '9.Russian issues'
head(topic.by.year)

ggplot(data=topic.by.year, aes(x=year, y=score, fill=topic_i)) + 
  geom_bar(stat="identity")+
  scale_x_continuous(breaks=1996:2002,labels=1996:2002)+
  scale_y_continuous(breaks=2*(0:9),labels=2*(0:9))+
  scale_fill_manual(values=c("black","red","orange","blue","green","violet","yellow","purple","yellowgreen"))+
  labs(x="publication year",y="score",fill='latent topic')+
  ggtitle("LDA: Korea media corpus")

###number of topics by findtopicsnumber###
library(ldatuning)
FindTopicsNumber(dtm.e, topics = 2:10, metrics = "Arun2010", mc.cores = 1L)

system.time(
  topic_number_lemma <- FindTopicsNumber(
    dtm.e,
    topics = c(seq(from = 2, to = 9, by = 1), seq(10, 20, 2), seq(25, 50, 5)),
    metrics = c( "Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
    method = "Gibbs",
    mc.cores = 2L,
    verbose = TRUE
  )
)

###CTM out###
library('topicmodels')
system.time(ctm.out <- CTM(dtm.e,control=list(seed=10012),k=20))
dim(ctm.out@gamma)
dim(ctm.out@beta)
terms(ctm.out,40)


###topic matrix###
post <- topicmodels::posterior(ctm.out)
cor_mat <- cor(t(post[["terms"]]))
cor_mat


###network of the word distributions over topics###
cor_mat[cor_mat < .05 ] <- 0
diag(cor_mat) <- 0
library(igraph)
graph <- graph.adjacency(cor_mat, weighted=TRUE, mode="lower")
graph <- delete.edges(graph, E(graph)[weight < 0.05])
E(graph)$edge.width <- E(graph)$weight*2
V(graph)$label <- paste("Topic", V(graph))
V(graph)$size <- colSums(post[["topics"]]) * 1
par(mar=c(0, 0, 3, 0))
set.seed(110)
plot.igraph(graph, edge.width = E(graph)$edge.width, 
            edge.color = "orange", vertex.color = "orange", 
            vertex.frame.color = NA, vertex.label.color = "grey30")
title("Strength Between Topics Based On Word Probabilities", cex.main=.8)


#statistical testing#
###Data preprocessing
par(mfrow=c(3,2))
qqnorm(mydata[,1], main="Development tech-transefer")
qqline(mydata[,1])
qqnorm(mydata[,2], main="Production tech-transfer")
qqline(mydata[,2])
qqnorm(mydata[,3], main="Education & training")
qqline(mydata[,3])
qqnorm(mydata[,4], main="Organization doctrine & tactics")
qqline(mydata[,4])
qqnorm(mydata[,5], main="Recipient's R&D plan fitness")
qqline(mydata[,5])
qqnorm(mydata[,6], main="Engineers and scientists fitness")
qqline(mydata[,6])
attach(mydata)
mydata$techop<-t1+t2
mydata$knowledge<-t3+t4
mydata$absorcapa<-t5+t6
View(mydata)

###MANOVA
F1 <- factor(mydata[, 7])
X <- as.matrix(mydata[, 8:10])
fit <- manova(X~F1)
summary.manova(fit, test="Wilks")

##ANOVA
mydatafactor<-factor(mydata[,7])
mydatamatrix<-as.matrix(mydata[,8:10])
resul<-aov(mydatamatrix~mydatafactor)
summary(resul)

###Bonferroni
y<-mydata[,7]
y
y<-as.matrix(y)
z<-as.matrix(mydata[,8:10])
z
reg73<-lm(y~z)
summary(reg73)
plot(reg73)
attach(mydata)
library(dunn.test)
dunn.test(`t1`, g = status, method = "bonferroni",  kw=TRUE, label=TRUE, 
          wrap=FALSE, table=TRUE, list=FALSE, rmc=FALSE, alpha=0.05, altp=FALSE)
dunn.test(`t6`, g = status, method = "bonferroni",  kw=TRUE, label=TRUE, 
          wrap=FALSE, table=TRUE, list=FALSE, rmc=FALSE, alpha=0.05, altp=FALSE)
