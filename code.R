library(tm)
library(ggplot2)
library(wordcloud)
library(Rgraphviz)
library(plyr)

#build corpus
mypwd = file.path('~','Desktop','Data')
dir(mypwd)
docs = Corpus(DirSource(mypwd))


#preprocess data 
docs <- tm_map(docs, content_transformer(tolower))
docs = tm_map(docs, removeNumbers)
docs = tm_map(docs, removePunctuation)
docs = tm_map(docs, removeWords, stopwords("english"))
docs = tm_map(docs, removeWords,c('said','one','also','will','just','for','but','include','may','can','like','the'))
docs = tm_map(docs,stemDocument)
docs = tm_map(docs,stripWhitespace)
docs = tm_map(docs,PlainTextDocument)

dtm = DocumentTermMatrix(docs)

mfreq = colSums(as.matrix(dtm))

#wordcloud
length(mfreq)
mord = order(mfreq, decreasing=TRUE)
high=mfreq[head(mord,50)]

set.seed(100) # if you'd like to make the configuration of the layout consistent each time
wordcloud(names,high,
          colors=brewer.pal(8, "Dark2"),
          random.color=F, 
          random.order=F)


names=c('apple','watch','device','company','use','new','app','year','user','iphone','first',
        'market','service','mobile','information','time','technology','data','product','make','system','provide',
        'include','store','report','pay','comput','phone','miliion','inc','music','patent','work',
        'custom','last','say','now','business','share','sale','google','operation','electronical','feature',
        'people','develope','get','payment','display','billion')



# frequency of news

example <- read.csv("~/Downloads/example.csv",header=F)
t=as.Date(example[,1])
jan=as.Date('2015/01/01')
june=as.Date('2015/06/30')
hist(t,breaks='days',freq=F,xlim=c(jan, june),main='Frequency of News by day',col=heat.colors(30))
lines(density(as.numeric(t)))
hist(t,breaks='weeks',freq=F,xlim=c(jan, june),main='Frequency of News by week', col=heat.colors(4))
lines(density(as.numeric(t)))
hist(t,breaks='months',freq=F,xlim=c(jan, june),main='Frequency of News by month', col=heat.colors(6),ylim=c(0, 0.01))
lines(density(as.numeric(t)))

# by minghao
C= read.csv("~/Downloads/wsj2015.csv",header=F)
t<-as.Date(C[,1])
jan<-as.Date("2015/01/01")
june<-as.Date("2015/06/30")
hist(t, breaks="days", freq=F, xlim=c(jan, june), col=heat.colors(30))
lines(density(as.numeric(t)))
hist(t, breaks="weeks", freq=F, xlim=c(jan, june), col=heat.colors(4))
lines(density(as.numeric(t)))
hist(t, breaks="months", freq=F, xlim=c(jan, june), ylim=c(0, 0.01), col=heat.colors(6))
lines(density(as.numeric(t)))



#sensitive analysis
#load up word polarity list and format it
afinn_list <- read.delim(file='~/Downloads/AFINN/AFINN-111.txt', header=FALSE, stringsAsFactors=FALSE)
names(afinn_list) <- c('word', 'score')
afinn_list$word <- tolower(afinn_list$word)

#categorize words as very negative to very positive and add some movie-specific words
vNegTerms <- afinn_list$word[afinn_list$score==-5 | afinn_list$score==-4]
negTerms <- afinn_list$word[afinn_list$score==-3 | afinn_list$score==-2  ]
neutralTerms<-afinn_list$word[afinn_list$score==-1| afinn_list$score==1]
posTerms <- afinn_list$word[afinn_list$score==3 | afinn_list$score==2 ]
vPosTerms <- afinn_list$word[afinn_list$score==5 | afinn_list$score==4]

sentimentScore <- function(sentences, vNegTerms, negTerms, neutralTerms,posTerms, vPosTerms){
  final_scores <- matrix('', 0, 6)
  scores <- laply(sentences, function(sentence, vNegTerms, negTerms, neutralTerms,posTerms, vPosTerms){
    initial_sentence <- sentence
    #remove unnecessary characters and split up by word 
    sentence <- gsub('[[:punct:]]', '', sentence)
    sentence <- gsub('[[:cntrl:]]', '', sentence)
    sentence <- gsub('\\d+', '', sentence)
    sentence <- tolower(sentence)
    wordList <- strsplit(sentence, '\\s+')
    words <- unlist(wordList)
    #build vector with matches between sentence and each category
    vPosMatches <- match(words, vPosTerms)
    posMatches <- match(words, posTerms)
    neuMaches<-match(words,neutralTerms)
    vNegMatches <- match(words, vNegTerms)
    negMatches <- match(words, negTerms)
    #sum up number of words in each category
    vPosMatches <- sum(!is.na(vPosMatches))
    posMatches <- sum(!is.na(posMatches))
    neuMaches<-sum(!is.na(neuMaches))
    vNegMatches <- sum(!is.na(vNegMatches))
    negMatches <- sum(!is.na(negMatches))
    score <- c(vNegMatches, negMatches, neuMaches,posMatches, vPosMatches)
    #add row to scores table
    newrow <- c(initial_sentence, score)
    final_scores <- rbind(final_scores, newrow)
    return(final_scores)
  }, vNegTerms, negTerms, neutralTerms,posTerms, vPosTerms)
  return(scores)
}   

s=example[,6]

totalscore=matrix(ncol=6)
vNegtotal=0
Negtotal=0
Neutotal=0
Positotal=0
vPositotal=0
nvneg=0
nneg=0
nneu=0
npos=0
nvpos=0

for (i in 1: length(s))
{
score1=sentimentScore(s[i], vNegTerms, negTerms, neutralTerms,posTerms, vPosTerms)
score_matrix=as.matrix(score1)
vNegtotal=vNegtotal+as.numeric(score_matrix[2])
Negtotal=Negtotal+as.numeric(score_matrix[3])
Neutotal=Neutotal+as.numeric(score_matrix[4])
Positotal=Positotal+as.numeric(score_matrix[5])
vPositotal=vPositotal+as.numeric(score_matrix[6])
if (max(as.numeric(score_matrix[2]),as.numeric(score_matrix[3]),as.numeric(score_matrix[4]),
        as.numeric(score_matrix[5]),as.numeric(score_matrix[6]))==as.numeric(score_matrix[2]))
{nvneg=nvneg+1}
if (max(as.numeric(score_matrix[2]),as.numeric(score_matrix[3]),as.numeric(score_matrix[4]),
        as.numeric(score_matrix[5]),as.numeric(score_matrix[6]))==as.numeric(score_matrix[3]))
{nneg=nneg+1}
if (max(as.numeric(score_matrix[2]),as.numeric(score_matrix[3]),as.numeric(score_matrix[4]),
        as.numeric(score_matrix[5]),as.numeric(score_matrix[6]))==as.numeric(score_matrix[4]))
{nneu=nneu+1}
if (max(as.numeric(score_matrix[2]),as.numeric(score_matrix[3]),as.numeric(score_matrix[4]),
        as.numeric(score_matrix[5]),as.numeric(score_matrix[6]))==as.numeric(score_matrix[5]))
{npos=npos+1}
if (max(as.numeric(score_matrix[2]),as.numeric(score_matrix[3]),as.numeric(score_matrix[4]),
        as.numeric(score_matrix[5]),as.numeric(score_matrix[6]))==as.numeric(score_matrix[6]))
{nvpos=nvpos+1}

}

w=c(vNegtotal,Negtotal,Neutotal,Positotal,vPositotal)

#
#categorize words as very negative to very positive and add some movie-specific words
vNegTerms <- afinn_list$word[afinn_list$score==-5 | afinn_list$score==-4]
negTerms <- afinn_list$word[afinn_list$score==-3 | afinn_list$score==-2 |afinn_list$score==-1 ]
posTerms <- afinn_list$word[afinn_list$score==3 | afinn_list$score==2| afinn_list$score==1 ]
vPosTerms <- afinn_list$word[afinn_list$score==5 | afinn_list$score==4]

sentimentScore <- function(sentences, vNegTerms, negTerms,posTerms, vPosTerms){
  final_scores <- matrix('', 0, 5)
  scores <- laply(sentences, function(sentence, vNegTerms, negTerms,posTerms, vPosTerms){
    initial_sentence <- sentence
    #remove unnecessary characters and split up by word 
    sentence <- gsub('[[:punct:]]', '', sentence)
    sentence <- gsub('[[:cntrl:]]', '', sentence)
    sentence <- gsub('\\d+', '', sentence)
    sentence <- tolower(sentence)
    wordList <- strsplit(sentence, '\\s+')
    words <- unlist(wordList)
    #build vector with matches between sentence and each category
    vPosMatches <- match(words, vPosTerms)
    posMatches <- match(words, posTerms)
    vNegMatches <- match(words, vNegTerms)
    negMatches <- match(words, negTerms)
    #sum up number of words in each category
    vPosMatches <- sum(!is.na(vPosMatches))
    posMatches <- sum(!is.na(posMatches))
    vNegMatches <- sum(!is.na(vNegMatches))
    negMatches <- sum(!is.na(negMatches))
    score <- c(vNegMatches, negMatches, posMatches, vPosMatches)
    #add row to scores table
    newrow <- c(initial_sentence, score)
    final_scores <- rbind(final_scores, newrow)
    return(final_scores)
  }, vNegTerms, negTerms, posTerms, vPosTerms)
  return(scores)
}   

s2730=read.csv("~/Desktop/27-30.csv",header=F)
s1=s2730[,6]
s1317=read.csv("~/Desktop/13-17.csv",header=F)
s2=s1317[,6]
s0412=read.csv("~/Desktop/04-12.csv",header=F)
s3=s0412[,6]
s1821=read.csv("~/Desktop/18-21.csv",header=F)
s4=s1821[,6]
s0105=read.csv("~/Desktop/01-05.csv",header=F)
s5=s0105[,6]

select=c('s1','s2','s3','s4','s5')

totalscore=matrix(ncol=5)
vNegtotal=0
Negtotal=0
Positotal=0
vPositotal=0
nvneg=0
nneg=0
npos=0
nvpos=0

for (n in 1:5)
{
s=select[n]
for (i in 1: length(s))
{
  score1=sentimentScore(s[i], vNegTerms, negTerms,posTerms, vPosTerms)
  score_matrix=as.matrix(score1)
  vNegtotal=vNegtotal+as.numeric(score_matrix[2])
  Negtotal=Negtotal+as.numeric(score_matrix[3])
  Positotal=Positotal+as.numeric(score_matrix[4])
  vPositotal=vPositotal+as.numeric(score_matrix[5])
  if (max(as.numeric(score_matrix[2]),as.numeric(score_matrix[3]),as.numeric(score_matrix[4]),
          as.numeric(score_matrix[5]))==as.numeric(score_matrix[2]))
  {nvneg=nvneg+1}
  if (max(as.numeric(score_matrix[2]),as.numeric(score_matrix[3]),as.numeric(score_matrix[4]),
          as.numeric(score_matrix[5]))==as.numeric(score_matrix[3]))
  {nneg=nneg+1}
  if (max(as.numeric(score_matrix[2]),as.numeric(score_matrix[3]),as.numeric(score_matrix[4]),
          as.numeric(score_matrix[5]))==as.numeric(score_matrix[4]))
  {npos=npos+1}
  if (max(as.numeric(score_matrix[2]),as.numeric(score_matrix[3]),as.numeric(score_matrix[4]),
          as.numeric(score_matrix[5]))==as.numeric(score_matrix[5]))
  {nvpos=nvpos+1}
  
}

words=c(vNegtotal,Negtotal,Positotal,vPositotal)
article=c(nvneg,nneg,npos,nvpos)
cat('words',words,'\n','article',article)
}


A<-read.csv("WSJ.csv", header=F)
B<-read.csv("example (1).csv",header=F)
C<-read.csv("wsj2015.csv", header=F)

#histogram of the number of news from jan to june
t<-as.Date(C[,1])
jan<-as.Date("2015/01/01")
june<-as.Date("2015/06/30")
hist(t, breaks="days", freq=F, xlim=c(jan, june), col=heat.colors(30))
lines(density(as.numeric(t)))
#more news in the beginning of the month
hist(t, breaks="weeks", freq=F, xlim=c(jan, june), col=heat.colors(4))
lines(density(as.numeric(t)))
hist(t, breaks="months", freq=F, xlim=c(jan, june), ylim=c(0, 0.01), col=heat.colors(6))
lines(density(as.numeric(t)))
#the density curve are getting closer to the histogram


#smoothed densities of publications scatterplot
dev.off()
pub<-as.data.frame(table(C[,4]))
with(pub,smoothScatter(Var1,Freq,main="Scatterplot Colored by Smoothed Densities",
                       xlab="publications"))  

#bubble plot of main publications
mainpub<-pub[pub$Freq>270,]
index<-seq(1:nrow(mainpub))
f<-mainpub[,2]
r<-rep(0.001,nrow(mainpub))
symbols(index,f,circles=r,inches=0.30,fg="white",bg="lightblue",
        main="Bubble Plot of main publications",ylab="Frequency",xlab="index")
text(index,f,mainpub[,1],cex=0.6)  


#smoothed densities of entities scatterplot
ent<-read.csv("ent.csv", header=F)
vec<-function (x){
  t(t(as.vector(x)))
}
ent1<-read.csv("ent1.csv",header=F)
e<-vec(ent1)
etable<-as.data.frame(table(e))[-c(1),]
with(etable,smoothScatter(e,Freq,
                          main="Scatterplot Colored by Smoothed Densities", xlab="Entities without Apple Inc."))


#bubble plot of main entities
mainet<-etable[etable$Freq>400,]
index<-seq(1:nrow(mainet))
f<-mainet[,2]
r<-rep(0.001,nrow(mainet))
symbols(index,f,circles=r,inches=0.30,fg="white",bg="lightblue",
        main="Bubble Plot of main entities",ylab="Frequency",xlab="index")
text(index,f,mainet[,1],cex=0.6)  

rm(list=ls())
setwd("C:/Users/lwh/Desktop/")
DATA<-read.csv(file="example(2).csv", header=F)
readtime<-DATA$V1
require(tm)
require(Rwordseg)
require(plyr)
require(RColorBrewer)
require(lattice)
time<-as.POSIXlt(readtime,origin='1970-1-1 00:00:00',format="%Y-%m-%d")
cbind(format(time,"%Y-%m"),format(time,"%d"))->day_table
as.data.frame(day_table)->day_table
table(day_table)->day_m
heatmap(day_m,Rowv=NA,Colv=NA,scale="column",col=brewer.pal(4,"Blues"),revC=TRUE)

install.packages('lattice')
xyplot(cd4 ~ time, data = readtime,
       aspect = "xy", type = "o",
       xlab = "Time", ylab = "CD4",
       scales = list(x  = list(rot = 45, at = c(1, 2, 8, 11)),
                     y  = list(alternating = 2)
       )
)

require(quantmod)
apple<-getSymbols("AAPL",src="yahoo", from='2015-05-01', to='2015-05-30')
APPLE<-to.daily(AAPL)
tail(APPLE)
candleChart(APPLE,theme="white")

stock.data=read.csv("table.csv")
days=dim(stock.data)[1]
dates=as.Date(stock.data[,1])
stock.return=numeric(3)
riseornot=numeric()
for (i in 2:days){
  stock.return[i]=(stock.data[i,7]/stock.data[i-1,7])-1
  if (stock.return[i]>0) riseornot[i]=1 else riseornot[i]=0
}
stock=data.frame(t=dates,r=stock.return,k=riseornot)
ori=as.Date("2015-01-01")
timespan=as.numeric(max(as.Date(stock[,1])-ori))+1
Date.series=as.Date(as.Date(ori)+c(0:as.numeric(timespan)))
nn=length(Date.series)
riseor=array(3,dim=nn)
for (i in 2:days) {
  riseor[as.numeric(as.Date(stock[i,1])-ori+1)]=stock[i,2]
}
for (i in nn:2) {
  if (riseor[i]==3) riseor[i]=riseor[i+1]
}


###
temp=read.csv("artical.csv",header=F)
temp1=temp[order(as.Date(temp[,1])-ori),]
temp=temp1[(as.Date(temp1[,1])-ori>=0),]

n=dim(temp)[1]
article.dates=as.Date(temp[,1])
qq=numeric()
for(i in 1:n) qq[i]= riseor[Date.series==as.Date(as.Date(temp[i,1])+1)]
for (i in 1:15000) {
  ttt=as.character(temp[i,5])
  if (qq[i]>0)
    write.table(ttt,sprintf("up_train/%s.txt",i),quote = F,row.names=F,col.names=F) else
      write.table(ttt,sprintf("down_train/%s.txt",i),quote = F,row.names=F,col.names=F)
}
for (i in 15001:n) {
  ttt=as.character(temp[i,5])
  if (qq[i]>0)
    write.table(ttt,sprintf("up_test/%s.txt",i),quote = F,row.names=F,col.names=F) else
      write.table(ttt,sprintf("down_test/%s.txt",i),quote = F,row.names=F,col.names=F)
}




install.packages("tm",dependencies = T)
library(tm);

source("hw4.R")
preprocess.directory = function(dirname){
  ds = DirSource(dirname)
  fp = Corpus( ds )
  fp = tm_map( fp , content_transformer(tolower));
  fp = tm_map( fp , removePunctuation);
  fp = tm_map( fp , removeNumbers)
  fp = tm_map( fp , removeWords, stopwords("english"));
  fp = tm_map( fp , stemDocument)
  fp = tm_map( fp , stripWhitespace)		
  writeCorpus( fp , path=sprintf('%s/_clean',dirname) )
}

preprocess.directory('up_train/')
preprocess.directory('up_test/')
preprocess.directory('down_test/')
preprocess.directory('down_train/')
#This is wrong, but later I copied the files in these four folders into one folder

up.test=c(20001:24169)[qq[20001:24169]>0]
down.test=c(20001:24169)[qq[20001:24169]<0]
up.train=c(1:20000)[qq[1:20000]<0]
down.train=c(1:20000)[qq[1:20000]<0]

#biglist=append(up.test,down.test)
#biglist=append(biglist,down.train)
#biglist=append(biglist,up.train)
#biglist=read.directory("all")
#dictionary=make.sorted.dictionary.df(biglist)
#garbarge=as.character(dictionary[dictionary[2]<=1,1])

fildir="all"
docs=Corpus(DirSource((fildir)))
#docs2=docs
#for (i in seq(1000,26000,1000)) {
#  tttr=as.character(garbarge[i-999:i])
#  docs2=tm_map(docs2, removeWords,tttr)
#}


dtm=DocumentTermMatrix(docs)

length()make.log.pvec <- function(dtm,mu){
  # Sum up the number of instances per word
  
  n=length(dtm$v)
  summ.up=array(0,dim=dtm$ncol)
  summ.down=array(0,dim=dtm$ncol)
  train=(dtm$i<=20000)
  up=(qq[dtm$i]>0)
  for (ii in 1:n) {
    summ.up[dtm$j[ii]]=summ.up[dtm$j[ii]]+dtm$v[ii]*train[ii]*up[ii]
    summ.down[dtm$j[ii]]=summ.down[dtm$j[ii]]+dtm$v[ii]*train[ii]*(1-up[ii])
  } 
  n.words.up <- sum(summ.up)
  n.words.down <- sum(summ.down)
  # Get dictionary size
  dic.len.up <- length(summ.up)
  dic.len.down <- length(summ.down)
  # Incorporate mu and normalize
  log.pvec.up <- log(summ.up + mu) - log(mu*dic.len.up + n.words.up)
  log.pvec.down <- log(summ.down + mu) - log(mu*dic.len.down + n.words.down)
  return(list(up=log.pvec.up,down=log.pvec.down))
}




logp=make.log.pvec(dtm,0.001)


naive.bayes = function(logp.up, logp.down,
                       log.prior.up, log.prior.down , dtm.test) {
  p.up=sum(logp.up*dtm.test)+log.prior.up
  p.down=sum(logp.down*dtm.test)+log.prior.down
  ans=if (p.up>p.down) "up" else "down"
  return(ans)
}
result=array()
for (i in 2422:length(down.test)){
  result[i]=(naive.bayes(logp$up,logp$down,0.50,0.5,as.vector(dtm[down.test[i],])))
  print(i)
}
downresult=result
result=array()
for (i in 1:length(up.test)){
  result[i]=(naive.bayes(logp$up,logp$down,0.50,0.5,as.vector(dtm[up.test[i],])))
  print(i)}
upresult=result

upresult=read.csv("up.csv")
downresult=read.csv("down.csv")

belong=array()
for (i in 1:length(down.test)) {
  belong[down.test[i]]=downresult[i,2]
}
for (i in 1:length(up.test)) {
  belong[up.test[i]]=upresult[i,2]
}

predup=array(0,220)
preddown=array(0,220)

for (i in 20001:24169) {
  datei=temp[i,1]
  index=c(1:220)[stock$t==as.Date(datei)]
  if (belong[i]==2) predup[index]=predup[index]+1
  if (belong[i]==1) preddown[index]=preddown[index]+1
}
uprate=predup/(predup+preddown)
decision=rep("",220)
for (i in 105:126)
{decision[i]=if(uprate[i]>0.60) "buy" else {if (uprate[i]<0.4) "sell" else ""} }
write.csv(cbind(uprate,stock.return)[105:126,],"reans.csv")


plot(stock[105:126,2],col="Red",type="l",pch=decision[105:126])
par(new=T)
plot(uprate[105:126],type="l")
lines(uprate[105:126])
lines(stock[105:126,2],type="l",col="Blue")
abline(0,0)


