
library(stringr)
library(tm)
library(wordcloud)
library("tm")
#install.packages("XML")
#install.packages("rvest")
library(XML)
library(rvest)

urlStringPart1 <- "https://www.amazon.com/Life-We-Bury-Allen-Eskens/product-reviews/1616149981/ref=cm_cr_arp_d_show_all?reviewerType=all_reviews&pageNumber="

url <- html("https://www.amazon.com/Life-We-Bury-Allen-Eskens/product-reviews/1616149981/ref=cm_cr_arp_d_show_all?reviewerType=all_reviews&pageNumber=1")
doc<-htmlParse(url)
pages<-xpathSApply(doc,'//li[@class="page-button"]',xmlValue)

noOfPages<- length(pages)
reviewsDF = data.frame(Reviews = character(), stringsAsFactors = FALSE)
reviewList = c(character())

if(noOfPages > 1){
  lastPage = as.numeric(pages[noOfPages])
}

pages <- c(1:lastPage)

retrieveReviews <- function(pages,urlStringPart1, reviewList){
  rrList = lapply(X = pages,FUN = function(page,urlStringPart1){
    urlStringPart2 = toString(page)
    urlCombined <- paste(urlStringPart1 ,urlStringPart2,sep = '')
    url <- html(urlCombined)
    doc<-htmlParse(url)
    review<- xpathSApply(doc,'//div[@class="a-row review-data"]',xmlValue)
    return (c(review))
  })
  return (rrList)
}


trainData <- read.csv("C:/Users/Angad/Dropbox (Personal)/Chahat/ADS/FinalProject/chahat.csv")
trainData$Body = trainData$Body[!is.na(trainData$Body)]
corpus <- Corpus(VectorSource(trainData$Body))
corpus$content[1]

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, stemDocument)
dtm <- DocumentTermMatrix(corpus)
col=brewer.pal(6,"Dark2")
par(mar = rep(2, 4))
wordcloud(corpus, min.freq=25, scale=c(2,2),rot.per = 0.25,
          random.color=T, max.word=45, random.order=F,colors=col)
positives= readLines("C:/Users/Angad/Dropbox (Personal)/Chahat/ADS/SentimentalAnalysis/positive-words.txt")
negatives = readLines("C:/Users/Angad/Dropbox (Personal)/Chahat/ADS/SentimentalAnalysis/negative-words.txt")
length(positives)

review <- trainData$Body
reviews<- review
rm(sentiment_scores)
sentiment_scores <- function(reviews, positives, negatives){
  scores = lapply(reviews,
                  function(review, positives, negatives){
                    review = gsub("[[:punct:]]", "", review)    
                    review = gsub("[[:cntrl:]]", "", review)   
                    review = gsub('\\d+', '', review)          
                    tryTolower <- function(x){
                      y <- NA
                      try_error <- tryCatch(tolower(x), error=function(e) e)
                      if (!inherits(try_error, "error"))
                        y <- tolower(x)
                      return(y)
                    }
                    review <- sapply(review, tryTolower)
                    word_list <- str_split(review, "\\s+")
                    words <- unlist(word_list)
                    positive_matches <- match(words, positives)
                    negative_matches <- match(words, negatives)
                    
                    positive_matches <- !is.na(positive_matches)
                    negative_matches <- !is.na(negative_matches)
                    
                    total <- sum(positive_matches) + sum(negative_matches)
                    
                    positivePercent <- (sum(positive_matches) / total) * 100
                    negativePercent <- (sum(negative_matches) / total) * 100
                    barplot(c(positivePercent,negativePercent),beside = TRUE, col = c("green","red")
                            ,legend.text = c("Positive","Negative"),  
                            ylim = c(0,100), xlab = 'Topics(Z)',ylab = 'P(z)', sub = substr(review,1,60))
                    score <- c(  positivePercent,  negativePercent)
                    return(score)
                  }, positives, negatives )
  return(scores)
}
score1 <- sentiment_scores(review, positives, negatives)
apply(score1,2,mean)
barplot(apply(score1,2,mean),beside = TRUE, col = c("green","red")
        ,legend.text = c("Positive","Negative"),  
        ylim = c(0,100), xlab = 'Topics(Z)',ylab = 'P(z)', sub = 'Overall Sentiment')



