rm(list=ls())
library(quanteda)
library(RColorBrewer)
library(randomForest)
raw.data <- read.csv("C:/Users/S Kwon/Desktop/SPAM text message 20170820 - Data.csv", header=TRUE, sep=",", quote='\"\"', stringsAsFactors=FALSE)
raw.data = raw.data[raw.data$Category %in% c("spam","ham"),]
names(raw.data) <- c("Category", "Message")

set.seed(1912)  
sms.corpus <- corpus(raw.data$Message)

#spam wordcloud
docvars(sms.corpus,field = "aa") <- raw.data$Category
spam.plot <- corpus_subset(sms.corpus, aa == "spam")  
spam.plot <- dfm(spam.plot, tolower = TRUE, remove_punct = TRUE, remove_twitter = TRUE, remove_numbers = TRUE, remove=stopwords("smart"))
spam.col <- brewer.pal(10, "BrBG")  
spam.cloud <- textplot_wordcloud(spam.plot, min_count = 10, color = spam.col)  
title("Spam Wordcloud", col.main = "grey14")

#ham wordcloud
spam.plot <- corpus_subset(sms.corpus, aa == "ham")  
spam.plot <- dfm(spam.plot, tolower = TRUE, remove_punct = TRUE, remove_twitter = TRUE, remove_numbers = TRUE, remove=stopwords("smart"))
spam.col <- brewer.pal(10, "BrBG")  
spam.cloud <- textplot_wordcloud(spam.plot, min_count = 10, color = spam.col)  
title("Ham Wordcloud", col.main = "grey14")

#dfm matrix
sms.dfm <- dfm(sms.corpus, tolower = TRUE)  
sms.dfm <- dfm_trim(sms.dfm, min_docfreq = 3)  
colnames(as.matrix(sms.dfm))

#train test split
sms.raw.train <- raw.data[1:1000,]  
sms.raw.test <- raw.data[1001:nrow(raw.data),]
sms.dfm.train <- sms.dfm[1:1000,]  
sms.dfm.test <- sms.dfm[1001:nrow(raw.data),] 

#construct glm model
train.df = data.frame(Y=raw.data$Category[1:1000],X=convert(sms.dfm.train,to="matrix"))
test.df = data.frame(Y=raw.data$Category[1001:nrow(raw.data)],X=convert(sms.dfm.test,to="matrix"))
glm.fit = glm(Y~.,data=train.df,family = "binomial")
table(test.df$Y,(predict(glm.fit,test.df)>0)+0)

#construct rf model
rf = randomForest(as.matrix(sms.dfm.train), as.factor(sms.raw.train$Category),ntree=500)
table(sms.raw.test$Category,predict(rf,as.matrix(sms.dfm.test)))

#construct textclf model
sms.classifier <- textmodel_nb(sms.dfm.train, sms.raw.train$Category, prior = "docfreq")
sms.predictions <- predict(sms.classifier, newdata = sms.dfm.test)
table(sms.raw.test$Category,sms.predictions )
