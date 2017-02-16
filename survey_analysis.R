rm(list=ls(all=TRUE))
setwd("E:/insofe/r_directory/training")

survey_data = read.csv("Survey1.csv", header = TRUE)
str(survey_data)
sum(is.na(survey_data))
summary(survey_data)

unique(survey_data$CustomerId)

survey_data1 = survey_data
survey_data1$GENDER[which(is.na(survey_data1$GENDER))] = "female"

sum(is.na(survey_data1))


str(survey_data1)

survey_data_cat = subset(survey_data1, select = c(4:22,24,25,26,27))
survey_data_num = subset(survey_data1, select = -c(4:22,24,25,26,27))
str(survey_data_cat)
str(survey_data_num)


survey_data_cat = as.factor(survey_data_cat)

survey_cat = data.frame(lapply(survey_data1[,c(4:22,24,25,26,27)], function(x) as.factor(as.character(x))))

survey_data_formatted = cbind(survey_data_num, survey_cat)
str(survey_data_formatted)
str(survey_data_num)

trans1 = read.transactions(file="Transactions.csv", rm.duplicates= FALSE, format="single",sep=",",cols =c(1,2))

survey_data_ques = subset(survey_data_formatted, select = c(1,6:24))
str(survey_data_ques)
survey_data_formatted$Brand_Avail
str(survey_data_formatted)


survey_data_q <- lapply(survey_data_ques,function(x){as.numeric(as.character(x))})
survey_d = as.data.frame(survey_data_q)
str(survey_d)

data_formatted = as.data.frame(t(survey_data_ques))

trans = read.transactions(survey_data_q, rm.duplicates= FALSE, format="basket",sep=",",cols =c(1,2))
survey_data0 = as.data.frame(survey_data_q)
str(survey_data0)
index = which(survey_data0[1,] == 1, arr.ind = T)

paste(colnames(survey_data0)[index[2]], sep=", ")

a <- c(1,2,3)
b <- c(4,5,6)
test.df <- data.frame(a,b)
str(test.df)

a <- which(test.df==1,arr.ind=T)
a

names(test.df[a[,2]]) 

library(rpart)

survey_data_q1 <- survey_data0[1,] 

nrow(survey_data0)
survey_data0$questions <- NA
survey_data0$questions
str(survey_data0)

survey_data0$questions[1]

index = which(survey_data0[1,] == 1, arr.ind = T)
toString(paste(colnames(survey_data0)[index[,2]], sep=", "))


survey_data0 = survey_data0[,-21]
str(survey_data0)
for (i in 1:nrow(survey_data0)){
  index = which(survey_data0[i,] == 1, arr.ind = T)
  #index[,2]
  survey_data0$questions[i] <- noquote(toString(paste(colnames(survey_data0)[index[,2]], sep=", ")))
  
}

data_format1 = subset(survey_data0, select = c(1,21))
str(data_format1)

write.csv(data_format1, file = "rules.csv",quote = FALSE)

trans = read.transactions("rules.csv", rm.duplicates= FALSE, format="basket",sep=",",cols =1)
rules@quality
rules@lhs


rules@lhs@data@i

inspect(trans)
rules <- apriori(trans,parameter = list(sup = 0.4, conf = 0.6,target="rules"))
summary(rules) 
inspect(rules)
image(trans)



top.confidence <- sort(rules, decreasing = TRUE, na.last = NA, by = "confidence")
inspect(head(top.confidence,10))

rules_result = as(rules, "data.frame");
write.csv(rules_result,"rules_result.csv")
