getwd()
setwd('~/Desktop/CARES/')

clean_data = read.csv('clean_data.csv')
#list(clean_data)
colnames(clean_data)

data = clean_data

library(ggplot2)
# bar plot
# year+Male
#dat1 <- data[which(data$Gender == 'Male' & data$Age.Unit %in% c('Month','Year')), ]
dat1 <- data[which(data$Gender == 'Male' & data$Age.Unit == 'Year'), ]
ggplot(data=dat1, aes(x=factor(Age_interval), fill=Industry)) + geom_bar(stat="count")

# month+Male
dat2 <- data[which(data$Gender == 'Male' & data$Age.Unit == 'Month'), ]
ggplot(data=dat2, aes(x=factor(Age_interval), fill=Industry)) + geom_bar(stat="count")

# day + Male
dat3 <- data[which(data$Gender == 'Male' & data$Age.Unit == 'Day'), ]
ggplot(data=dat3, aes(x=factor(Age_interval), fill=Industry)) + geom_bar(stat="count")

# year + Female
dat4 <- data[which(data$Gender == 'Female' & data$Age.Unit == 'Year'), ]
ggplot(data=dat4, aes(x=factor(Age_interval), fill=Industry)) + geom_bar(stat="count")

# month + female
dat5 <- data[which(data$Gender == 'Female' & data$Age.Unit == 'Month'), ]
ggplot(data=dat5, aes(x=factor(Age_interval), fill=Industry)) + geom_bar(stat="count")

# day + female
dat6 <- data[which(data$Gender == 'Female' & data$Age.Unit == 'Day'), ]
ggplot(data=dat6, aes(x=factor(Age_interval), fill=Industry)) + geom_bar(stat="count")

# year overall 
dat7 <- data[which(data$Age.Unit == 'Year'), ]
ggplot(data=dat7, aes(x=factor(Age_interval), fill=Industry)) + geom_bar(stat="count")

# month overall
dat8 <- data[which(data$Age.Unit == 'Month'), ]
ggplot(data=dat8, aes(x=factor(Age_interval), fill=Industry)) + geom_bar(stat="count")

# day overall
dat9 <- data[which(data$Age.Unit == 'Day'), ]
ggplot(data=dat9, aes(x=factor(Age_interval), fill=Industry)) + geom_bar(stat="count")




