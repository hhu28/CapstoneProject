# Plots: life threatening
getwd()
setwd('~/Dropbox/CARES')

library(ggplot2)
data = read.csv('life_threatening.csv')

data <- data[which(data$Age.Unit == 'Year' & data$Industry %in% c('Unconventional_food',
                                                                  'Cosmetics',
                                                                  'Nuts',
                                                                  'Fishery',
                                                                  'Bakery Prod'
)), ]

#label7 = c( na = 'Unknown', Female = 'Female', Male = 'Male')
t1 = aggregate(life_threatening ~ Industry + Age_interval, data, FUN=sum)
t1[order(-t1$life_threatening), ]

# a pie chart
pie_chart = read.csv('pie_chart.csv')
pie = ggplot(pie_chart[which(pie_chart$life_threatening == 1)], aes(x = '', y=life_threatening, fill=industry))
+ geom_bar(stat = "identity") 

+ coord_polar("y", start=0)

#  bar chart: better with y axis: proportion
p1 = ggplot(data=data[which(data$Industry %in% c('Unconventional_food',
                        'Fishery',
                        'Cosmetics',
                        'Dietary Conv Food',
                        'Bakery Prod',
                        'Soft Drink') & (data$life_threatening == 1), ),]
       , aes(x=factor(Age_interval), fill=Industry)) + 
  geom_bar(stat="count", position=position_dodge(width=0.9)) 
p1 + geom_text(stat='count',aes(label=..count..),position=position_dodge(width=.9), size=4, vjust=-.25)
#+ theme(axis.text.x = element_text(angle = 30)) 

png('p1.png')
p7 +facet_wrap(~Gender, labeller=labeller(Gender = label7)) + labs(x = 'Age Interval')
dev.off()

library(plyr)
all = count(data[which(data$Industry %in% c('Unconventional_food',
                                            'Fishery',
                                            'Cosmetics', 
                                            'Dietary Conv Food',
                                            'Bakery Prod',
                                            'Soft Drink')),], c('Industry', 'Age_interval'))
life = count(data[which(data$Industry %in% c('Unconventional_food',
                                             'Fishery',
                                             'Cosmetics'
                                             'Dietary Conv Food',
                                             'Bakery Prod',
                                             'Soft Drink') & data$life_threatening == 1),], c('Industry', 'Age_interval'))

m = merge(all, life, by = c('Industry', 'Age_interval'),  all.x = TRUE, all.y = TRUE)
m$prop = m$freq.y / m$freq.x
p2 = ggplot(data=m, aes(x=Age_interval, y = prop, fill=Industry)) + geom_bar(stat="identity", position=position_dodge(width=0.9)) 
p2
# logistic regresion
s1 = data[which(data$Industry %in% c('Unconventional_food',
                                              'Fishery',
                                              'Cosmetics',
                                              'Dietary Conv Food',
                                              'Bakery Prod',
                                              'Soft Drink')), ]
m1 <- glm(life_threatening ~ Industry + Age_interval + Gender , data = s1, family = "binomial")
summary(m1)

s2 = data[which(data$Industry %in% c('Unconventional_food',
                                     'Fishery',
                                     'Cosmetics',
                                     'Dietary Conv Food',
                                     'Bakery Prod',
                                     'Soft Drink') & data$Age_interval == '21-40'), ]

m2 <- glm(life_threatening ~ Industry + Gender, data = s2, family = "binomial")
summary(m2)

s3 = data[which(data$Industry %in% c('Unconventional_food',
                                     'Fishery',
                                     'Cosmetics',
                                     'Dietary Conv Food',
                                     'Bakery Prod',
                                     'Soft Drink') & data$Age_interval == '41-60'), ]

m3 <- glm(life_threatening ~ Industry + Gender, data = s3, family = "binomial")
summary(m3)

s4 = data[which(data$Industry %in% c('Unconventional_food',
                                     'Fishery',
                                     'Cosmetics',
                                     'Dietary Conv Food',
                                     'Bakery Prod',
                                     'Soft Drink') & data$Age_interval == '61-80'), ]

m4 <- glm(life_threatening ~ Industry + Gender, data = s4, family = "binomial")
summary(m4)

s5 = data[which(data$Industry %in% c('Unconventional_food',
                                     'Fishery',
                                     'Cosmetics',
                                     'Dietary Conv Food',
                                     'Bakery Prod',
                                     'Soft Drink') & data$Age_interval == '61-80'), ]

m5 <- glm(life_threatening ~ Industry + Gender, data = s5, family = "binomial")
summary(m5)


