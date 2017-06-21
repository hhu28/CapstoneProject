setwd('~/Desktop/CARES/')

library(plyr)
library(ggplot2)
clean_data = read.csv('clean_data.csv')
#list(clean_data)
colnames(clean_data)
data = clean_data
data$Year = as.factor(data$Year)
colnames(data)

#freq.table <- prop.table((xtabs(~resp+sound, data=df)), 2)
#freq.table.df <- as.data.frame(freq.table)
# year: top 5
dat1 =  data[which(data$Age.Unit == 'Year'), ]
attach(dat1)
t1 <- count(dat1, c('Year','Industry'))
#t1 <- as.data.frame(t1)

# plot 1
ggplot(t1[which(t1$Industry %in% c('Unconventional_food',
                                       'Cosmetics',
                                       'Nuts',
                                       'Fishery',
                                       'Bakery Prod',
                                         
                                       )), ], 
aes(Year, freq, group=Industry, color=Industry)) + geom_line()

ggplot(t1[which(t1$Industry %in% c('Dietary Conv Food',
                                   'Soft Drink',
                                   'Fruit',
                                   'Vegetables',
                                   'Milk'                                                                 
)), ], aes(Year, freq, group=Industry, color=Industry)) + geom_line()

ggplot(t1[which(t1$Industry %in% c(
                                   
                                   'Dietary Conv Food',
                                   'Soft Drink',
                                   'Fruit',
                                   'Vegetables',
                                   'Milk'
)), ], aes(Year, Freq, group=Industry, color=Industry)) + geom_line()

ggplot(t1[which(t1$Industry %in% c('Unconventional_food',
                                   'Cosmetics'
                                  
)), ], aes(Year, Freq, group=Industry, color=Industry)) + geom_line()

ggplot(t1[which(t1$Industry %in% c(
                                   'Nuts',
                                   'Fishery',
                                   'Bakery Prod',
                                   'Milk'
)), ], aes(Year, Freq, group=Industry, color=Industry)) + geom_line()

# year compare female and male
dat11 =  data[which(data$Age.Unit == 'Year'), ]
t11 <- count(dat11, c('Year','Industry','Gender'))
label1 = c( na = 'Unknown', Female = 'Female', Male = 'Male')
p11 <- ggplot(t11[which(t11$Industry %in% c('Unconventional_food',
                                   'Cosmetics',
                                   'Nuts',
                                   'Fishery',
                                   'Bakery Prod'                  
)), ], 
aes(Year, freq, group=Industry, color=Industry)) + geom_line() + theme(axis.text.x = element_text(angle = 90)) + facet_wrap(~Gender, labeller=labeller(Gender = label1))

p11
png('p2.png')
p11 + labs(
           x = 'Year reported', y = 'Count')
dev.off()

# Month
dat2 =  data[which(data$Age.Unit == 'Month'), ]
attach(dat2)
t2 <- count(dat2, c('Year','Industry'))
ggplot(t2[which(t2$Industry %in% c(
 'Baby Food Prod',
 'Unconventional_food',
 'Dietary Conv Food',
 'Cosmetics',
 'Soft Drink'
)), ], aes(Year, freq, group=Industry, color=Industry)) + geom_line()

detach(dat2)

# day
dat3 =  data[which(data$Age.Unit == 'Day'), ]
attach(dat3)
t3 <- count(dat3, c('Year','Industry'))
ggplot(t3[which(t3$Industry %in% c(
  'Baby Food Prod',
  'Unconventional_food',
  'Cosmetics'
)), ], aes(Year, freq, group=Industry, color=Industry)) + geom_line()

# female
dat4 =  data[which(data$Age.Unit == 'Year' & data$Gender == 'Female'), ]
#attach(dat4)
t4 <- count(dat4, c('Year','Industry'))

ggplot(t4[which(t4$Industry %in% c('Unconventional_food',
                                   'Cosmetics',
                                   'Nuts',
                                   'Fishery',
                                   'Bakery Prod'
                                   
)), ], 
aes(Year, freq, group=Industry, color=Industry)) + geom_line()

# male
dat5 =  data[which(data$Age.Unit == 'Year' & data$Gender == 'Male'), ]
#attach(dat5)
t5 <- count(dat5, c('Year','Industry'))

ggplot(t5[which(t5$Industry %in% c('Unconventional_food',
                                   'Cosmetics',
                                   'Nuts',
                                   'Fishery',
                                   'Bakery Prod'                             
)), ], 
aes(Year, freq, group=Industry, color=Industry)) + geom_line()

# Month: female
dat6 =  data[which(data$Age.Unit == 'Month' & data$Gender == 'Female'), ]

t6 <- count(dat6, c('Year','Industry'))
ggplot(t6[which(t6$Industry %in% c(
  'Baby Food Prod',
  'Unconventional_food',
  'Dietary Conv Food',
  'Cosmetics',
  'Soft Drink'
)), ], aes(Year, freq, group=Industry, color=Industry)) + geom_line()

# Month: compare female and male
dat7 =  data[which(data$Age.Unit == 'Month'), ]
t7 <- count(dat7, c('Year','Industry', 'Gender'))
label2 = c(NA = 'Unknown', Female = 'Female', Male = 'Male')
p7 = ggplot(t7[which(t7$Industry %in% c(
  'Baby Food Prod',
  'Unconventional_food',
  'Dietary Conv Food',
  'Cosmetics',
  'Soft Drink'
)), ], aes(Year, freq, group=Industry, color=Industry)) + geom_line() + facet_wrap(~Gender, labeller=labeller(Gender = label2))

#multiplot(p11, p7, cols=1)
library(grid)
grid.arrange(p11, p7, ncol = 1)

# day: compare female and male
dat12 =  data[which(data$Age.Unit == 'Day'), ]
t12 <- count(dat12, c('Year','Industry', 'Gender'))
ggplot(t12[which(t12$Industry %in% c(
  'Baby Food Prod',
  'Unconventional_food',
  'Cosmetics',
  'Soft Drink',
  'Dietary Conv Food'
)), ], aes(Year, freq, group=Industry, color=Industry)) + geom_line() + facet_wrap(~Gender)

