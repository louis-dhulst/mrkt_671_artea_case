######Imports
install.packages("readxl")
install.packages("writexl")

library("readxl")
library(ggplot2)
library(gridExtra)
library(dplyr)
library(car)
library(purrr)
library(tidyverse)
library("writexl")

##### Data Handling

ABtest <- read_excel("521703-XLS-ENG.xlsx", sheet=2)
nextcampaign <- read_excel("521703-XLS-ENG.xlsx", sheet=3)

#For Vaibhav's code
artea_ab <- data.frame(ABtest)
artea_nc <- data.frame(nextcampaign)


summary(ABtest)

attach(ABtest)

#Numeric to numeric and factor to factor
ABtest = ABtest %>% 
  mutate_at(vars(trans_after,revenue_after,num_past_purch,spent_last_purchase,
                 weeks_since_visit, browsing_minutes),as.numeric) %>% 
  mutate_at(vars(id,minority,female,test_coupon, channel_acq, shopping_cart),
            as.factor)

#recode the channel_acq factor to set an appropriate baseline
levels(ABtest$channel_acq) <- c("Google", "Facebook", "Instragram","Referral", "Other")

#Create dataframes for test_coupon group 0 and 1
ABtest_gp0 <- subset(ABtest, test_coupon==0)
ABtest_gp1 <- subset(ABtest, test_coupon==1)

#Numeric columns
vec.num <- c(2,3,8,9,10,11)
###Histogram plots of continuous variables
histplot = function (data, column) {
  ggplot(data, aes_string(x = column)) +
    geom_histogram(fill = "blue") +
    xlab(column) 
    
}
list.histplots <- lapply(colnames(ABtest), histplot, data = ABtest[vec.num])
names(list.histplots) <- colnames(ABtest)

#Arrange in grid
n <- length(list.histplots)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(list.histplots, ncol=nCol))

##Histogram plots of every variable: group test_coupon=1
list.histplots_gp1 <- lapply(colnames(ABtest_gp1), histplot, data = ABtest_gp1[vec.num])
names(list.histplots_gp1) <- colnames(ABtest_gp1)

#Arrange in grid
n <- length(list.histplots_gp1)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(list.histplots_gp1, ncol=nCol))

##Histogram plots of every variable: group test_coupon=0
list.histplots_gp0 <- lapply(colnames(ABtest), histplot, data = ABtest_gp0[vec.num])
names(list.histplots_gp0) <- colnames(ABtest)

#Arrange in grid
n <- length(list.histplots_gp0)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(list.histplots_gp0, ncol=nCol))

###Scatter plots of every variable
pairs(ABtest[,2:12], pch=10, lower.panel=NULL)

###Scatter plots to check relation between revenue_after and explanatory variables
scatplot_rev = function (data, column) {
  ggplot(data, aes_string(y=ABtest$revenue_after, x=column)) +
    geom_point() +
    xlab(column) +
    ylab("revenue_after")
  
}
vec.vars <- c(2:12)
list.scatplots_rev <- lapply(colnames(ABtest[,vec.vars]), scatplot_rev, data = ABtest)
names(list.scatplots_rev) <- colnames(ABtest[,vec.vars])

n <- length(list.scatplots_rev)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(list.scatplots_rev, ncol=nCol))


###Check normality of distributions
#For revenue_after: not normal
with(ABtest, shapiro.test(revenue_after[test_coupon == 0]))
with(ABtest, shapiro.test(revenue_after[test_coupon == 1]))

 

###Hypothesis testing: check if two groups are comparable
#For that, use a non parametric approach due to large differences in distributions
#Wilcoxon two-sample so Mann-Whitney, null hypothesis of same distribution
wilc.df <- data.frame(matrix(ncol = (length(ABtest) - 1), nrow = 1))
colnames(wilc.df) <- colnames(ABtest[,2:12])

wilc.df["trans_after"] <- wilcox.test(trans_after ~ test_coupon, data = ABtest)$p.value #trans_after
wilc.df["revenue_after"] <- wilcox.test(revenue_after ~ test_coupon, data = ABtest)$p.value #Revenue_after
wilc.df["minority"] <- wilcox.test(minority ~ test_coupon, data = ABtest)$p.value #minority
wilc.df["female"] <- wilcox.test(female ~ test_coupon, data = ABtest)$p.value #female
wilc.df["channel_acq"] <- wilcox.test(channel_acq ~ test_coupon, data = ABtest)$p.value #channel_acq
wilc.df["num_past_purch"] <- wilcox.test(num_past_purch ~ test_coupon, data = ABtest)$p.value #num_past_purch
wilc.df["spent_last_purchase"] <- wilcox.test(spent_last_purchase ~ test_coupon, data = ABtest)$p.value #spent_last_purchase
wilc.df["weeks_since_visit"] <- wilcox.test(weeks_since_visit ~ test_coupon, data = ABtest)$p.value #weeks_since_visit
wilc.df["browsing_minutes"] <- wilcox.test(browsing_minutes ~ test_coupon, data = ABtest)$p.value #browsing_minutes
wilc.df["shopping_cart"] <- wilcox.test(shopping_cart ~ test_coupon, data = ABtest)$p.value #shopping_cart

#Can see that all of the variables have similar distributions even in groups


#Sum of revenue by test coupon group
sum.rev <- aggregate(ABtest$revenue_after, by=list(Category=ABtest$test_coupon), FUN=sum)
#Mean of revenue by test coupon group
avg.rev <- aggregate(ABtest$revenue_after, by=list(Category=ABtest$test_coupon), FUN=mean)
#Stdev of revenue
std.rev <- aggregate(ABtest$revenue_after, by=list(Category=ABtest$test_coupon), FUN=sd)

#Sum of transactions by test coupon group
sum.trans <- aggregate(ABtest$trans_after, by=list(Category=ABtest$test_coupon), FUN=sum)
#Mean of revenue by test coupon group
avg.trans <- aggregate(ABtest$trans_after, by=list(Category=ABtest$test_coupon), FUN=mean)

##### Correlation matrix
cor(ABtest[vec.num])

######Basic linear regressions
test.lreg <- lm(revenue_after ~  test_coupon + minority + female + channel_acq + 
                  num_past_purch  + weeks_since_visit + shopping_cart + spent_last_purchase + browsing_minutes, data = ABtest)
summary(test.lreg)

test.lreg_transac <- lm(trans_after ~  test_coupon + minority + female + channel_acq + 
                          num_past_purch  + weeks_since_visit + shopping_cart + spent_last_purchase + browsing_minutes, data = ABtest)
summary(test.lreg_transac)

#Residual Plots and Tuckey test for non-linearity
residualPlots(test.lreg)
residualPlots(test.lreg_transac)

#ncvTests
ncvTest(test.lreg)
ncvTest(test.lreg_transac)

#Other attempt at explanatory linear regression
test2.lreg <- lm(revenue_after ~  test_coupon + minority + female + channel_acq + 
                   poly(num_past_purch,3)  + weeks_since_visit + shopping_cart + spent_last_purchase + browsing_minutes
                 + channel_acq * test_coupon + shopping_cart * test_coupon, data = ABtest)
summary(test2.lreg)

test3.lreg <- lm(revenue_after ~  test_coupon + minority + female + channel_acq + 
                   poly(num_past_purch,3)  + weeks_since_visit + shopping_cart + spent_last_purchase + browsing_minutes
                  + shopping_cart * test_coupon * channel_acq, data = ABtest)
summary(test3.lreg)


##### Predicting revenue and transactions of next campaign using basic model and coupon assignment
nextcampaign$test_coupon <- numeric(nrow(nextcampaign))
nextcampaign$test_coupon[sample(nrow(nextcampaign), nrow(nextcampaign)/2)] <- 1


nextcampaign = nextcampaign %>% 
  mutate_at(vars(num_past_purch,spent_last_purchase,
                 weeks_since_visit, browsing_minutes),as.numeric) %>% 
  mutate_at(vars(test_coupon, channel_acq, shopping_cart),
            as.factor)

#recode the channel_acq factor to set an appropriate baseline
levels(nextcampaign$channel_acq) <- c("Google", "Facebook", "Instragram","Referral", "Other")

#Fit models without demographic variables since they are not in nextcampaign
rev.lreg <- lm(revenue_after ~ test_coupon + channel_acq + num_past_purch  + weeks_since_visit 
               + shopping_cart + spent_last_purchase, data = ABtest)
summary(rev.lreg) 

trans.lreg <- lm(trans_after ~ test_coupon + channel_acq + num_past_purch  + weeks_since_visit 
              + shopping_cart + spent_last_purchase, data = ABtest)
summary(trans.lreg) 

yo <- lm(revenue_after ~ test_coupon + channel_acq + num_past_purch  + weeks_since_visit 
         + shopping_cart + spent_last_purchase + channel_acq*test_coupon, data = ABtest)
summary(yo) 


#Predicting revenue
predict.rev <- predict(rev.lreg, nextcampaign)
sum(predict.rev)
predict.trans <- predict(trans.lreg, nextcampaign)
sum(predict.trans)

##### Trying to see if high revenue did better or worse with coupon
spent_last_quantile <- quantile(ABtest$spent_last_purchase, probs = seq(0, 1, by= 0.2))
#80% is 83.99
#Give top 20% 1, others 0
ABtest$top_20 <- ifelse(spent_last_purchase >= spent_last_quantile[5], 1, 0)
ABtest$top_40 <- ifelse(spent_last_purchase >= spent_last_quantile[4], 1, 0)
ABtest$top_60 <- ifelse(spent_last_purchase >= spent_last_quantile[3], 1, 0)

ABtest = ABtest %>% 
  mutate_at(vars(top_20,top_40,top_60),
            as.factor)

#Histogram revenue_after for top 20% vs others
ggplot(ABtest, aes(x = revenue_after, fill = top_20, colour = top_20)) + 
  geom_histogram(alpha = 0.5, position = "identity") 

#Histogram revenue_after for top 40% vs others
ggplot(ABtest, aes(x = revenue_after, fill = top_40, colour = top_40)) + 
  geom_histogram(alpha = 0.5, position = "identity") 

#Histogram revenue_after for top 60% vs others
ggplot(ABtest, aes(x = revenue_after, fill = top_60, colour = top_60)) + 
  geom_histogram(alpha = 0.5, position = "identity") 

#Sum of revenue for top_20% spenders who were given coupon vs not
sum.rev.top_20 <- aggregate(revenue_after ~ test_coupon + top_20, ABtest, sum)
#Sum of revenue for top_40% spenders who were given coupon vs not
sum.rev.top_40 <- aggregate(revenue_after ~ test_coupon + top_40, ABtest, sum)
#Sum of revenue for top_60% spenders who were given coupon vs not
sum.rev.top_60 <- aggregate(revenue_after ~ test_coupon + top_60, ABtest, sum)

#Mean of revenue for top_20% spenders who were given coupon vs not
avg.rev.top_20 <- aggregate(revenue_after ~ test_coupon + top_20, ABtest, mean)
#Mean of revenue for top_40% spenders who were given coupon vs not
avg.rev.top_40 <- aggregate(revenue_after ~ test_coupon + top_40, ABtest, mean)
#Mean of revenue for top_60% spenders who were given coupon vs not
avg.rev.top_60 <- aggregate(revenue_after ~ test_coupon + top_60, ABtest, mean)

##### Trying to see if channels matter as preliminary regressions indicate they do

#Sum of revenue for channelAcq who were given coupon vs not
sum.rev.channels <- aggregate(revenue_after ~ test_coupon + channel_acq, ABtest, sum)

#Mean of revenue for channelAcq who were given coupon vs not
avg.rev.channels <- aggregate(revenue_after ~ test_coupon + channel_acq, ABtest, mean)

#Sum of transactions for channelAcq who were given coupon vs not
sum.trans.channels <- aggregate(trans_after ~ test_coupon + channel_acq, ABtest, sum)

#Mean of transactions for channelAcq who were given coupon vs not
avg.trans.channels <- aggregate(trans_after ~ test_coupon + channel_acq, ABtest, mean)



## Testing to see if differences are significant
ABtest.instagram <- subset(ABtest, channel_acq=="Instragram")
wilcox.test(revenue_after ~ test_coupon, data = ABtest.instagram)

ABtest.google <- subset(ABtest, channel_acq=="Google")
wilcox.test(revenue_after ~ test_coupon, data = ABtest.google)

ABtest.other <- subset(ABtest, channel_acq=="Other")
wilcox.test(revenue_after ~ test_coupon, data = ABtest.other)


##### Shopping Cart

#Sum of revenue for shopping_cart = 1 who were given coupon vs not
sum.rev.cart <- aggregate(revenue_after ~ test_coupon + shopping_cart, ABtest, sum)
#Mean of revenue for shopping_cart = 1 who were given coupon vs not
avg.rev.cart <- aggregate(revenue_after ~ test_coupon + shopping_cart, ABtest, mean)

#Sum of transactions for shopping_cart = 1 who were given coupon vs not
sum.trans.cart <- aggregate(trans_after ~ test_coupon + shopping_cart, ABtest, sum)
#Mean of transactions for shopping_cart = 1 who were given coupon vs not
avg.trans.cart <- aggregate(trans_after ~ test_coupon + shopping_cart, ABtest, mean)


##Testing if significant
ABtest.cart <- subset(ABtest, shopping_cart==1)
wilcox.test(revenue_after ~ test_coupon, data = ABtest.cart)



##### Minority
sum.rev.minority <- aggregate(revenue_after ~ test_coupon + minority, ABtest, sum)
#Mean of revenue for top_60% spenders who were given coupon vs not
avg.rev.minority <- aggregate(revenue_after ~ test_coupon + minority, ABtest, mean)

##Testing if significant
ABtest.minority <- subset(ABtest, minority==1)
wilcox.test(revenue_after ~ test_coupon, data = ABtest.minority)

##### Female
sum.rev.female <- aggregate(revenue_after ~ test_coupon + female, ABtest, sum)
#Mean of revenue for top_60% spenders who were given coupon vs not
avg.rev.female <- aggregate(revenue_after ~ test_coupon + female, ABtest, mean)

##Testing if significant
ABtest.female <- subset(ABtest, female==1)
wilcox.test(revenue_after ~ test_coupon, data = ABtest.female)

ABtest.male <- subset(ABtest, female==0)
wilcox.test(revenue_after ~ test_coupon, data = ABtest.male)

##### Shopping_cart * channels
sum.rev.cart_channels <- aggregate(revenue_after ~ test_coupon + shopping_cart + channel_acq, ABtest, sum)
#Mean of revenue for top_60% spenders who were given coupon vs not
avg.rev.cart_channels <- aggregate(revenue_after ~ test_coupon + shopping_cart + channel_acq, ABtest, mean)

## Testing for instagram and shopping cart
ABtest.cart_instagram <- subset(ABtest, channel_acq == "Instragram" & shopping_cart == 1)
wilcox.test(revenue_after ~ test_coupon, data = ABtest.cart_instagram)

## Testing for facebook and shopping cart
ABtest.cart_facebook <- subset(ABtest, channel_acq == "Facebook" & shopping_cart == 1)
wilcox.test(revenue_after ~ test_coupon, data = ABtest.cart_facebook)

## Testing for Referral and shopping cart
ABtest.cart_referral <- subset(ABtest, channel_acq == "Referral" & shopping_cart == 1)
wilcox.test(revenue_after ~ test_coupon, data = ABtest.cart_referral)

##### Channels * female

#Sum of revenue for shopping_cart = 1 who were given coupon vs not
sum.rev.channel_fem <- aggregate(revenue_after ~ test_coupon + female + channel_acq, ABtest, sum)
#Mean of revenue for shopping_cart = 1 who were given coupon vs not
avg.rev.channel_fem <- aggregate(revenue_after ~ test_coupon + female + channel_acq, ABtest, mean)

#Sum of transactions for shopping_cart = 1 who were given coupon vs not
sum.trans.channel_fem <- aggregate(trans_after ~ test_coupon + female + channel_acq, ABtest, sum)
#Mean of transactions for shopping_cart = 1 who were given coupon vs not
avg.trans.channel_fem <- aggregate(trans_after ~ test_coupon + female + channel_acq, ABtest, mean)

## Testing significance

## Testing for cart = 1, female = 1, channel_acq = Facebook
ABtest.signif <- subset(ABtest, channel_acq == "Facebook" & shopping_cart == 1 & female == 1)
wilcox.test(revenue_after ~ test_coupon, data = ABtest.signif)

## Testing for cart = 1, female = 0, channel_acq = Other
ABtest.signif <- subset(ABtest, channel_acq == "Other" & shopping_cart == 1 & shopping_cart == 1)
wilcox.test(revenue_after ~ test_coupon, data = ABtest.signif)


##### Shopping_cart * channels * female

#Sum of revenue for shopping_cart = 1 who were given coupon vs not
sum.rev.3 <- aggregate(revenue_after ~ test_coupon + shopping_cart + female + channel_acq, ABtest, sum)
#Mean of revenue for shopping_cart = 1 who were given coupon vs not
avg.rev.3 <- aggregate(revenue_after ~ test_coupon + shopping_cart + female + channel_acq, ABtest, mean)

#Sum of transactions for shopping_cart = 1 who were given coupon vs not
sum.trans.3 <- aggregate(trans_after ~ test_coupon + shopping_cart + female + channel_acq, ABtest, sum)
#Mean of transactions for shopping_cart = 1 who were given coupon vs not
avg.trans.3 <- aggregate(trans_after ~ test_coupon + shopping_cart + female + channel_acq, ABtest, mean)

## Testing significance

## Testing for cart = 1, female = 1, channel_acq = Facebook
ABtest.signif <- subset(ABtest, channel_acq == "Facebook" & shopping_cart == 1 & female == 1)
wilcox.test(revenue_after ~ test_coupon, data = ABtest.signif)

## Testing for cart = 1, female = 0, channel_acq = Other
ABtest.signif <- subset(ABtest, channel_acq == "Other" & shopping_cart == 1 & shopping_cart == 1)
wilcox.test(revenue_after ~ test_coupon, data = ABtest.signif)


##### Plots for Q3-4

### Plots for channel_acq
#Sum of revenue
gg.channel_rev <- ggplot(sum.rev.channels, aes(fill=channel_acq, y=revenue_after, x=test_coupon)) +
  geom_bar(position="dodge", stat="identity") +
  labs(y= "Post-test Revenue", x = "Coupon") +
  scale_x_discrete(labels= c("No", "Yes")) 

#Mean of revenue
gg.channel_rev_mean <- ggplot(avg.rev.channels, aes(fill=channel_acq, y=revenue_after, x=test_coupon)) +
  geom_bar(position="dodge", stat="identity") +
  labs(y= "Post-test Average Revenue", x = "Coupon") +
  scale_x_discrete(labels= c("No", "Yes")) 

#Sum of transactions
gg.channel_trans <- ggplot(sum.trans.channels, aes(fill=channel_acq, y=trans_after, x=test_coupon)) +
  geom_bar(position="dodge", stat="identity") +
  labs(y= "Post-test Transactions", x = "Coupon") +
  scale_x_discrete(labels= c("No", "Yes")) 
  
#Mean of transactions
gg.channel_trans_mean <- ggplot(avg.trans.channels, aes(fill=channel_acq, y=trans_after, x=test_coupon)) +
  geom_bar(position="dodge", stat="identity") +
  labs(y= "Post-test Average Transactions", x = "Coupon") +
  scale_x_discrete(labels= c("No", "Yes")) 

grid.arrange(gg.channel_rev, gg.channel_rev_mean, gg.channel_trans, gg.channel_trans_mean)

### Plots for shopping_cart
#Sum of revenue
gg.cart_rev <- ggplot(sum.rev.cart, aes(fill=shopping_cart, y=revenue_after, x=test_coupon)) +
  geom_bar(position="dodge", stat="identity") +
  labs(y= "Post-test Revenue", x = "Coupon") +
  scale_x_discrete(labels= c("No", "Yes")) +
  scale_fill_discrete(name="Shopping Cart",
                     labels=c("No","Yes"))
#Mean of revenue
gg.cart_rev_mean <- ggplot(avg.rev.cart, aes(fill=shopping_cart, y=revenue_after, x=test_coupon)) +
  geom_bar(position="dodge", stat="identity") +
  labs(y= "Post-test Average Revenue", x = "Coupon") +
  scale_x_discrete(labels= c("No", "Yes")) +
  scale_fill_discrete(name="Shopping Cart",
                      labels=c("No","Yes"))

#Sum of transactions
gg.cart_trans <- ggplot(sum.trans.cart, aes(fill=shopping_cart, y=trans_after, x=test_coupon)) +
  geom_bar(position="dodge", stat="identity") +
  labs(y= "Post-test Transactions", x = "Coupon") +
  scale_x_discrete(labels= c("No", "Yes")) +
  scale_fill_discrete(name="Shopping Cart",
                      labels=c("No","Yes"))
#Mean of transactions
gg.cart_trans_mean <- ggplot(avg.trans.cart, aes(fill=shopping_cart, y=trans_after, x=test_coupon)) +
  geom_bar(position="dodge", stat="identity") +
  labs(y= "Post-test Average Transactions", x = "Coupon") +
  scale_x_discrete(labels= c("No", "Yes")) +
  scale_fill_discrete(name="Shopping Cart",
                      labels=c("No","Yes"))

grid.arrange(gg.cart_rev, gg.cart_rev_mean, gg.cart_trans, gg.cart_trans_mean)

#### Big grid
grid.arrange(gg.channel_rev, gg.channel_trans,
             gg.cart_rev, gg.cart_trans,
             ncol = 2)

##### Table question E
avg.rev.3$diff <- rep(avg.rev.3$revenue_after[seq(2,nrow(avg.rev.3),by=2)] - avg.rev.3$revenue_after[seq(1,nrow(avg.rev.3),by=2)], each=2)
diff_table <- avg.rev.3[ !duplicated(avg.rev.3$diff), ]
diff_table = diff_table[,-1]

sum.rev.3$diff <- rep(sum.rev.3$revenue_after[seq(2,nrow(sum.rev.3),by=2)] - sum.rev.3$revenue_after[seq(1,nrow(sum.rev.3),by=2)], each=2)
diff_table_sum <- sum.rev.3[ !duplicated(sum.rev.3$diff), ]
diff_table_sum = diff_table_sum[,-1]

diff_table_sum$diff_avg <- diff_table$diff

write_xlsx(diff_table_sum,"C:\\Users\\louis\\OneDrive - McGill University\\MMA Winter 2021 Notes\\MRKT671\\Artea Case\\Data\\diff_table.xlsx")

#Have identified 5 positive results greater than 5
#1: cart = 1, female = 1, channel_acq = Facebook
ABtest.signif <- subset(ABtest, channel_acq == "Facebook" & shopping_cart == 1 & female == 1)
wilcox.test(revenue_after ~ test_coupon, data = ABtest.signif)

#2: cart = 1, female = 1, channel_acq = Instragram
ABtest.signif <- subset(ABtest, channel_acq == "Instragram" & shopping_cart == 1 & female == 1)
wilcox.test(revenue_after ~ test_coupon, data = ABtest.signif)

#3: cart = 1, female = 0, channel_acq = Referral
ABtest.signif <- subset(ABtest, channel_acq == "Referral" & shopping_cart == 1 & female == 0)
wilcox.test(revenue_after ~ test_coupon, data = ABtest.signif)

#4: cart = 1, female = 1, channel_acq = Referral
ABtest.signif <- subset(ABtest, channel_acq == "Referral" & shopping_cart == 1 & female == 1)
wilcox.test(revenue_after ~ test_coupon, data = ABtest.signif)

#5: cart = 1, female = 0, channel_acq = Other
ABtest.signif <- subset(ABtest, channel_acq == "Other" & shopping_cart == 1 & female == 0)
wilcox.test(revenue_after ~ test_coupon, data = ABtest.signif)
