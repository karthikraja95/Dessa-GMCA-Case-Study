####################################################################
#Dessa Case Competition - Descriptive Stats and Logistic Regression                                
####################################################################
library(dplyr)
library(ggplot2)
library(readr)
library(boot)

#####DESSA CASE COMP####
raw_data <- read.csv("~/Downloads/kickstarter-projects/data_dessa.csv")

str(raw_data)

#Variables
raw_data$ID <- as.factor(raw_data$ID)
raw_data$category <- as.factor(raw_data$category)
raw_data$main_category <- as.factor(raw_data$main_category)
raw_data$deadline <- as.Date(raw_data$deadline,format = "%m/%d/%Y")
raw_data$launched <- as.Date(raw_data$launched,format = "%m/%d/%Y")
raw_data$state <- as.factor(raw_data$state)
raw_data$backers <- as.numeric(raw_data$backers)
raw_data$country <- as.factor(raw_data$country)
raw_data$usd_pledged <- as.numeric(raw_data$usd_pledged)
raw_data$usd_pledged_real <- as.numeric(raw_data$usd_pledged_real)
raw_data$usd_goal_real <- as.numeric(raw_data$usd_goal_real)

####ORGANIZATION####

#Include useful variables only
raw_data <- raw_data %>%
  select(category,
         main_category,
         deadline,
         launched,
         backers,
         state,
         usd_pledged_real,
         usd_goal_real)

#Only inlcude successful or failed projects
raw_data1 <- raw_data %>% 
  filter(state == "failed" | state == "successful")

#Check for missing values
sum(is.na(raw_data1)) #no missing data in dataset

summary(raw_data1)

#Create new variable - duration: number of days between the deadline and launched date
raw_data1$duration <- as.numeric(raw_data1$deadline - raw_data1$launched)
summary(raw_data1$duration)

##STATS####
##Proportion of projects in each category
prop.category <- raw_data1 %>%
  group_by(main_category) %>%
  summarize(count = n()) %>%
  mutate(freq = count/ sum(count)*100) %>%
  arrange(desc(count))

ggplot(data=prop.category, aes(reorder(main_category, -count), count)) +
  geom_bar(stat="identity")

#Proportion of projects in each subcategory
prop.subcategory <- raw_data1 %>%
  group_by(category) %>%
  summarize(count=n()) %>%
  mutate(freq = count/ sum(count)*100) %>%
  arrange(desc(count))

ggplot(data=prop.subcategory, aes(reorder(category, -count), count)) +
  geom_bar(stat="identity")

#Success and failure for each category
prop.suc.fail<- raw_data1 %>%
  group_by(main_category, state) %>%
  summarize(count=n()) %>%
  mutate(freq = count/ sum(count)*100) %>%
  arrange(desc(count))

##Values for table
#Median goal amount for successful and unsuccessful projects
data_success <- raw_data1 %>%
  filter (state == "successful")

data_fail <- raw_data1 %>%
  filter (state == "failed")
#Within the music industry
data_success_m <- raw_data1 %>%
  filter (state == "successful") %>%
  filter(main_category == "Music")

data_fail_m <- raw_data1 %>%
  filter (state == "failed") %>%
  filter(main_category == "Music")

summary(data_fail_m$duration)

#Types of products being funded
#(focus on amount pledged - more pledged = increased popularity)#
top.pledged <- head(arrange(raw_data1,desc(usd_pledged_real)), n = 15) #top 15 projects with the highest pledged values

quantile(raw_data1$usd_pledged_real, probs = seq(from = 0, to = 1, by = .2)) #quantile distribution of the usd_pledged_real variable

#which projects get the most funding, by category
total.pledged.category <- raw_data1 %>%
  group_by(main_category) %>%
  summarize(total=sum(usd_pledged_real)) %>%
  arrange(desc(total))

ggplot(data=total.pledged.category, aes(reorder(main_category, -total), total)) +
  geom_bar(stat="identity")

#which projects get the most funding, by subcategory
total.pledged.subcategory <- raw_data1 %>%
  group_by(main_category) %>%
  summarize(total=sum(usd_pledged_real)) %>%
  arrange(desc(total))

#average amount pledged per backer, by category
avg.pledged.category <- raw_data1 %>%
  group_by(main_category) %>%
  summarize(total.pledged=sum(usd_pledged_real), backers=sum(backers)) %>%
  mutate(avg.pledged.backer = total.pledged/backers) %>%
  arrange(desc(avg.pledged.backer))

ggplot(data=avg.pledged.category, aes(main_category, avg.pledged.backer)) +
  geom_bar(stat="identity")

#by success/failure
avg.pledged.category.status <- raw_data1 %>%
group_by(main_category, state) %>%
  summarize(total.pledged=sum(usd_pledged_real), backers=sum(backers)) %>%
  mutate(avg.pledged.backer = total.pledged/backers) %>%
  arrange(desc(avg.pledged.backer))

#Distribution of pledged, by main_category (taken from keggle R code: https://www.kaggle.com/andrewjmah/kickstarter-exploratory-data-analysis-with-r/code)
ggplot(raw_data1, aes(main_category, usd_pledged_real, fill=main_category)) + geom_boxplot() + 
  ggtitle("Amount Pledged vs. Project Category") + xlab("Project Category") + 
  ylab("Amount Pledged (USD)") + 
  theme(plot.title=element_text(size=15, face="bold", hjust=0.5), 
        axis.title=element_text(size=12, face="bold"), 
        axis.text.x=element_text(size=12, angle=90), legend.position="null") + 
  coord_cartesian(ylim=c(0,20000))

#Success/failure and duration 
duration.success.category <- raw_data1 %>%
  filter(duration <= 60) %>%
  group_by(duration, state) %>%
  summarize(count=n()) %>%
  mutate(freq=count/sum(count)*100)

#(ggplot2 code taken from keggle R code: https://www.kaggle.com/andrewjmah/kickstarter-exploratory-data-analysis-with-r/code)
ggplot(duration.success.category[duration.success.category$state=="successful",], aes(duration, freq)) + 
  geom_point(colour="royalblue4", size=2.5) + ggtitle("Success Rate vs. Project Duration") + 
  xlab("Project Duration (Days)") + ylab("Success Rate (%)") + 
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60)) + geom_vline(xintercept=30, colour="red") + 
  theme_minimal() + 
  theme(plot.title=element_text(hjust=0.5), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#####CORRELATION####
#Cont. variables
raw_data_corr <- raw_data1 %>%
  select(usd_pledged_real, usd_goal_real, duration, backers, duration)

cor(raw_data_corr, method = "pearson")
#none are correlated -- include all cont. variables in the model.

####LOGISTIC REGRESSION MODEL####
#Wanted to do bootstraping - but don't have enough processing power for that - creating test and validation from the original dataset instead. 
#Test = 70%, validate = 30%
set.seed(12)
spec <- c(test = .7, validate = .3)
sam <- sample(cut(seq(nrow(raw_data1)), nrow(raw_data1)*cumsum(c(0,spec)), labels = names(spec)))

res = split(raw_data1, sam)
test.data <- res$test
validation.data <- res$validate

#Logistic regression
model_1 <- glm(state ~ main_category + usd_goal_real + usd_pledged_real + backers + duration, data = test.data, family = "binomial")
summary(model_1)
exp(model_1$coefficients)

#Goodness of fit - classfication table
fit1 <- predict(model_1, newdata =test.data, type = "response")
prediction1.1 <- ifelse(fit1 > 0.7, 1, 0)
summary(prediction1.1)

#table
table(test.data$state, prediction1.1, dnn = c("Observed", "Predicted"))

#Goodness of fit - ROC
library(pROC)
roc(test.data$state == "successful", model_1$fitted.values,plot=T,print.thres="best")

#validation model
fit2_val <- predict(model_1, validation.data, type = "response")
prediction2.1 <- ifelse(fit2_val > 0.7, 1, 0)

table(validation.data$state, prediction2.1, dnn = c("Observed", "Predicted"))

roc(validation.data$state == "successful", prediction2.1,plot=T,print.thres="best")