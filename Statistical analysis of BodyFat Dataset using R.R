#import library
library("readxl")
#Let me load data into Rstudio
body_data <- read_excel("D:\\stirling\\maani\\a2\\3070654BodyFatData.xlsx")
#printing data
body_data
#it has 252 rows with 2 variables.

#check the header of data
head(body_data)

#check the summary of data set
summary(body_data)


#summary(body_data$BodyFat)
#summary(body_data$Abdomen)

#plot the histogram of dataset
hist.default(body_data$BodyFat,col='gray')
#checking second histogram
hist.default(body_data$Abdomen,col='red')

#plot all the dataset
plot.ts(body_data)


#t-test of on variable
set.seed(0)
t_test_body_fat <- body_data$BodyFat
t.test(t_test_body_fat, mu = 39000)
#t-test of other variable
t_test_Abdomen <- body_data$Abdomen
t.test(t_test_Abdomen, mu = 39000)


#Code for Question-2
#Correlation
cor.test(body_data$BodyFat, body_data$Abdomen, method = "pearson")
#plot it
library(ggstatsplot)
ggscatterstats(data = body_data, x = BodyFat, y = Abdomen)
#second_plot
library(corrplot)
corrplot(cor(body_data),method = "number",type = "lower")

#code for Question 3:
#Scatter Plot for both variables
ggplot(body_data, aes(x = BodyFat, y = Abdomen)) +
  geom_point() +
  stat_smooth()

#Correlation of both variables
cor(body_data$BodyFat, body_data$Abdomen)

#Model Implementation
linear_model <- lm(Abdomen ~ BodyFat, data = body_data)
linear_model

#Implement regression
ggplot(body_data, aes(x = BodyFat, y = Abdomen)) +
  geom_point() +
  stat_smooth(method = lm)

#Model Assessment
summary(linear_model)


confint(linear_model)

sigma(linear_model)*100/mean(body_data$Abdomen)

