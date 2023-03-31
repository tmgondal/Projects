#libraries
library(ggplot2)
#for ploting
library(moments)
#for correlation
library(ggcorrplot)
#for correlation
library(corrplot)
#for correlation
library(Metrics)
#plotting
library(reshape2)
#plotting
library(ggpubr)
#Import Data
dataset <- read.csv(file="D:\\stirling\\but\\a3\\L_E_D.csv")
#Print Data
dataset
#lets checkout the dimensions
dim(dataset)
#Data Cleaning, as per instructions, dataset needs to be cleaned
#In this dataset, country name and year are two variables which are not usefull.
#Removing those"
dataset <- subset(dataset, select = -c(Country, Year))
dataset
#here I can check that 20 variables are left out of 22 variables.
#Lets deal with the missing values
missing.rows = dim(dataset)[1] -  dim(na.omit(dataset))[1]
#lets check the missing row
missing.rows
#lets plot those
missing <- data.frame(type=c("missing values of rows", "non-missing values of rows") ,count = c(missing.rows,  dim(na.omit(dataset))[1]))
set_plot_dimensions(4,16)
ggplot(missing, aes(fill=type, y="", x=count)) + #selecting graph type
  geom_bar(position="stack", stat="identity")+ #selecting position
  ggtitle("Comparison of Missing and Non Missing Values in the Dataset") + #labeling data
  xlab("Missing rows count") + ylab("Range") + #labling axes
  scale_fill_brewer(palette="Set3")
#let us plot the missing count as well
missing_count_or_rows <- data.frame(feature = factor(names(dataset)),counts=sapply(dataset, function(x) sum(is.na(x))))
#New plot_setting
set_plot_dimensions(18,8)
#adjusting ggplot values
ggplot(missing_count_or_rows,aes(x=reorder(feature, -counts), y=counts, fill=counts)) +  geom_bar(stat="identity") +  ggtitle("Missing values counts in each feature of dataset") +  xlab("Feature Name") + ylab("Missing values count") +
  theme(axis.text.x=element_text(angle=75, hjust=1))+  theme(text = element_text(size = 12))+  scale_fill_continuous(trans = 'sqrt')
#Life Expextancy
hist(dataset$Life.expectancy,main = "Life Expectance: Distribution Chart",xlab = "Life Expectancy in (yrs)")
plot(density(dataset$Life.expectancy), main = "Distribution: Life Expectancy",xlab = "Life Expectancy in (yrs)")
abline(v=mean(dataset$Life.expectancy))
#setting up plots
set_plot_dimensions(16,4)
#adjusting ggplot
ggplot(dataset ,aes(x= Status,y=Life.expectancy, fill= Status)) +   geom_boxplot() +  ggtitle("Life expectancy per country Status")+
  theme(text = element_text(size = 14))+  scale_fill_brewer(palette="Set2")
#setting up plot
set_plot_dimensions(14,10)
corr_a <- round(cor(subset(dataset, select =-c(Status))), 4)
ggcorrplot(corr_a,type = "upper", lab = FALSE, outline.color = "black", lab_size = 3, legend.title = "Correlation")+
  ggtitle("Correlation Matrix")
mod.linear <- lm(Life.expectancy~ ., data = subset(dataset, select =-c(Status)))
vifs <- data.frame(vif(mod.linear))
set_plot_dimensions(16,8)
ggplot(vifs, aes(y=vif.mod.linear., x=row.names(vifs))) + 
  geom_bar(aes(fill=vif.mod.linear.>5),stat="identity")+
  scale_y_continuous(trans = "sqrt",  breaks = c(6, 11, 51, 101))+
  geom_hline(yintercept = 6, colour = "red") + 
  ggtitle("VIF per feature") +
  xlab("Featurs Name") + ylab("VIF") +
  theme(axis.text.x=element_text(angle=75, hjust=2))+
  theme(text = element_text(size = 14))+
  scale_fill_brewer(palette="Dark2")
EDA_a <- subset(dataset, select = -c(infant.deaths))
EDA_a <- subset(EDA_a, select = -c(GDP))
EDA_a <- subset(EDA_a, select = -c(thinness..1.19.years))
set_plot_dimensions(16,10)
corr_a <- round(cor(subset(EDA_a, select =-c(Status))), 4)
ggcorrplot(corr_a,type = "upper", lab = FALSE, outline.color = "black", lab_size = 4, legend.title = "Correlation")
mod.linear <- lm(Life.expectancy~ ., data = subset(EDA_a, select =-c(Status)))
vifs <- data.frame(vif(mod.linear))
set_plot_dimensions(16,8)
ggplot(vifs, aes(y=vif.mod.linear., x=row.names(vifs))) + 
  geom_bar(aes(fill=vif.mod.linear.<5),stat="identity")+
  scale_y_continuous(trans = "sqrt",  breaks = c(5, 10, 50, 100))+
  geom_hline(yintercept = 5, colour = "red") + 
  ggtitle("VIF per feature") +
  xlab("Featurs") + ylab("VIF") +
  theme(axis.text.x=element_text(angle=20, hjust=1))+
  theme(text = element_text(size = 18))+
  scale_fill_brewer(palette="Dark2")


regfit.best <- regsubsets(Life.expectancy~., data= EDA_a, nvmax = 16)
reg.summary <- summary(regfit.best)

par(mfrow=c(2,2))

#- residual sum of squares:
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
which.min(reg.summary$rss)
points(16,reg.summary$rss[16], col="red",cex=2,pch=20)

# adjusted-R^2 with its largest value
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted Rsq",type="l")
which.max(reg.summary$adjr2)
points(15,reg.summary$adjr2[15], col="red",cex=2,pch=20)

# Mallow's Cp with its smallest value
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(reg.summary$cp)
points(13,reg.summary$cp[13],col="red",cex=2,pch=20)

# BIC with its smallest value
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
which.min(reg.sumary$bic)
points(12,reg.summary$bic[12],col="red",cex=2,pch=20)


#set seed as 124
set.seed(124)

sample_data <- sample_data(c(TRUE, FALSE), nrow(EDA_a), replace=TRUE, prob=c(0.71,0.29))
train_x <- dataset[sample_data, ]
x.test_a <-dataset[!sample_data, ]
y.test_a <- dataset[!sample_data, ]$Life.expectancy
model_full<- lm(Life.expectancy~., data = train_x)
summary(model_full)


pred <- predict(model_full, newdata=x.test_a)
rmse(pred,y.tes_a)
par(mfrow=c(2,2))
plot(model_full)
#"

confint(model_full, level=0.95)
View(model_full)
