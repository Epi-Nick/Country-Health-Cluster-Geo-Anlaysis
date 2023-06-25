########################################
#Setting working directory instructions#
########################################
#Before running the following code it is important to first set the working directory to the folder where the original
#data file is stored. This can be done in the following ways:
#Navigate to: Session > Set Working Directory > Choose Directory
#Alternatively you can press: Ctrl + Shift + H
#Or you can type the code: setwd("") and type the folder directory into the quotation marks

##############################################
#Installing and calling all relevant packages#
############################################## 
if (!("tidyverse" %in% installed.packages())) {
  install.packages("tidyverse")}
library(tidyverse)
if (!("dplyr" %in% installed.packages())) {
  install.packages("dplyr")}
library(dplyr)
if (!("grid" %in% installed.packages())) {
  install.packages("grid")}
library(grid)
if (!("gridExtra" %in% installed.packages())) {
  install.packages("gridExtra")}
if (!("corrplot" %in% installed.packages())) {
  install.packages("corrplot")}
library(corrplot)
if (!("tidyr" %in% installed.packages())) {
  install.packages("tidyr")}
library(tidyr)
if (!("ggplot2" %in% installed.packages())) {
  install.packages("ggplot2")}
library(ggplot2)
if (!("RColorBrewer" %in% installed.packages())) {
  install.packages("RColorBrewer")}
library(RColorBrewer)
if (!("factoextra" %in% installed.packages())) {
  install.packages("factoextra")}
library(factoextra)
if (!("cluster" %in% installed.packages())) {
  install.packages("cluster")}
library(cluster)
if (!("dendextend" %in% installed.packages())) {
  install.packages("dendextend")}
library(dendextend)
if (!("mice" %in% installed.packages())) {
  install.packages("mice")}
library(mice)
if (!("rworldmap" %in% installed.packages())) {
  install.packages("rworldmap")}
library(rworldmap)
if (!("mlbench" %in% installed.packages())) {
  install.packages("mlbench")}
library(mlbench)
if (!("rpart" %in% installed.packages())) {
  install.packages("rpart")}
library(rpart)
if (!("randomForest" %in% installed.packages())) {
  install.packages("randomForest")}
library(randomForest)
if (!("e1071" %in% installed.packages())) {
  install.packages("e1071")}
library(e1071)
if (!("caTools" %in% installed.packages())) {
  install.packages("caTools")}
library(caTools)
if (!("caret" %in% installed.packages())) {
  install.packages("caret")}
library(caret)
if (!("lattice" %in% installed.packages())) {
  install.packages("lattice")}
library(lattice)
if (!("missForest" %in% installed.packages())) {
  install.packages("missForest")}
library(missForest)
if (!("VIM" %in% installed.packages())) {
  install.packages("VIM")}
library(VIM)
if (!("rpart.plot" %in% installed.packages())) {
  install.packages("rpart.plot")}
library(rpart.plot)
if (!("margins" %in% installed.packages())) {
  install.packages("margins")}
library(margins)
if (!("dismo" %in% installed.packages())) {
  install.packages("dismo")}
library(dismo)
if (!("gbm" %in% installed.packages())) {
  install.packages("gbm")}
library(gbm)

###########
### EDA ###
###########
data <- read.csv("health_data.csv")                        #Imports Data Set from Working Directory
dim(data)                                                  #Check dimensionality
head(data)                                                 #View data
### Imputing the NA values.
df_mice <- mice(data, m=5, maxit=50, meth='pmm', seed=500) #Create data frame for imputed values (replacement of missing values using mice function)
data <- complete(df_mice, 1)                               #Combine data set with imputed values to complete the data.
attach(data)                                               #Attach Data Set to be recognized by R in further code
#
table(data$Country)
### The Number of records per country is the same for every country, 84 records.
table(data$Sex)
### There are the same number of male and female records (8400).
table(data$Year)
### There are 400 records for each year in the period 1975-2016.

##########################################
### Examination of Variables in detail ###
##########################################

##############################
### Children               ###
##############################
### Exploring the Mean BMI in Children
range(data$Mean_BMI_children)
mean(data$Mean_BMI_children)
median(data$Mean_BMI_children)
ggplot(data = data, aes(x=factor(0),y=Mean_BMI_children)) + geom_boxplot() +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
summary(data$Mean_BMI_children)
ggplot(data = data, aes(x=Mean_BMI_children)) + geom_histogram(binwidth=0.1)
ggplot(data = data, aes(x=Mean_BMI_children)) + geom_histogram(binwidth=0.5)
### The data is normally distributed as it is shown in the plot.
### The peak values are between 18 and 20 BMI.
### The left side of the distribution descends drastically compared to the right side that descends stepwise.
### From the boxplot we can appreciate there are some outliers in both sides.

### Exploring the Prevalence of Obesity in Children
range(data$Prevalence_obesity_children)
mean(data$Prevalence_obesity_children)
median(data$Prevalence_obesity_children)
ggplot(data = data, aes(x=factor(0),y=Prevalence_obesity_children)) + geom_boxplot() +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
summary(data$Prevalence_obesity_children)
ggplot(data = data, aes(x=Prevalence_obesity_children)) + geom_histogram(binwidth=0.1)
ggplot(data = data, aes(x=Prevalence_obesity_children)) + geom_histogram(binwidth=0.05)
### The data is right-skewed.
### The peak-value is around 0.0.
### From the boxplot we can appreciate there are some maximum outliers.


### Exploring the Prevalence of Overweight in Children
range(data$Prevalence_overweight_children)
mean(data$Prevalence_overweight_children)
median(data$Prevalence_overweight_children)
ggplot(data = data, aes(x=factor(0),y=Prevalence_overweight_children)) + geom_boxplot() +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
summary(data$Prevalence_overweight_children)
ggplot(data = data, aes(x=Prevalence_overweight_children)) + geom_histogram(binwidth=0.1)
ggplot(data = data, aes(x=Prevalence_overweight_children)) + geom_histogram(binwidth=0.05)
### The data is right-skewed.
### The peak-value is around 0.05.


### Exploring the Prevalence of Underweight in Children
range(data$Prevalence_underweight_children)
mean(data$Prevalence_underweight_children)
median(data$Prevalence_underweight_children)
ggplot(data = data, aes(x=factor(0),y=Prevalence_underweight_children)) + geom_boxplot() +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
summary(data$Prevalence_underweight_children)
ggplot(data = data, aes(x=Prevalence_underweight_children)) + geom_histogram(binwidth=0.1)
ggplot(data = data, aes(x=Prevalence_underweight_children)) + geom_histogram(binwidth=0.05)
### The data is normally distributed as it is shown in the plot.
### The peak-value is around 0.2.




##############################
### Adults                 ###
##############################

### Exploring the Mean BMI in Adults
range(data$Mean_BMI_adults)
mean(data$Mean_BMI_adults)
median(data$Mean_BMI_adults)
ggplot(data = data, aes(x=factor(0),y=Mean_BMI_adults)) + geom_boxplot() +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
summary(data$Mean_BMI_adults)
ggplot(data = data, aes(x=Mean_BMI_adults)) + geom_histogram(binwidth=0.1)
ggplot(data = data, aes(x=Mean_BMI_adults)) + geom_histogram(binwidth=0.5)
### The data is normally distributed as it is shown in the plot.
### The peak values are at 25 BMI.
### The right side of the distribution descends drastically compared to the left side that descends stepwise.
### From the boxplot we can appreciate there are some outliers in both sides.


### Exploring the Prevalence of Obesity in Adults
range(data$Prevalence_obesity_adults)
mean(data$Prevalence_obesity_adults)
median(data$Prevalence_obesity_adults)
ggplot(data = data, aes(x=factor(0),y=Prevalence_obesity_adults)) + geom_boxplot() +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
summary(data$Prevalence_obesity_adults)
ggplot(data = data, aes(x=Prevalence_obesity_adults)) + geom_histogram(binwidth=0.1)
ggplot(data = data, aes(x=Prevalence_obesity_adults)) + geom_histogram(binwidth=0.05)
### The data is right-skewed.
### The peak-value is around 0.0.
### From the boxplot we can appreciate there are some maximum outliers.


### Exploring the Prevalence of Underweight in Adults
range(data$Prevalence_underweight_adults)
mean(data$Prevalence_underweight_adults)
median(data$Prevalence_underweight_adults)
ggplot(data = data, aes(x=factor(0),y=Prevalence_underweight_adults)) + geom_boxplot() +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
summary(data$Prevalence_underweight_adults)
ggplot(data = data, aes(x=Prevalence_underweight_adults)) + geom_histogram(binwidth=0.1)
ggplot(data = data, aes(x=Prevalence_underweight_adults)) + geom_histogram(binwidth=0.05)
### The data is right-skewed.
### The peak-value is between the period of 0.0 and 0.05.


### Exploring the Prevalence of Morbid Obesity in Adults
range(data$Prevalence_morbid_obesity_adults)
mean(data$Prevalence_morbid_obesity_adults)
median(data$Prevalence_morbid_obesity_adults)
ggplot(data = data, aes(x=factor(0),y=Prevalence_morbid_obesity_adults)) + geom_boxplot() +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
summary(data$Prevalence_morbid_obesity_adults)
ggplot(data = data, aes(x=Prevalence_morbid_obesity_adults)) + geom_histogram(binwidth=0.1)
ggplot(data = data, aes(x=Prevalence_morbid_obesity_adults)) + geom_histogram(binwidth=0.05)
length( which( data$Prevalence_morbid_obesity_adults <0.1 ) )
### The peak-value is around 0.0.
### The prevalence of Morbid Obesity is so small in general that the bigger cases are considered as outliers.
### 16579 out of the 16800 (98.68%) cases have a prevalence of morbid obesity smaller than 0.1




##############################
### Diabetes               ###
##############################
### Exploring the Prevalence of Diabetes
range(data$Diabetes_prevalence, na.rm = TRUE)
mean(data$Diabetes_prevalence, na.rm = TRUE)
median(data$Diabetes_prevalence, na.rm = TRUE)
ggplot(data = data, aes(x=factor(0),y=Diabetes_prevalence)) + geom_boxplot() +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
summary(data$Diabetes_prevalence)
ggplot(data = data, aes(x=Diabetes_prevalence)) + geom_histogram(binwidth=0.1)
ggplot(data = data, aes(x=Diabetes_prevalence)) + geom_histogram(binwidth=0.05)
length( which( data$Diabetes_prevalence >=0.05 ) )
### There is a huge peak value between 0.025 and 0.075. Then, it descends stepwise.
### There are 2800 NA values.
### There is a really big difference between the peak bar and the rest of the bars from the histograms.


##############################
### Blood Pressure         ###
##############################
range(data$Systolic_blood_pressure, na.rm = TRUE)
mean(data$Systolic_blood_pressure, na.rm = TRUE)
median(data$Systolic_blood_pressure, na.rm = TRUE)
ggplot(data = data, aes(x=factor(0),y=Systolic_blood_pressure)) + geom_boxplot() +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
summary(data$Systolic_blood_pressure)
ggplot(data = data, aes(x=Systolic_blood_pressure)) + geom_histogram(binwidth=0.1)
ggplot(data = data, aes(x=Systolic_blood_pressure)) + geom_histogram(binwidth=0.5)
### Although in the previous plots there were lots of maximum outliers, this case is different, because there are no maximum outliers, instead there are minimum outliers.
### The data is normally distributed with peak values between 120 and 130 blood pressure.


range(data$Prevalence_raised_blood_pressure, na.rm = TRUE)
mean(data$Prevalence_raised_blood_pressure, na.rm = TRUE)
median(data$Prevalence_raised_blood_pressure, na.rm = TRUE)
ggplot(data = data, aes(x=factor(0),y=Prevalence_raised_blood_pressure)) + geom_boxplot() +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
summary(data$Prevalence_raised_blood_pressure)
ggplot(data = data, aes(x=Prevalence_raised_blood_pressure)) + geom_histogram(binwidth=0.01)
ggplot(data = data, aes(x=Prevalence_raised_blood_pressure)) + geom_histogram(binwidth=0.05)
### The data is normally distributed with peak values between 0.25 and 0.3.
### There are 400 NA values.


##############################
### Years of Education     ###
##############################
range(data$Years_of_education, na.rm = TRUE)
mean(data$Years_of_education, na.rm = TRUE)
median(data$Years_of_education, na.rm = TRUE)
ggplot(data = data, aes(x=factor(0),y=Years_of_education)) + geom_boxplot() +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
summary(data$Years_of_education)
ggplot(data = data, aes(x=Years_of_education)) + geom_histogram(binwidth=0.1)
ggplot(data = data, aes(x=Years_of_education)) + geom_histogram(binwidth=0.5)
### The data is widely expanded between the period 0 and 14 years.
### There are no outliers

##############################
### Urbanisation           ###
##############################
range(data$Urbanisation, na.rm = TRUE)
mean(data$Urbanisation, na.rm = TRUE)
median(data$Urbanisation, na.rm = TRUE)
ggplot(data = data, aes(x=factor(0),y=Urbanisation)) + geom_boxplot() +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
summary(data$Urbanisation)
ggplot(data = data, aes(x=Urbanisation)) + geom_histogram(binwidth=0.01)
ggplot(data = data, aes(x=Urbanisation)) + geom_histogram(binwidth=0.05)
### The data is expanded between the period of 0 and 1 but more populated between the 0.25 and 0.75.


##############################
### Western Diet Score     ###
##############################
range(data$Western_diet_score, na.rm = TRUE)
mean(data$Western_diet_score, na.rm = TRUE)
median(data$Western_diet_score, na.rm = TRUE)
ggplot(data = data, aes(x=factor(0),y=Western_diet_score)) + geom_boxplot() +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
summary(data$Western_diet_score)
ggplot(data = data, aes(x=Western_diet_score)) + geom_histogram(binwidth=0.1)
ggplot(data = data, aes(x=Western_diet_score)) + geom_histogram(binwidth=0.05)
### Big part of the data is negative.
### The peak values are between -1 and 0.
### The data is right-skewed


##############################
### Combining Variables    ###
##############################
male_data <- filter(data, Sex == "Male")
female_data <- filter(data, Sex == "Female")
group_by_year_data <- data %>% group_by(Year) %>% summarise(Children_BMI=mean(Mean_BMI_children), 
                                                            Adults_BMI = mean(Mean_BMI_adults), 
                                                            Diabetes_Prob = mean(Diabetes_prevalence, 
                                                                                 na.rm = TRUE), 
                                                            Raise_Blood_Pressure_Prevalence = mean(Prevalence_raised_blood_pressure, na.rm = TRUE))
group_by_year_prevalence_data <- data %>% group_by(Year) %>% summarise(Children_Underweight=mean(Prevalence_underweight_children), 
                                                                       Children_Overweight = mean(Prevalence_overweight_children), 
                                                                       Children_Obesity = mean(Prevalence_obesity_children, na.rm = TRUE), 
                                                                       Adults_Underweight=mean(Prevalence_underweight_adults), 
                                                                       Adults_Obesity = mean(Prevalence_obesity_adults), 
                                                                       Adults_Morbid_Obesity = mean(Prevalence_morbid_obesity_adults, 
                                                                                                    na.rm = TRUE))
group_by_yearcountry_data <- data %>% group_by(Year, Country) %>% summarise(Children_BMI=mean(Mean_BMI_children), 
                                                                            Adults_BMI = mean(Mean_BMI_adults), 
                                                                            Diabetes_Prob = mean(Diabetes_prevalence, na.rm = TRUE), 
                                                                            Raise_Blood_Pressure_Prevalence = mean(Prevalence_raised_blood_pressure, 
                                                                                                                   na.rm = TRUE))
### Comparing the relationship between the Systolic Blood Pressure and the Prevalence Raised of Blood Pressure.
plot(data$Systolic_blood_pressure, data$Prevalence_raised_blood_pressure, col = "#2E9FDF")
### There is a positive linear relationship between the variables.

### Comparing the prevalence of diabetes between male and females.
plot(male_data$Diabetes_prevalence, female_data$Diabetes_prevalence)
### The relationship between male and female is linear, therefore, the possibility of getting diabetes is more or less the same for both.

### Exploring the evolution of the BMI in Children and Adults across the Years.
plot(group_by_year_data$Year, group_by_year_data$Children_BMI, type="l", ylim = c(17, 27), 
     xlab = "Year", ylab = "BMI")
lines(group_by_year_data$Year, group_by_year_data$Adults_BMI, col="red", lwd=2)
title("Evolution of Children and Adults BMI across years")
legend("topleft",legend=c("Children BMI","Adults BMI"), lwd=c(5,2), col=c("black","red"))
### As the years go by, the BMI has been in continuous growth.

### Exploring the evolution of the prevalence in diabetes and blood pressure across the years. 
plot(group_by_year_data$Year, group_by_year_data$Diabetes_Prob, type="l", ylim = c(0, 0.32), 
     xlab = "Year", ylab = "Prevalence")
lines(group_by_year_data$Year, group_by_year_data$Raise_Blood_Pressure_Prevalence, col="red", lwd=2)
title("Evolution of Diabetes and Raise Blood Pressure Prevalences across years", cex.main=0.82)
legend("topright",legend=c("Diabetes","Raise Blood Pressure"), lwd=c(5,2), col=c("black","red"))
### The Prevalence of diabetes grew across the years while the raise of blood pressure decreased.


#####################################
### Evolution in Weight Condition ###
#####################################
### Exploring the evolution in children weight condition prevalences across years.
plot(group_by_year_prevalence_data$Year, group_by_year_prevalence_data$Children_Underweight, type="l", 
     ylim = c(0, 0.32), xlab = "Year", ylab = "Prevalence")
lines(group_by_year_prevalence_data$Year, group_by_year_prevalence_data$Children_Overweight, col="red", lwd=2)
lines(group_by_year_prevalence_data$Year, group_by_year_prevalence_data$Children_Obesity, col="blue", lwd=2)
title("Evolution of Weight Condition Prevalences in Children across years", cex.main=0.82)
legend("topright",legend=c("Underweight","Overweight", "Obesity"), lwd=c(5,2), col=c("black","red", "blue"))
### The obesity has increased from a 0.01 to a 0.09.
### The overweight, from 0.069 to 0.25.
### On the other hand, the underweight has decreased from 0.27 to 0.17.


### Exploring the evolution in adults weight condition prevalences across years.
plot(group_by_year_prevalence_data$Year, group_by_year_prevalence_data$Adults_Underweight, type="l", 
     ylim = c(0, 0.32), xlab = "Year", ylab = "Prevalence")
lines(group_by_year_prevalence_data$Year, group_by_year_prevalence_data$Adults_Obesity, col="red", lwd=2)
lines(group_by_year_prevalence_data$Year, group_by_year_prevalence_data$Adults_Morbid_Obesity, col="blue", 
      lwd=2)
title("Evolution of Weight Condition Prevalences in Adults across years", cex.main=0.82)
legend("topright",legend=c("Underweight","Obesity", "Morbid Obesity"), lwd=c(5,2), 
       col=c("black","red", "blue"))



#####################################
### Geographical Representations  ###
#####################################
# get map
worldmap <- getMap(resolution = "coarse")
# plot world map
plot(worldmap, col = "lightgrey", 
     fill = T, border = "darkgray",
     xlim = c(-180, 180), ylim = c(-90, 90),
     bg = "aliceblue",
     asp = 1, wrap=c(-180,180))

country_codes <- data %>% group_by(ISO) %>% summarise(Children_BMI=mean(Mean_BMI_children), 
                                                      Adults_BMI=mean(Mean_BMI_adults), 
                                                      Blood_pressure=mean(Systolic_blood_pressure),
                                                      GDP_USD = mean(GDP_USD))
country_codes$Children_BMI <- round(country_codes$Children_BMI, 2)
country_codes$Adults_BMI <- round(country_codes$Adults_BMI, 2)
bmi_map <- joinCountryData2Map(country_codes, 
                               joinCode = "ISO3",
                               nameJoinColumn = "ISO")                                                      

### Representation of the Children BMI around the world
mapCountryData(bmi_map, 
               nameColumnToPlot="Children_BMI",
               oceanCol = "azure2",
               catMethod = "quantiles",
               missingCountryCol = gray(.8),
               colourPalette = c("coral",
                                 "coral2",
                                 "coral3", "orangered", 
                                 "orangered3", "orangered4"),
               addLegend = T,
               mapTitle = "Geographical Representation of Children BMI",
               border = NA)
### In most of Africa and the southern part of Asia, the children BMI is below average.
### On the other hand, in Canada and the Southern part of North-America together with Oceania, the children BMI isabove average.


### Representation of the Adults BMI around the world.
mapCountryData(bmi_map, 
               nameColumnToPlot="Adults_BMI",
               oceanCol = "azure2",
               catMethod = "quantiles",
               missingCountryCol = gray(.8),
               colourPalette = c("coral",
                                 "coral2",
                                 "coral3", "orangered", 
                                 "orangered3", "orangered4"),
               addLegend = T,
               mapTitle = "Geographical Representation of Adults BMI",
               border = NA)
### Not very different from the Children's representation.
### Rusia's BMI is just in the average.
### Oceania's has decreased a bit while in the northern part of Africa has increased.

### Exploring the average Raised Blood Pressure per country
mapCountryData(bmi_map, 
               nameColumnToPlot="Blood_pressure",
               oceanCol = "azure2",
               catMethod = "quantiles",
               missingCountryCol = gray(.8),
               colourPalette = c("coral",
                                 "coral2",
                                 "coral3", "orangered", 
                                 "orangered3", "orangered4"),
               addLegend = T,
               mapTitle = "Geographical Representation of the Raised Blood Pressure",
               border = NA)
### In countries were the Adults BMI were above average, the raised blood pressure is below the average.
### Some examples of these are North-America, Oceania....
### In most African countries is the opposite, high Raised Blood Pressure with Low Adults BMI.

mapCountryData(bmi_map, 
               nameColumnToPlot="GDP_USD",
               oceanCol = "azure2",
               catMethod = "quantiles",
               missingCountryCol = gray(.8),
               colourPalette = c("coral",
                                 "coral2",
                                 "coral3", "orangered", 
                                 "orangered3", "orangered4"),
               addLegend = T,
               mapTitle = "Geographical Representation of the GDP_USD",
               border = NA)

### Those countries with a higher GDP_USD also have a higher BMI.
### To corroborate this, I will check their relationship.
plot(country_codes$GDP_USD, country_codes$Adults_BMI, col = "red")
data2 <- subset(country_codes, select= c(Children_BMI, Adults_BMI, Blood_pressure, GDP_USD))
corrplot(cor(data2), method = "number")
### The relationship is not very strong compared to the Adults BMI, but it is stronger compared to the Children BMI.

######################################
#General Data Cleaning for Clustering#
######################################
df2 <- data                             #Create initial data frame to preserve initial data.
df2$Sex<-ifelse(df2$Sex=="Female",1,0)  #Converting Gender variable into binary. Female = 1 and Male = 0.
df3 <- filter(df2, Sex > 0)             #Splitting data set by Sex (Female)
df4 <- filter(df2, Sex < 1)             #Splitting data set by sex (Male)


#The following clusters are split by Females and Males. Three years were selected: 1975 (beginning of data set), 1996
#(middle of data set) and 2016 (end of data set) to showcase how variables changed over time through cluster analysis.
#Running a cluster with all variables from all recorded time periods would not provide any useful insights as it would
#produce extremely cluttered cluster plots.

#NOTE: To open cluster plots code must be run chunk by chunk as these plots are visualized in pop-up windows and are
#automatically closed by the code before the next cluster plot is opened.

##########################
#Cluster for 1975 Females#
##########################
#Further data cleaning:
df_1975F <- filter(df3, Year <= 1975)           #Create new data frame containing data for Females in 1975.
rownames(df_1975F) <- make.names(df_1975F[,1])  #Change Country variable to row names as all worded variables must be deleted so that data can be scaled.
df_1975F <- df_1975F[,-1]                       #Deletes Country variable so that data can be scaled.
df_1975F <- df_1975F[,-1]                       #Deletes ISO variable so that data can be scaled.
df_1975F <- df_1975F[,-1]                       #Deletes Sex variable as it no longer adds any value to analysis as this is all Female data.
df_1975F <- df_1975F[,-1]                       #Deletes Year variable as it no longer adds any value to analysis as all of this data is from 1975.
df_1975F <- df_1975F[,-12]                      #Deletes Region variable so that data can be scaled.
df_1975F <- df_1975F[,-12]                      #Deletes Superregion variable so that data can be scaled.
df_1975F <- scale(df_1975F)                     #Scale's data allowing different measurement variables to become comparable.
#Running the cluster:
fviz_nbclust(df_1975F, kmeans, method = "silhouette")   #Statistical silhouette method for determining optimal number of clusters.
clust_1975F <-  kmeans(df_1975F, 3, nstart = 25)        #Using kmeans to set number of centers (clusters).
dev.new()                                               #Opens separate window for plot for easier interpretation.
(fviz_cluster(clust_1975F, data = df_1975F)             #Generating a cluster output.
+ ggtitle("Cluster for Females in 1975"))               #Generates a title for the cluster plot.

#There are three interesting clusters for 1975 Female data. The green cluster shows countries with higher rates of 
#obesity/diabetes prevalence. We can see that there are two extreme outliers (Nauru and American Samoa) which exhibit much 
#larger rates of obesity prevalence than all other nations. Countries belonging to the red cluster exhibit high 
#prevalence of underweight citizens. Japan is often cited as the healthiest country (based on weight). So some 
#countries in the blue cluster are leaning to words higher rates of obesity/diabetes prevalence.

########################
#Cluster for 1975 Males#
########################
df_1975M <- filter(df4, Year <= 1975)           #Create new data frame containing data for Males in 1975.
rownames(df_1975M) <- make.names(df_1975M[,1])  #Change Country variable to row names as all worded variables must be deleted so that data can be scaled.
df_1975M <- df_1975M[,-1]                       #Deletes Country variable so that data can be scaled.
df_1975M <- df_1975M[,-1]                       #Deletes ISO variable so that data can be scaled.
df_1975M <- df_1975M[,-1]                       #Deletes Sex variable as it no longer adds any value to analysis as this is all Male data.
df_1975M <- df_1975M[,-1]                       #Deletes Year variable as it no longer adds any value to analysis as all of this data is from 1975.
df_1975M <- df_1975M[,-12]                      #Deletes Region variable so that data can be scaled.
df_1975M <- df_1975M[,-12]                      #Deletes Superregion variable so that data can be scaled.
df_1975M <- scale(df_1975M)                     #Scale's data allowing different measurement variables to become comparable.
#Running the cluster:
dev.off()                                               #Prevents silhouette plot from over riding cluster plot window if all code is run (closes previous plot window).
fviz_nbclust(df_1975M, kmeans, method = "silhouette")   #Statistical silhouette method for determining optimal number of clusters.
clust_1975M <-  kmeans(df_1975M, 2, nstart = 25)        #Using kmeans to set number of centers (clusters).
dev.new()                                               #Opens separate window for plot for easier interpretation.
(fviz_cluster(clust_1975M, data = df_1975M)             #Generating a cluster output.
+ ggtitle("Cluster for Males in 1975"))                 #Generates a title for the cluster plot.

#Male 1975 cluster plot takes a similar form to the Female cluster analysis. However, two clusters are more appropriate
#here as there is less space between data points within the red cluster. We can see that countries on the extreme ends
#stayed relatively the same. However, countries deeper in the clusters have changed places in a lot of cases. This may
#be due to lifestyle differences between genders.

##########################
#Cluster for 1996 Females#
##########################
#Further data cleaning:
df_1996F <- filter(df3, Year > 1995 & Year <1997) #Create new data frame containing data for Females in 1996.
rownames(df_1996F) <- make.names(df_1996F[,1])    #Change Country variable to row names as all worded variables must be deleted so that data can be scaled.
df_1996F <- df_1996F[,-1]                         #Deletes Country variable so that data can be scaled.
df_1996F <- df_1996F[,-1]                         #Deletes ISO variable so that data can be scaled.
df_1996F <- df_1996F[,-1]                         #Deletes Sex variable as it no longer adds any value to analysis as this is all Female data.
df_1996F <- df_1996F[,-1]                         #Deletes Year variable as it no longer adds any value to analysis as all of this data is from 1996.
df_1996F <- df_1996F[,-12]                        #Deletes Region variable so that data can be scaled.
df_1996F <- df_1996F[,-12]                        #Deletes Superregion variable so that data can be scaled.
df_1996F <- scale(df_1996F)                       #Scale's data allowing different measurement variables to become comparable.
#Running the cluster:
dev.off()                                               #Prevents silhouette plot from over riding cluster plot window if all code is run (closes previous plot window).
fviz_nbclust(df_1996F, kmeans, method = "silhouette")   #Statistical silhouette method for determining optimal number of clusters.
clust_1996F <-  kmeans(df_1996F, 3, nstart = 25)        #Using kmeans to set number of centers (clusters).
dev.new()                                               #Opens separate window for plot for easier interpretation.
(fviz_cluster(clust_1996F, data = df_1996F)             #Generating a cluster output.
+ ggtitle("Cluster for Females in 1996"))               #Generates a title for the cluster plot.

#1996 Female cluster analysis shows some substantial changes have occurred to obesity/diabetes prevalence within the 
#nations from our data set. Far more countries now belong to the middle cluster as rates of underweight citizens have
#dropped. American Samoa has climbed a lot closer to Nauru suggesting massive increases in obesity/diabetes prevalence.
#The United States has entered the far right cluster suggesting problematic rates of obesity/diabetes. Japan has moved
#to the left edge of the middle cluster. Since Japan is considered the "ideal" in terms of obesity/diabetes prevalence,
#most other countries in the middle cluster are likely moving towards undesired rates of obesity/diabetes.

########################
#Cluster for 1996 Males#
########################
df_1996M <- filter(df4, Year > 1995 & Year <1997) #Create new data frame containing data for Males in 1996.
rownames(df_1996M) <- make.names(df_1996M[,1])    #Change Country variable to row names as all worded variables must be deleted so that data can be scaled.
df_1996M <- df_1996M[,-1]                         #Deletes Country variable so that data can be scaled.
df_1996M <- df_1996M[,-1]                         #Deletes ISO variable so that data can be scaled.
df_1996M <- df_1996M[,-1]                         #Deletes Sex variable as it no longer adds any value to analysis as this is all Male data.
df_1996M <- df_1996M[,-1]                         #Deletes Year variable as it no longer adds any value to analysis as all of this data is from 1996.
df_1996M <- df_1996M[,-12]                        #Deletes Region variable so that data can be scaled.
df_1996M <- df_1996M[,-12]                        #Deletes Superregion variable so that data can be scaled.
df_1996M <- scale(df_1996M)                       #Scale's data allowing different measurement variables to become comparable.
#Running the cluster:
dev.off()                                               #Prevents silhouette plot from over riding cluster plot window if all code is run (closes previous plot window).
fviz_nbclust(df_1996M, kmeans, method = "silhouette")   #Statistical silhouette method for determining optimal number of clusters.
clust_1996M <-  kmeans(df_1996M, 2, nstart = 25)        #Using kmeans to set number of centers (clusters).
dev.new()                                               #Opens separate window for plot for easier interpretation.
(fviz_cluster(clust_1996M, data = df_1996M)             #Generating a cluster output.
+ ggtitle("Cluster for Males in 1996"))                 #Generates a title for the cluster plot.

#Interestingly, cluster plot for 1996 Males appears to be flipped upside down. For example, Nauru and American Samoa
#changed from their usual top right position to bottom right. It is fairly difficult to determine the cause of this 
#change by looking solely at the cluster plot as the axis have been transformed through Principal Component analysis
#to represent as many variables as possible. However, by looking at the data for Nauru:
df_1996Nauru <- df2[10375:10376,] #Creates data frame for Nauru data in 1996.
view(df_1996Nauru)                #Opens a tab showing the contents of the data frame.
#We can see obesity and diabetes prevalence in Males is lower. However, systolic blood pressure and raised blood
#pressure prevalence are higher in Males.

##########################
#Cluster for 2016 Females#
##########################
#Further data cleaning:
df_2016F <- filter(df3, Year > 2015)            #Create new data frame containing data for Females in 2016.
rownames(df_2016F) <- make.names(df_2016F[,1])  #Change Country variable to row names as all worded variables must be deleted so that data can be scaled.
df_2016F <- df_2016F[,-1]                       #Deletes Country variable so that data can be scaled.
df_2016F <- df_2016F[,-1]                       #Deletes ISO variable so that data can be scaled.
df_2016F <- df_2016F[,-1]                       #Deletes Sex variable as it no longer adds any value to analysis as this is all Female data.
df_2016F <- df_2016F[,-1]                       #Deletes Year variable as it no longer adds any value to analysis as all of this data is from 2016.
df_2016F <- df_2016F[,-12]                      #Deletes Region variable so that data can be scaled.
df_2016F <- df_2016F[,-12]                      #Deletes Superregion variable so that data can be scaled.
df_2016F <- scale(df_2016F)                     #Scale's data allowing different measurement variables to become comparable.
#Running the cluster:
dev.off()                                               #Prevents silhouette plot from over riding cluster plot window if all code is run (closes previous plot window).
fviz_nbclust(df_2016F, kmeans, method = "silhouette")   #Statistical silhouette method for determining optimal number of clusters.
clust_2016F <-  kmeans(df_2016F, 3, nstart = 25)        #Using kmeans to set number of centers (clusters).
dev.new()                                               #Opens separate window for plot for easier interpretation.
(fviz_cluster(clust_2016F, data = df_2016F)             #Generating a cluster output.
+ ggtitle("Cluster for Females in 2016"))               #Generates a title for the cluster plot.

#Cluster plot for 2016 Females shows extreme changes to obesity/diabetes prevalence in comparison to 1975 and 1996.
#Japan now belongs to the left cluster and is now position closely to North Korea. This potentially suggest some mis-
#reporting has occurred for North Korea's data as they actually have a much larger prevalence of underweight adults and
#children due to severe malnutrition. Nauru and American Samoa are no longer massive outliers as they have been joined
#by other nations with increased levels of diabetes/obesity prevalence. Most countries now fall within the middle 
#cluster suggesting prevalence of obesity/diabetes is generally rising.


########################
#Cluster for 2016 Males#
########################
df_2016M <- filter(df4, Year > 2015) #Create new data frame containing data for Males in 2016.
rownames(df_2016M) <- make.names(df_2016M[,1])    #Change Country variable to row names as all worded variables must be deleted so that data can be scaled.
df_2016M <- df_2016M[,-1]                         #Deletes Country variable so that data can be scaled.
df_2016M <- df_2016M[,-1]                         #Deletes ISO variable so that data can be scaled.
df_2016M <- df_2016M[,-1]                         #Deletes Sex variable as it no longer adds any value to analysis as this is all Male data.
df_2016M <- df_2016M[,-1]                         #Deletes Year variable as it no longer adds any value to analysis as all of this data is from 2016M.
df_2016M <- df_2016M[,-12]                        #Deletes Region variable so that data can be scaled.
df_2016M <- df_2016M[,-12]                        #Deletes Superregion variable so that data can be scaled.
df_2016M <- scale(df_2016M)                       #Scale's data allowing different measurement variables to become comparable.
#Running the cluster:
dev.off()                                               #Prevents silhouette plot from over riding cluster plot window if all code is run (closes previous plot window).
fviz_nbclust(df_2016M, kmeans, method = "silhouette")   #Statistical silhouette method for determining optimal number of clusters.
clust_2016M <-  kmeans(df_2016M, 2, nstart = 25)        #Using kmeans to set number of centers (clusters).
dev.new()                                               #Opens separate window for plot for easier interpretation.
(fviz_cluster(clust_2016M, data = df_2016M)             #Generating a cluster output.
+ ggtitle("Cluster for Males in 2016"))                 #Generates a title for the cluster plot.
########
dev.off()                                               #Closes final cluster plot window.

#Cluster plot for 2016 Males is substantially different. United States Males are shown to have higher obesity/diabetes
#prevalence than Females and the same can be said for most countries including Japan. This may be due to differing 
#societal pressures on body image faced by women all other the world. 


################
#Classification#
################

#import the data 
Datafile<-read.csv("IntOrg_NCD_variables_2022_02_02.csv") #Use function read.csv to allow R to read the data file where it is saved in your desktop
attach(Datafile)                                          #Attach data file
dim(Datafile)                                             #dim function outputs how much data is in the file as well as how many variables are in it 
summary(Datafile)                                         #Summary function produces summaries of the results of various model fitting functions from the data 

shuffle_index <- sample(1:nrow(Datafile))                 #This code will generate a random list of index from 1 to the maximum number of rows  
head(shuffle_index)                                       #This outputs the random list of numbers  

df <- Datafile[shuffle_index, ]                           #Using this index will shuffle the data set                                         
head(df)                                                  #This outputs the random data from the file 

#clean the data
library(dplyr)                                            #Load the dplyr package into r 
df$Prevalence_morbid_obesity_adults <- NULL               #This next step cleans the data, firstly need to drop any data which is not needed, this code will drop the morbid obesity in adults data as it is not needed due to other similar variables already 
df$ISO <- NULL                                            #Next the ISO set will be dropped due to being irrelevant
df$Region <- NULL                                         #Next the Region set will be dropped due to being irrelevant
df$Prevalence_overweight_children <- NULL                 #Next the overweight children set will be dropped due to being irrelevant
df$Prevalence_raised_blood_pressure <- NULL               #Next the raised blood pressure set will be dropped due to being irrelevant
df$Years_of_education <- NULL                             #Next the years of education set will be dropped due to being irrelevant
df$GDP_USD <- NULL                                        #Next the GDP_USD set will be dropped due to being irrelevant
df$Western_diet_score <- NULL                             #Next the western diet score set will be dropped due to being irrelevant
df$Urbanisation <- NULL                                   #Next the urbanisation set will be dropped due to being irrelevant
df$Country <- NULL                                        #Next the country set will be dropped due to being irrelevant
df_mice <- mice(df, m=5, maxit=50, meth='pmm', seed=500)  #Create data frame for imputed values (replacement of missing values using mice function)
df <- complete(df_mice, 1)                                #Combine data set with imputed values to complete the data.
attach(df)                                                #Re-attach the completed data frame.  
data_frame <- df                                          #Create new data frame to preserve data.

#create the train/test set
set.seed(123)                                                                     #Set random seed number       
sample<-sample.split(data_frame, SplitRatio = 0.7)                                #This first code is used to split the data into 70% and 30%
train <- subset(data_frame, sample== TRUE)                                        #70% is put into the train data to test and run
test <- subset(data_frame, sample== FALSE)                                        #30% is put into the test data to predict later on
dim(train)                                                                        #Returns the number of elements in the data
dim(test)                                                                         #Returns the number of elements in the data


#build the model
library(rpart.plot)                                                               #Load the package rpart.plot into r
fit <- rpart(Sex~., data = train, method = 'class')                               #The rpart function fits the model by formulating a decision tree using the data              
rpart.plot(fit, box.palette = "blue", extra = "auto")                             #This plots the tree in blue and auto allows it to pick the best model type 

#make a prediction
predict_unseen <-predict(fit, test, type = "class")                               #This code predicts the class(0/1) of the test set 


table_mat <- table(test$Sex, predict_unseen)                                      #Creates a table to count the predicted values versus the actual values from the decision tree 
table_mat                                                                         #The model correctly predicted 2301 as female but misclassified 254 as males when they were meant to be females 

#measure performance
accuracy_test <-sum(diag(table_mat))                                              #The first gives a sum of the diagonal
sum(table_mat)                                                                    #Sum of the matrix 
print(paste('Accuracy for test', accuracy_test))                                  #Gives us a accuracy for test score 

#tune the hyper-parameters
accuracy_tune <- function(fit) {                                                  #This function is run as a whole so highlight lines 82 and 87 and run altogether, this function displays the accuracy 
  predict_unseen <- predict(fit, test, type = 'class')
  table_mat <- table(test$Sex, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}


control <- rpart.control(minsplit = 4, minbucket = round(5/3),                    #This tunes the parameters and sees if you can improve the model over the default value  
                         maxdepth = 3, cp = 0)
tune_fit <- rpart(Sex~., data = train, method = "class", control = control)       
accuracy_tune(tune_fit)                                                           #This outputs the accuracy data 

#clean the data and turn binary
data_frame$Sex <-ifelse(data_frame$Sex=='Female',1,0)                             #Make the sex variable in the data binary 
data_frame$Year <- NULL                                                           #Get rid of the year variable as it is irrelevant 
data_frame$Superregion <- NULL                                                    #Get rid of the superregion variable as it is irrelevant

#create the train/test set 
sample<-sample.split(data_frame, SplitRatio = 0.7)                                #This first code is used to split the data into 70% and 30%
train.binary <- subset(data_frame, sample== TRUE)                                 #70% is put into the train data to test and run
test.binary <- subset(data_frame, sample== FALSE)                                 #30% is put into the test data to predict later on
dim(train.binary)                                                                 #Returns the number of elements in the data
dim(test.binary)                                                                  #Returns the number of elements in the data


#build GBM model
set.seed(123)                                                                     #Set the random seed     
gbm_model <- gbm(formula = Sex~., distribution = "bernoulli", data = train.binary, n.trees = 1000, shrinkage = 0.10, interaction.depth = 5, cv.folds = 3)      #This code makes a gradient boosting model GBM, which combines the predictions from multiple decision trees to generate the final prediction                                                       
print(gbm_model)                                                                  #Prints how many models were made and found       
summary(gbm_model)                                                                #From the results you can see the obesity in adults had the greatest relative influence in predicting whether an individual was male or female, with a graphic representation of the results as well 

#hyperparameter tunings 
optimal_cv <- gbm.perf(gbm_model, method = "cv")                                  #This fits a graph to show the inflection point where performance on the test dataset starts to decrease  
print(optimal_cv)                                                                 #Tells us optimal number of trees 

#model results
resultpredict <- predict(object = gbm_model, n.trees = optimal_cv, type = "response") #To view our model results

predictionresults <- as.factor(ifelse(resultpredict>0.80,1,0))                    #Used to create prediction
train.binary$Sex <- as.factor(train.binary$Sex)                                   #Used to create prediction results of the sex
confusionMatrix(predictionresults, train.binary$Sex)                              #Use this to test the models accuracy, this shows a high accuracy rate suggesting the model is good at predicting the sex 




