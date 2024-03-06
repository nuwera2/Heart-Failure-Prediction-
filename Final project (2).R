##Loading data
data <- read.csv("heart 2.csv")
head(data)
View(data)
##data shape
dim(data)

##General information
str(data)
##Statistical analysis
summary(data)
#Missing values
colSums(is.na(data))

#Outliers
numvariable <- c("Age", "RestingBP", "Cholesterol", "MaxHR", "Oldpeak")
boxplotdata <- data[, numvariable]
boxplot(boxplotdata, main = "Boxplots of numerical Variables")

qqnorm(data$Age)
qqline(data$Age)

qqnorm(data$RestingBP)
qqline(data$RestingBP)

qqnorm(data$Cholesterol)
qqline(data$Cholesterol)

qqnorm(data$MaxHR)
qqline(data$MaxHR)

qqnorm(data$Oldpeak)
qqline(data$Oldpeak)

newdata <- data

# Iterate over continuous data columns only
for (col in c("Cholesterol", "Oldpeak")) {
  
  # Calculate Q1, Q3 & IQR
  q1 <- quantile(data[[col]], 0.25)
  q3 <- quantile(data[[col]], 0.75)
  IQR <- q3 - q1
  
  # Define the lower bound and upper bound
  lower <- q1 - 1.5 * IQR
  upper <- q3 + 1.5 * IQR
  cat(col, ": lower bound is", round(lower, 2), ", upper bound is", round(upper, 2), "\n")
  
  # Remove outliers by filtering based on lower & upper bounds
  newdata <- newdata[newdata[[col]] >= lower & newdata[[col]] <= upper, ]
}

#Convert categorical
unique(newdata$Sex)
newdata$Sex <- ifelse(newdata$Sex == "M",1,0)

unique(newdata$ChestPainType)
newdata$ChestPainType <- as.numeric(factor(newdata$ChestPainType,
                                          levels = c('ATA','NAP','ASY','TA'),
                                          labels = c(1,2,3,4)))

unique(newdata$RestingECG)
newdata$RestingECG <- as.numeric(factor(newdata$RestingECG,
                                       levels = c('Normal','ST','LVH'),
                                       labels = c(1,2,3)))

unique(newdata$ExerciseAngina)
newdata$ExerciseAngina <- ifelse(newdata$ExerciseAngina == "Y",1,0)

unique(newdata$ST_Slope)
newdata$ST_Slope <- as.numeric(factor(newdata$ST_Slope,
                                     levels = c('Up','Flat','Down'),
                                     labels = c(1,2,3)))
View(newdata)
str(newdata)

#Univariate Analysis
hist(newdata$Age)

hist(newdata$RestingBP)

hist(newdata$Cholesterol)

hist(newdata$MaxHR)

hist(newdata$Oldpeak)

#Multiple scatterplots
pairs(newdata[, c("Age", "RestingBP", "Cholesterol", "MaxHR", "Oldpeak")])
#Bivariate analysis
cor(newdata[c("Age", "RestingBP", "Cholesterol", "MaxHR", "Oldpeak")])

library(ggplot2)
#Sex & Heart disease
ggplot(newdata, aes(x = factor(Sex, labels = c("F", "M")))) +
  geom_bar(aes(fill = factor(HeartDisease, labels = c("0", "1"))), position = "dodge") +
  labs(x = "Sex", fill = "Heart Disease")
#ChestPainType & Heart disease
ggplot(newdata, aes(x = factor(ChestPainType, labels = c("ATA","NAP","ASY","TA")))) +
  geom_bar(aes(fill = factor(HeartDisease, labels = c("0", "1"))), position = "dodge") +
  labs(x = "ChestPainType", fill = "Heart Disease")
#ExerciseAngina & Heart disease
ggplot(newdata, aes(x = factor(ExerciseAngina, labels = c("N", "Y")))) +
  geom_bar(aes(fill = factor(HeartDisease, labels = c("0", "1"))), position = "dodge") +
  labs(x = "ExerciseAngina", fill = "Heart Disease")
#RestingECG & Heart disease
ggplot(newdata, aes(x = factor(RestingECG, labels = c("Normal", "ST", "LVH")))) +
  geom_bar(aes(fill = factor(HeartDisease, labels = c("0", "1"))), position = "dodge") +
  labs(x = "RestingECG", fill = "Heart Disease")
#ST_Slope & Heart disease
ggplot(newdata, aes(x = factor(ST_Slope, labels = c("Up", "Flat", "Down")))) +
  geom_bar(aes(fill = factor(HeartDisease, labels = c("0", "1"))), position = "dodge") +
  labs(x = "ST_Slope", fill = "Heart Disease")
#FastingBS & Heart disease
ggplot(newdata, aes(x = factor(FastingBS, labels = c("Otherwise", "FastingBS > 120 mg/dl")))) +
  geom_bar(aes(fill = factor(HeartDisease, labels = c("0", "1"))), position = "dodge") +
  labs(x = "FastingBS", fill = "Heart Disease")

#Age with Heartdisease 
boxplot(newdata$Age ~ newdata$HeartDisease)
#RestingBP with Heartdisease 
boxplot(newdata$RestingBP ~ newdata$HeartDisease)
#Cholesterol with Heartdisease 
boxplot(newdata$Cholesterol ~ newdata$HeartDisease)
#MaxHR with Heartdisease 
boxplot(newdata$MaxHR ~ newdata$HeartDisease)

#Data modeling
model <- glm(newdata$HeartDisease ~ ., data = newdata, family = binomial)
summary(model)

model1 <- glm(newdata$HeartDisease ~ Age + Sex + ChestPainType + RestingBP + Cholesterol +
               FastingBS + RestingECG + MaxHR + ExerciseAngina + Oldpeak +
               ST_Slope, data = newdata, family = binomial)
summary(model1)

model1 <- glm(newdata$HeartDisease ~ Age + Sex + ChestPainType + RestingBP + Cholesterol +
                FastingBS + MaxHR + ExerciseAngina + Oldpeak + ST_Slope, data = newdata, family = binomial)
summary(model1)

model2 <- glm(newdata$HeartDisease ~ Age + Sex + ChestPainType + RestingBP + Cholesterol +
                MaxHR + ExerciseAngina + Oldpeak + ST_Slope, data = newdata, family = binomial)
summary(model2)

model3 <- glm(newdata$HeartDisease ~ Age + Sex + ChestPainType + Cholesterol +
                MaxHR + ExerciseAngina + Oldpeak + ST_Slope, data = newdata, family = binomial)
summary(model3)

model4 <- glm(newdata$HeartDisease ~ Age + Sex + ChestPainType + Cholesterol +
                ExerciseAngina + Oldpeak + ST_Slope, data = newdata, family = binomial)
summary(model4)
summary(model4)$coef

#install.packages("rms")
library(rms)
model4.lrm <- lrm(newdata$HeartDisease ~ Age + Sex + ChestPainType + Cholesterol +
                    ExerciseAngina + Oldpeak + ST_Slope, data = newdata)
model4.lrm


