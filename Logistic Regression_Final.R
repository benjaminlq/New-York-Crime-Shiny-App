library(readxl)
library(readr)
library(dplyr)
library(plyr)
library(ggplot2)
library(tidyverse)
library(data.table)
library(zoo)
library(pROC)
library(MASS)
library(caret)

rawdata <- read_csv("arrest_2016.csv")
for (i in 2017:2021){
  rawdata <- rbind(rawdata,read_csv(paste("arrest_",as.character(i),".csv",sep = "")))
}

arrest = subset(rawdata, select = c('ARREST_DATE','OFNS_DESC', 'LAW_CAT_CD', 'ARREST_BORO', 'ARREST_PRECINCT', 'AGE_GROUP','PERP_SEX','PERP_RACE'))

head(rawdata)
summary(arrest)
nrow(arrest)


# Change data type and map values

arrest$ARREST_DATE <- as.Date(arrest$ARREST_DATE,format = "%m/%d/%Y")
arrest$WEEKDAY <- weekdays(arrest$ARREST_DATE)
arrest$WEEKDAY <- factor(arrest$WEEKDAY, levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
arrest$ARREST_PRECINCT <- as.character(arrest$ARREST_PRECINCT)
arrest$ARREST_BORO <- mapvalues(arrest$ARREST_BORO,
                              from = c('B','K','M','Q','S'),
                              to = c('Bronx','Brooklyn','Manhattan','Queens','Staten Island'))
arrest$LAW_CAT_CD <- mapvalues(arrest$LAW_CAT_CD,
                               from = c('F','I','M','V'),
                               to = c('Felony','Infraction','Misdemeanor','Violation'))
arrest$PERP_SEX <- mapvalues(arrest$PERP_SEX,
                               from = c('F','M'),
                               to = c('Female','Male'))

glimpse(arrest)


#check for missing value

vars <- colnames(arrest) 
for (i in vars){
 print(paste(i,sum(is.na(arrest[,i]))))
}


#exclude missing values 

arrest = filter(arrest, LAW_CAT_CD!='' & OFNS_DESC!='')
glimpse(arrest)

# create violent variables
arrest$ARREST_YEAR <- format(as.Date(arrest$ARREST_DATE, format="%d/%m/%Y"),"%Y")
arrest$ARREST_MONTH <- format(as.Date(arrest$ARREST_DATE, format="%d/%m/%Y"),"%m")
arrest$ARREST_TIME <- as.yearmon(arrest$ARREST_DATE,"%m-%Y")

arrest$B_violent <-ifelse(arrest$OFNS_DESC %in% c('ARSON','DANGEROUS WEAPONS', 'FELONY ASSAULT','ASSAULT 3 & RELATED OFFENSES', 'GRAND LARCENY OF MOTOR VEHICLE','HOMICIDE-NEGLIGENT,UNCLASSIFIE','KIDNAPPING','KIDNAPPING & RELATED OFFENSES', 'MURDER & NON-NEGL. MANSLAUGHTE','RAPE','ROBBERY'), 1, 0)

arrest$F_violent <- ifelse(arrest$B_violent==1, 'Violent','Non-violent')

head(arrest)



arrest <- as.data.frame(arrest)
colnames(arrest)


# Data exploration - categorical
for (i in c(4:11))
{
  plot <- ggplot(data=arrest, mapping = aes(x=arrest[,i], fill = F_violent)) + geom_bar(position = "fill") + labs(x = colnames(arrest)[i])+theme(axis.text.x = element_text(angle = 90))
  print(plot)
}


for (i in c(4:11))
{
  plot <- ggplot(data=arrest, mapping = aes(x=arrest[,i], fill = F_violent)) +
   geom_bar() + labs(x = colnames(arrest)[i]) +
   theme(axis.text.x = element_text(angle = 90))
  print(plot)
}



# =================== fit initial Logistic====================================================

arrest_B = subset(arrest, select = c('ARREST_BORO','AGE_GROUP','PERP_SEX','PERP_RACE','WEEKDAY', 'ARREST_YEAR' ,'ARREST_MONTH', 'B_violent'))
arrest_B$ARREST_YEAR = as.numeric(arrest_B$ARREST_YEAR)
arrest_B$ARREST_MONTH = as.numeric(arrest_B$ARREST_MONTH)


library(caret)
set.seed(4321)
partition <- createDataPartition(arrest_B$B_violent, list = FALSE, p = .70) 
train <- arrest_B[partition, ]
test <- arrest_B[-partition, ]

#check both datasets are representative

print("TRAIN")
mean(train$B_violent)
print("Test")
mean(test$B_violent)

# fit
GLM <- glm(B_violent ~. , data = train, family = binomial(link = "logit"))
summary(GLM)

#Some levels of race are not significant, check if all variables are significant

anova(GLM, test='Chisq')

# model performance

predslogit <- predict(GLM,newdat=test,type="response")
confusionMatrix(factor(1*(predslogit>.5)),factor(test$B_violent))

roclogit <- roc(test$B_violent,predslogit)
plot(roclogit)
auc(roclogit)

#try stepwise selection

model_step <- stepAIC(GLM)


# considered by variable, not by level
###################### stepAIC with binarization##########################
arrest_DS = subset(arrest, select = c('ARREST_BORO','AGE_GROUP','PERP_SEX','PERP_RACE','WEEKDAY', 'ARREST_YEAR', 'ARREST_MONTH', 'B_violent'))
arrest_DS$ARREST_YEAR = as.numeric(arrest_DS$ARREST_YEAR)
arrest_DS$ARREST_MONTH = as.numeric(arrest_DS$ARREST_MONTH)

# create binary vars
factor_names <- c("PERP_RACE","ARREST_BORO","WEEKDAY", "AGE_GROUP")
factor_vars <- arrest_DS[,factor_names]
for (var in factor_names) {
  factor_vars[, var] <- as.character(factor_vars[, var])
}

binarizer <- caret::dummyVars(paste("~", paste(factor_names, collapse = "+")) , data = factor_vars, fullRank = FALSE)
binarized_vars <- data.frame(predict(binarizer, newdata = factor_vars))
head(binarized_vars)


#delete a base level
binarized_vars$ARREST_BOROBronx <- NULL
binarized_vars$AGE_GROUP.18 <- NULL
binarized_vars$PERP_RACEAMERICAN.INDIAN.ALASKAN.NATIVE<- NULL
binarized_vars$WEEKDAYMonday<- NULL

head(binarized_vars)

#create binarized data

arrest.bin <- cbind(arrest_DS,binarized_vars)

arrest.bin$PERP_RACE <- NULL
arrest.bin$ARREST_BORO <- NULL
arrest.bin$WEEKDAY <- NULL
arrest.bin$AGE_GROUP <- NULL
arrest.bin$F_Felony <- NULL
summary(arrest.bin)

#partition using the same seed value

set.seed(4321)
partition <- createDataPartition(arrest.bin$B_violent, list = FALSE, p = .70) 
train.bin <- arrest.bin[partition, ]
test.bin <- arrest.bin[-partition, ]

# check if both datasets are representative

print("TRAIN")
mean(train.bin$B_violent)
print("Test")
mean(train.bin$B_violent)


#fit w all vars

glmall <- glm(B_violent ~ . , data=train.bin, family = binomial(link = "logit"))
summary(glmall)


#try to drop vars that are not essential - backward
glmback<- stepAIC(glmall, direction="backward")
summary(glmback)


#model performance check

predback <- predict(glmback,newdat=test.bin,type="response")
confusionMatrix(factor(1*(predback>.5)),factor(test.bin$B_violent))

rocback <- roc(test.bin$B_violent,predback)
plot(rocback)
auc(rocback)



# rerun the model on full data

glmfull <- glm(formula = B_violent ~ PERP_SEX + ARREST_YEAR + ARREST_MONTH + 
    PERP_RACEASIAN...PACIFIC.ISLANDER + PERP_RACEBLACK.HISPANIC + 
    PERP_RACEUNKNOWN + PERP_RACEWHITE + PERP_RACEWHITE.HISPANIC + 
    ARREST_BOROBrooklyn + ARREST_BOROManhattan + ARREST_BOROQueens + 
    ARREST_BOROStaten.Island + WEEKDAYFriday + WEEKDAYSaturday + 
    WEEKDAYSunday + WEEKDAYThursday + WEEKDAYTuesday + WEEKDAYWednesday + 
    AGE_GROUP18.24 + AGE_GROUP25.44 + AGE_GROUP45.64 + AGE_GROUP65., 
    family = binomial(link = "logit"), data = arrest.bin)

summary(glmfull)




pred <- predict(glmfull,newdat=arrest.bin,type="response")
confusionMatrix(factor(1*(pred>.5)),factor(arrest.bin$B_violent))

roc <- roc(arrest.bin$B_violent,pred)
plot(roc)
auc(roc)



                        
