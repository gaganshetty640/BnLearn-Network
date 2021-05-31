# Set Directory ####
setwd("C:/Users/gagan.shetty/CPS/Bnlearn_Networks")

# install.packages("BiocManager")
# BiocManager::install("Rgraphviz")
# 
# if(!require(pacman)) install.packages("pacman")
# pacman::p_load(qgraph,bnlearn,Rgraphviz)

# Install the packages ####
if(!("bnlearn" %in% installed.packages()[,1] )){install.packages("bnlearn")}
if(!("forecast" %in% installed.packages()[,1] )){install.packages("forecast")}

# Load the libraries ####
library(bnlearn)
library(forecast)

# Load the data ####
data_copy <- read.csv("Mid_High_Segment_new connections_15Dec2020.csv",na.strings=c("?",NA))

# Copy the original data to some other variable
data1 <- data_copy

# Data Preprocessing ####
#Replace NA values with median of that column 
for(i in 1:ncol(data1)){
  data1[is.na(data1[,i]), i] <- median((data1[,i]), na.rm = TRUE)
}

library(abn, lib="C:/Users/gagan.shetty/Downloads")

#Checking summary of data
summary(data1)

#Checking how many NA values are there in columns
colSums(is.na(data1))

# storing continuous data columns (column 11 to 29) in variable data2
data2 <- data1[,c(11:29)]

# Discretizing the continuous data (data2)
list_m <-lapply(
  X= c("interval"),
  FUN = function(method) discretize(
    data= data2,
    method=method,
    breaks = 3,
    ordered= TRUE
  )
)

#Renaming the column
names(list_m) <-c("interval")

#Checking summary of discretized data(list_m)
lapply(X=list_m,FUN = summary)

# data1$CLTV_12_months <- cut(data1$CLTV_12_months, breaks = 3, include.lowest = TRUE)

# combining dichotomous and discretized data
data3 <- cbind.data.frame(data1[,1:10],list_m$interval)

#Converting each data type as factor type
data3[1:length(data3)] <- lapply(data3[1:length(data3)], as.factor)


#Model Building ####

#tabu
tabu_model <- tabu()


nb_model_1 <-
  bnlearn::tree.bayes(
    training = "CLTV_12_months",
    x = data3,
    debug = T,
    mi = "mi"
  )

#Creating object of the model
nb_model_1_tan_cpt <- bn.fit(nb_model_1, data = data3,method = "bayes",debug = T)

#Predicting the dependent variable
pred = predict(nb_model_1, data3)
# table(pred, data1[, "CLTV_12_months"])

attr(predict(nb_model_1_tan_cpt,"CLTV_12_months",as.data.frame(data3[,-11]),prob = T),"prob")


# rbn(nb_model_1_tan_cpt, 202)



graphviz.plot(nb_model_1)





# discretize(data1, method = 'interval', breaks = 4, ibreaks = 20)
