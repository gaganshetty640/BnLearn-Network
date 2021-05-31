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

#Method 1 ####

data1[1:10] <- lapply(data1[1:10], as.factor)


#Model Building 

#tabu
tabu_model <- tabu(x = data1,optimized = TRUE)

tabu_model_fitted <- bn.fit(tabu_model,data1)
graphviz.plot(tabu_model,layout="neato")


#Method 2 ####
# storing continuous data columns (column 11 to 29) in variable data2
data2 <- data1[,c(11:29)]

# Discretizing the continuous data (data2)
list_m <-lapply(
  X= c("interval","quantile","hartemink"),
  FUN = function(method) discretize(
    data= data2,
    method=method,
    breaks = 3,
    ordered= TRUE
  )
)

#Renaming the column
names(list_m) <-c("interval","quantile","hartemink")


#Checking summary of discretized data(list_m)
lapply(X=list_m,FUN = summary)

# data1$CLTV_12_months <- cut(data1$CLTV_12_months, breaks = 3, include.lowest = TRUE)

# combining dichotomous and discretized data
data_interval <- cbind.data.frame(data1[,1:10],list_m$interval)
data_quantile <- cbind.data.frame(data1[,1:10],list_m$quantile)
data_hartemink <- cbind.data.frame(data1[,1:10],list_m$hartemink)


#Converting each data type as factor type
data_interval[1:length(data_interval)] <- lapply(data_interval[1:length(data_interval)], as.factor)
data_quantile[1:length(data_quantile)] <- lapply(data_quantile[1:length(data_quantile)], as.factor)
data_hartemink[1:length(data_hartemink)] <- lapply(data_hartemink[1:length(data_hartemink)], as.factor)

#Model building
#tabu
tabu_model_interval <- tabu(x = data_interval,optimized = TRUE,score="bic")
tabu_model_quantile <- tabu(x = data_quantile,optimized = TRUE,score="bic")
tabu_model_hartemink <- tabu(x = data_hartemink,optimized = TRUE,score="bic")


tabu_model_interval_score <- score(tabu_model_interval, data_interval, type = "bic")
tabu_model_quantile_score <- score(tabu_model_quantile, data_quantile, type = "bic")
tabu_model_hartemink_score <- score(tabu_model_hartemink, data_hartemink, type = "bic")


tabu_model_fittedinterval <- bn.fit(tabu_model_interval,data_interval)
tabu_model_fittedquantile <- bn.fit(tabu_model_quantile,data_quantile)
tabu_model_fittedhartemnik <- bn.fit(tabu_model_hartemink,data_hartemink)


#Plotting 
par(cex=0.05)
graphviz.plot(tabu_model_interval,main = "Interval")
graphviz.plot(tabu_model_quantile,main = "Quantile")
graphviz.plot(tabu_model_hartemink,main = "Hartemink")


g <- Rgraphviz::layoutGraph(bnlearn::as.graphNEL(tabu_model_hartemink))
graph::nodeRenderInfo(g) <- list(fontsize=100)
Rgraphviz::renderGraph(g)

g2 <- Rgraphviz::layoutGraph(bnlearn::as.graphNEL(tabu_model_quantile))
graph::nodeRenderInfo(g2) <- list(fontsize=100)
Rgraphviz::renderGraph(g2)

g3 <- Rgraphviz::layoutGraph(bnlearn::as.graphNEL(tabu_model_interval))
graph::nodeRenderInfo(g3) <- list(fontsize=100)
Rgraphviz::renderGraph(g3)
