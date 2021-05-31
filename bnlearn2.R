# Set Directory ####
setwd("C:/Users/gagan.shetty/CPS")

# Install the packages ####
if(!("bnlearn" %in% installed.packages()[,1] )){install.packages("bnlearn")}
if(!("forecast" %in% installed.packages()[,1] )){install.packages("forecast")}

# Load the libraries ####
library(bnlearn)
library(forecast)

# Load the data ####
data1 <- read.csv("bayesia_test.csv")

data3 <-data1

#Method 1####
# with(dat, ifelse(x < 0.15 & dif <0, 3, ifelse(x > 0.15 & dif < 0, 2, 1)))
data2 <- sapply(data1[,c("x1","x2")],function(x) as.factor(x)) %>% data.frame(stringsAsFactors = T)


dag <- empty.graph(nodes = c("x1","x2"))
arc.set <- matrix(c("x1",	"x2"),byrow = TRUE, ncol = 2,dimnames = list(NULL, c("from", "to")))
arcs(dag) <- arc.set

nb_model_1 <- bn.fit(dag, data = data2,method = "bayes",debug = T)


#Method 2 ####

data3$x1 <- ifelse(data1$x1<=2,"<=2", ifelse(data1$x1>2 & data1$x1<=4,"<=4",ifelse(data1$x1>4,">4",""))) %>% as.factor
data3$x2 <- ifelse(data1$x2<=5,"<=5", ifelse(data1$x2>5 & data1$x2<=6,"<=6",ifelse(data1$x2>6,">6",""))) %>% as.factor
data3$x3 <- ifelse(data1$x3<=12,"<=12", ifelse(data1$x3>12 & data1$x3<=16,"<=16",ifelse(data1$x3>16,">16",""))) %>% as.factor
data3$x4 <- ifelse(data1$x4<=2,"<=2", ifelse(data1$x4>2 & data1$x4<=4,"<=4",ifelse(data1$x4>4,">4",""))) %>% as.factor

dag <- empty.graph(nodes = c("x1","x2","x3","x4"))
arc.set <- matrix(c("x1",	"x2",
                    "x1",	"x3",
                    "x3", "x4",
                    "x2", "x4"),byrow = TRUE, ncol = 2,dimnames = list(NULL, c("from", "to")))
arcs(dag) <- arc.set

nb_model_2 <- bn.fit(dag, data = data3,method = "bayes",debug = T)

graphviz.plot(nb_model_2)
