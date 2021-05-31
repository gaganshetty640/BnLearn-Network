# Set Directory ####
setwd("C:/Users/mohammed.basha/Desktop/bayesialab simulation")

# Install the packages ####
if(!("bnlearn" %in% installed.packages()[,1] )){install.packages("bnlearn")}
if(!("forecast" %in% installed.packages()[,1] )){install.packages("forecast")}
BiocManager::install("RBGL")
# Load the libraries ####
library(bnlearn)
library(forecast)
library(gRain)
library('RBGL')

# Load the data ####
data1 <- read.csv("data_3node.csv")

data3 <-data1

#Method 1#####
#(continuous dataset)

data2 <- sapply(data1[,c("x1","x2","x3")],function(x) as.factor(x)) %>% data.frame(stringsAsFactors = T)


dag <- empty.graph(nodes = c("x1","x2","x3"))
arc.set <- matrix(c("x1",	"x2",
                    "x2",	"x3"
                    ),byrow = TRUE, ncol = 2,dimnames = list(NULL, c("from", "to")))
arcs(dag) <- arc.set

nb_model_1 <- bn.fit(dag, data = data2,method = "bayes",debug = T)


#Method 2 ####
#(discrete datset)

data3$x1 <- ifelse(data1$x1<=2,"<=2", ifelse(data1$x1>2 & data1$x1<=4,"<=4",ifelse(data1$x1>4,">4",""))) %>% as.factor
data3$x2 <- ifelse(data1$x2<=5,"<=5", ifelse(data1$x2>5 & data1$x2<=6,"<=6",ifelse(data1$x2>6,">6",""))) %>% as.factor
data3$x3 <- ifelse(data1$x3<=12,"<=12", ifelse(data1$x3>12 & data1$x3<=16,"<=16",ifelse(data1$x3>16,">16",""))) %>% as.factor
# data3$x4 <- ifelse(data1$x4<=2,"<=2", ifelse(data1$x4>2 & data1$x4<=4,"<=4",ifelse(data1$x4>4,">4",""))) %>% as.factor

dag <- empty.graph(nodes = c("x1","x2","x3"))
arc.set <- matrix(c("x1",	"x2",
                    "x2",	"x3"),byrow = TRUE, ncol = 2,dimnames = list(NULL, c("from", "to")))
arcs(dag) <- arc.set

nb_model_2 <- bn.fit(dag, data = data3,method = "bayes",debug = T)

#plotting DAGs
graphviz.plot(dag, layout = "dot")
graphviz.plot(dag, layout = "fdp")
graphviz.plot(dag, layout = "circo")

bn.fit.barchart(nb_model_2$x1, main = "Conditional Probability",
                xlab = "Prob", ylab = "")

# probability
a1<-nb_model_2$x1$prob

a2<-nb_model_2$x2$prob

a3<-nb_model_2$x3$prob


compile_fit <- compile(as.grain((nb_model_2)))

summary(compile_fit)

#marginal probability
querygrain(compile_fit,c('x1','x2','x3'),type = "joint")
querygrain(compile_fit,nodes =c("x1","x2","x3"),type = "marginal",result = "array")

#setting hard evidence 
tree_query <- setEvidence(compile_fit, nodes=c("x1"), states=c("<=2"))
a<-querygrain(tree_query,nodes =c("x1","x2","x3"),type = "marginal",result = "array")
a1<-querygrain(tree_query,nodes =c("x1","x2","x3"),type = "joint",result = "array")
a2<-querygrain(tree_query,nodes =c("x1","x2","x3"),type = "conditional",result = "array")

vname<-c("x1")
inobs<-c("<=2")
quer <- function(inobs){
  tree_query <- setEvidence(compile_fit, nodes=vname, states=inobs)
  a<-querygrain(tree_query,nodes = c("x1","x2","x3"))
  return(a)
}
out <- apply(d, 1, quer)
out1 <- t(as.data.frame(out))


d<-as.data.frame(matrix(,nrow=3,ncol=3))



#trying virtual evidence
tree_query <- setEvidence(compile_fit, nodes=c("x1"), states=c("<=2"))

tree_query <- setEvidence(compile_fit, evidence=list(x1=c(0.5, 0.5,0), x2=c(0.5, 0.5,0)),propagate = F)
querygrain(compile_fit,nodes =c("x1","x2","x3"),type = "marginal",result = "array")
querygrain(tree_query,nodes =c("x1","x2","x3"),type = "marginal",result = "array")

sapply(qb, as.numeric)

?retractEvidence
