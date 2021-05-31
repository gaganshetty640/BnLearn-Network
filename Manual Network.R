# Set Directory ####

setwd("D:\\Projects\\Solution\\NBS")

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


# Look at the directed acyclic graph
dag <- empty.graph(nodes = c("Urban",
                             "Rural",
                             "Surburban",
                             "Web",
                             "Phone",
                             "Multichannel",
                             "Discount",
                             "BOGO",
                             "NoOffer",
                             "Treated_Responded",
                             "CLTV_12_months",
                             "WHITE_HANGING_HEART_T-LIGHT_HOLDER",
                             "REGENCY_CAKESTAND_3_TIER",
                             "JUMBO_BAG_RED_RETROSPOT",
                             "PARTY_BUNTING",
                             "LUNCH_BAG_RED_RETROSPOT",
                             "SET_OF_3_CAKE_TINS_PANTRY_DESIGN",
                             "LUNCH_BAG _BLACK_SKULL",
                             "PACK_OF_72_RETROSPOT_CAKE_CASES",
                             "PAPER_CHAIN_KIT_50'S_CHRISTMAS",
                             "LUNCH_BAG_SPACEBOY_DESIGN",
                             "LUNCH_BAG_CARS_BLUE",
                             "LUNCH_BAG_PINK_POLKADOT",
                             "JAM_MAKING_SET_WITH_JARS",
                             "LUNCH_BAG_SUKI_DESIGN",
                             "LUNCH_BAG_APPLE_DESIGN",
                             "JAM_MAKING_SET_PRINTED",
                             "LUNCH_BAG_WOODLAND",
                             "JUMBO_STORAGE_BAG_SUKI"))


arc.set <- matrix(c("PACK_OF_72_RETROSPOT_CAKE_CASES",	"CLTV_12_months",
                    "LUNCH_BAG_APPLE_DESIGN",	"CLTV_12_months",
                    "LUNCH_BAG_WOODLAND",	"CLTV_12_months",
                    "JAM_MAKING_SET_WITH_JARS",	"PACK_OF_72_RETROSPOT_CAKE_CASES",
                    "JAM_MAKING_SET_WITH_JARS",	"JAM_MAKING_SET_PRINTED",
                    "JAM_MAKING_SET_WITH_JARS",	"SET_OF_3_CAKE_TINS_PANTRY_DESIGN",
                    "SET_OF_3_CAKE_TINS_PANTRY_DESIGN",	"WHITE_HANGING_HEART_T-LIGHT_HOLDER",
                    "SET_OF_3_CAKE_TINS_PANTRY_DESIGN",	"LUNCH_BAG_SPACEBOY_DESIGN",
                    "LUNCH_BAG_SPACEBOY_DESIGN",	"LUNCH_BAG_APPLE_DESIGN",
                    "SET_OF_3_CAKE_TINS_PANTRY_DESIGN",	"JUMBO_STORAGE_BAG_SUKI",
                    "BOGO",	"JAM_MAKING_SET_WITH_JARS",
                    "Discount",	"BOGO",
                    "NoOffer",	"Discount",
                    "NoOffer",	"BOGO",
                    "NoOffer",	"Treated_Responded",
                    "Discount",	"LUNCH_BAG_WOODLAND",
                    "LUNCH_BAG_WOODLAND",	"LUNCH_BAG_RED_RETROSPOT",
                    "LUNCH_BAG_APPLE_DESIGN",	"LUNCH_BAG_RED_RETROSPOT",
                    "LUNCH_BAG_APPLE_DESIGN",	"LUNCH_BAG_WOODLAND",
                    "LUNCH_BAG_APPLE_DESIGN",	"LUNCH_BAG_SUKI_DESIGN",
                    "LUNCH_BAG_CARS_BLUE",	"LUNCH_BAG_WOODLAND",
                    "LUNCH_BAG_CARS_BLUE",	"LUNCH_BAG_SUKI_DESIGN",
                    "LUNCH_BAG_SUKI_DESIGN",	"LUNCH_BAG_BLACK_SKULL",
                    "LUNCH_BAG_BLACK_SKULL",	"LUNCH_BAG_PINK_POLKADOT",
                    "LUNCH_BAG_PINK_POLKADOT",	"JUMBO_BAG_RED_RETROSPOT",
                    "LUNCH_BAG_RED_RETROSPOT", "JUMBO_BAG_RED_RETROSPOT",
                    "JUMBO_BAG_RED_RETROSPOT",	"JAM_MAKING_SET_PRINTED",
                    "PACK_OF_72_RETROSPOT_CAKE_CASES",	"PAPER_CHAIN_KIT_50'S_CHRISTMAS",
                    "PAPER_CHAIN_KIT_50'S_CHRISTMAS",	"REGENCY_CAKESTAND_3_TIER",
                    "PAPER_CHAIN_KIT_50'S_CHRISTMAS",	"PARTY_BUNTING",
                    "WHITE_HANGING_HEART_T-LIGHT_HOLDER",	"Multichannel",
                    "Multichannel",	"Web",
                    "Multichannel",	"JUMBO_BAG_RED_RETROSPOT",
                    "Phone", "Web",
                    "Phone", "Multichannel",
                    "Urban", "JAM_MAKING_SET_PRINTED",
                    "Urban", "Rural",
                    "Urban", "Surburban",
                    "Surburban", "LUNCH_BAG_RED_RETROSPOT"
),
byrow = TRUE, ncol = 2,
dimnames = list(NULL, c("from", "to")))

# Assigning the same arc
arcs(dag) <- arc.set


interval_score <- score(dag, data_interval, type = "bic")
quantile_score <- score(dag, data_quantile, type = "bic")
hartemink_score <- score(dag, data_hartemink, type = "bic")


fittedinterval <- bn.fit(dag,data_interval)
fittedquantile <- bn.fit(dag,data_quantile)
fittedhartemnik <- bn.fit(dag,data_hartemink)






