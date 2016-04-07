library(kohonen)
library(ggplot2)
library(RColorBrewer)

set.seed(7)
train <- scale(train_data)
test <- scale(test_data, center = attr(train, "scaled:center"), scale = attr(Xtrain, "scaled:scale"))
Y <-  ### data to be modelled categorical or continous
alpha <- c(,)  ### vector for the learning rate
topo <- ### either hexagonal or rectangular
contin <- ### either TRUE or FALSE indicating whether its a continous or a categorical variable
xweight <-
types <- c(NULL, "codes", "changes", "count", "quality", "property", "mapping")

k_som <- function(data,num, x, y, topography, rlen = 100, alpha = c(0.05, 0.01),
                  radius = quantile(nhbrdist, 0.67) * c(1, -1), init,
                  toroidal = FALSE, n.hood, keep.data = TRUE){
  k_som_grid <- somgrid(x,y, topo = topography)
  scaled_data <- scale(data)
  som(scaled_data, grid = k_som_grid, rlen, alpha, radius, init, toroidal, n.hood, keep.data)
  plot()
  
}
auto <- read.table(file = "C:/Users/bharath.goda/Desktop/auto.txt", header = F, sep = ",", stringsAsFactors = T)
names_auto <- read.table("C:/Users/bharath.goda/Desktop/names.txt", sep = "\n", stringsAsFactors = T)
names_auto <- t(names_auto)
names(auto) <- names_auto
auto$`normalized-losses` <- as.character(auto$`normalized-losses`)
auto$`normalized-losses` <- as.numeric(auto$`normalized-losses`)
i <- sapply(auto, is.factor)
auto_cont <- auto[!i]
na.mean <- function (x) {
    x[is.na(x)] <- mean(x, na.rm = T)
    return(x)
}
na.zero <- function (x) {
    x[is.na(x)] <- 0
    return(x)
}
auto_cont <- apply(auto_cont, 2, na.mean)
auto_cont_scaled <- scale(auto_cont)
auto_som <- som(auto_cont_scaled, grid = somgrid(8, 6, "hexagonal"))

plot(auto_som, type="mapping", 
     labels = as.numeric(auto$`fuel-type`), col = as.numeric(auto$`fuel-type`)+1,
    main = "mapping plot")
plot(auto_som, type="changes", palette.name=coolBlueHotRed)
plot(auto_som, type="counts", palette.name=coolBlueHotRed)
plot(auto_som, type="mapping", 
     labels = as.numeric(auto$make), col = as.numeric(auto$make)+1,
     main = "mapping plot")
plot(auto_som, type = "property", property = unscaled, main=colnames(auto_som$data)[11], palette.name=coolBlueHotRed, heatkey = T, heatkeywidth = .1,  keepMargins = T)
pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2')
som_cluster <- cutree(hclust(dist(auto_som$codes)), 6)
plot(auto_som, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters") 
add.cluster.boundaries(auto_som, som_cluster, lwd = 5)

plot(auto_som, type = "property", property = unscaled, main=colnames(auto_som$data)[11], palette.name=topo.colors, heatkey = T, heatkeywidth = .1,  keepMargins = T)
plot(auto_som, type="mapping", bgcol = topo.colors(7, 1)[som_cluster], main = "Clusters")


rainbow(n, s = 1, v = 1, start = 0, end = max(1, n - 1)/n, alpha = 1)
heat.colors(n, alpha = 1)
terrain.colors(n, alpha = 1)
topo.colors(n, alpha = 1)
cm.colors(n, alpha = 1)

factor2numeric <- function(x,y){
	k <- as.character(x[y])
	return(as.numeric(k))
}


apply(auto, 2, function(x){x[is.na(x)] <- mean(x, na.rm = T)})


i <- sapply(auto, is.factor)
auto[i] <- lapply(auto[i], as.character)

i <- sapply(auto, is.character)
data <- data[!i]



na.mean <- function (x) {
    x[is.na(x)] <- mean(x, na.rm = T)
    return(x)
}

library(XLConnect)
wb <- loadWorkbook("C:/Users/bharath.goda/Documents/yeast.xlsx", package = "XLConnect")
wb <- loadWorkbook(system.file(file.choose(), package = "XLConnect"))

wb <- loadWorkbook("C:/Users/bharath.goda/Documents/yeast.xlsx")
t = readWorksheet(wb, sheet = getSheets(wb))
t <- sapply(t, as.matrix)

for(i in 1:length(t)){
	z[i] <- }

	t = readWorksheet(loadWorkbook("C:/Users/bharath.goda/Documents/yeast.xlsx"), sheet = getSheets(loadWorkbook("C:/Users/bharath.goda/Documents/yeast.xlsx")))
	
	
	
	
	
	
	
	
	
	"D:/FRACTAL/SOM/Knime/wines.csv"


library(readxl)
library(kohonen)
read_excel_allsheets <- function(filename) {
    sheets <- readxl::excel_sheets(filename)
    x <-    lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
    names(x) <- sheets
    x
}
matrixminusone<-function(x) {
    m<-as.matrix(x[,-1])
    rownames(m)<-x[,1]
    m
}
yeast <- read_excel_allsheets("C:/Users/bharath.goda/Documents/yeast.xlsx")
t <- sapply(t, matrixminusone)
t.supersom <- supersom(t, somgrid(6, 6, "hexagonal"), whatmap = 3:6)











p[k] <- apply(auto[k], 2, as.numeric)

som_cluster <- clusterSOM(ClusterCodebooks= auto_som$codes, LengthClustering = nrow(auto_som$code),
Mapping=auto_som$unit.classif, MapRow=auto_som$grid$xdim, MapColumn=auto_som$grid$ydim,
StoppingCluster=2)[[5]]


as.numeric.factor <- function(x) {seq_along(levels(x))[x]}
n <- sapply(auto, as.numeric)


 i <- sapply(auto, function(x)all(is.na(x))) ### gives teh column which have NA values

 
 
auto <- read.csv(file = "auto.csv")
k <- c(19,20, 22, 23, 26)
auto[k]  <- apply(auto[k], 2, as.numeric)
auto <- sapply(auto, as.numeric)
i <- sapply(auto, function(x)(any(is.na(x))))
auto <- apply(auto, 2, na.mean)
k <- c("fuel.type", "engine.type", "engine.size", "city.mpg")
auto_sub <- auto[k]
auto_cont_scaled <- scale(auto_sub)
auto_som <- som(auto_cont_scaled, grid = somgrid(6, 6, "hexagonal"))



marital_data <- data[, c("T1_2SGLT", "T1_2MART", "T1_2SEPT",
"T1_2DIVT", "T1_2WIDT", "T1_2T")]



na.mean <- function (x) {
    x[is.na(x)] <- mean(x, na.rm = T)
    return(x)
}

na.zero <- function (x) {
    x[is.na(x)] <- 0
    return(x)
}

na.remove <- function (x) {    
    return(x[!is.na(x)])
}
 
###################################################################################################################################################
MAAIN FUNCTION
#################################################################################################################################################3

library(kohonen)
library(dplyr)
cards <- read.csv("C:/Users/bharath.goda/Desktop/Cards_Data_Model_24April.csv")
cards_sub <- select(cards, Risky2_tag, revenue_tag, GENDER, MARITAL_STATUS, EDUCATION, CL_PED_CART_SCWR,MACHINE_CREDLIM, CREDLIM, atm_spend, AGE, no_of_debit_txn, no_of_credit_txn, credit_amt)
cards_sub[6] <- lapply(cards_sub[6], as.character)
cards_sub[6] <- lapply(cards_sub[6], as.numeric)
i <- apply(cards_sub, 2,function(x)(any(is.na(x))))
cards_sub[i] <- apply(cards_sub[i], 2, na.mean)
cards_sub2 <- cards_sub 
cards_sub <- sapply(cards_sub, as.numeric)
cards_sub <- as.data.frame(cards_sub)
cards_scaled <- scale(cards_sub)
j <- sapply(cards_sub2, is.factor)
z <- names(cards_sub[j])



cards_som <- som(cards_scaled, grid = somgrid(20, 20, "hexagonal"), rlen = 150)
plot(cards_som, type="changes", palette.name=coolBlueHotRed)
plot(cards_som, type="changes", palette.name=coolBlueHotRed)
cards_som <- som(cards_scaled, grid = somgrid(20, 20, "hexagonal"), rlen = 200)
plot(cards_som, type="changes", palette.name=coolBlueHotRed)
plot(cards_som, type="counts", palette.name=coolBlueHotRed)

unscaled <- aggregate(as.numeric(cards_sub[,i]), by=list(cards_som$unit.classif), FUN=mean, simplify=TRUE)[,2]
if(colnames(cards_som$data)[i] %in% z, 
 plot(cards_som, type = "property", property = unscaled, main=colnames(cards_som$data)[i], palette.name=topo.colors, heatkey = F,  ncolors = length(unique(cards_sub[i])[,1])),
 plot(cards_som, type = "property", property = unscaled, main=colnames(cards_som$data)[i], palette.name=topo.colors, heatkey = T, heatkeywidth = 0.5,  keepMargins = T, ncolors = length(unique(cards_sub[i])[,1]))
 } 

jpeg(file = “C://R//SAVEHERE//myplot.jpg”)
plot(x,y)
dev.off()


for(i in 1:13){
 mypath <- paste("C:/Users/bharath.goda/Desktop", i, ".png", sep = "")
 png(file=mypath)
 unscaled <- aggregate(as.numeric(cards_sub[,i]), by=list(cards_som$unit.classif), FUN=mean, simplify=TRUE)[,2]
 plot(cards_som, type = "property", property = unscaled, main=colnames(cards_som$data)[var], palette.name=topo.colors, heatkey = T, heatkeywidth = 0.5,  keepMargins = T, ncolors = 6)
 dev.off()
}


 for(i in 1:ncol(cards_sub)){
    mypath <- paste("C:/Users/bharath.goda/Desktop/som_plots/", i, ".png", sep = "")
     png(file=mypath)
     unscaled <- aggregate(as.numeric(cards_sub[,i]), by=list(cards_som$unit.classif), FUN=mean, simplify=TRUE)[,2]
if(colnames(cards_som$data)[i] %in% z){ 
 plot(cards_som, type = "property", property = unscaled, main=colnames(cards_som$data)[i], palette.name=topo.colors, heatkey = F,  ncolors = length(unique(cards_sub[i])[,1])),
 else(plot(cards_som, type = "property", property = unscaled, main=colnames(cards_som$data)[i], palette.name=topo.colors, heatkey = T, heatkeywidth = 0.5,  keepMargins = T, ncolors = length(unique(cards_sub[i])[,1])))
 }
dev.off()
 }
 
 
 
 ## Load the dataset
 ## converting the numeric data which is in character state to numeric
 ## converting the categorical variables to numeric variables
 ## selecting the required columns
 ## replacing the NA values with the mean/zero/omitting. Its better to omit the observations with NA vlaues in categorical variables
 ## applying the R Model
 ## selecting the required plots




##3 create all the plots and show the viewer only what he wants to viewwer
## put an option where he can selected if all graphs have to be made 
 
 

 
 
 
library(kohonen)
data <- knime.in
k ## columns which are actually numeric but are in character/factor
data[k]  <- apply(data[k], 2, as.character)
data[k]  <- apply(data[k], 2, as.character)
i <- sapply(data, is.factor)
data[i] <- sapply(data[i], as.numeric)
l ## columns which have to be considered
data_sub <- data[l]
data_scaled <- scale(data_sub)
knime.model <- som(data_scaled, grid = somgrid(xdim, ydim, topo = "hexagonal"), rlen = 100, alpha = c(0.05, 0.01),
    radius = quantile(nhbrdist, 0.67) * c(1, -1), init,
    toroidal = FALSE, n.hood, keep.data = TRUE)

k <- function(x){
	z <- letters[1:length(levels(x[i]))]
	
	}
 

 
  legend("left", legend = levels(cards$EDUCATION),title = "levels", fill = topo.colors(length(unique(cards_sub$EDUCATION))), cex = 1, bty = "n")
  legend("bottom", legend = levels(cards$EDUCATION),title = "levels", fill = topo.colors(length(unique(cards_sub$EDUCATION))), cex = 1, bty = "n", horiz=TRUE)
  
  
 b <- 0
 
 
b <- 0
for (i in 1:ncol(cards_sub2)){
if(is.factor(cards_sub2[i]){
b <- c(b,i))
} 
}






for(i in 1:ncol(cards_sub)){
mypath <- paste("C:/Users/bharath.goda/Desktop/som_plots/", i, ".png", sep = "")
png(file=mypath)
unscaled <- aggregate(as.numeric(cards_sub[,i]), by=list(cards_som$unit.classif), FUN=mean, simplify=TRUE)[,2]
ifelse(colnames(cards_som$data)[i] %in% z, plot(cards_som, type = "property", property = unscaled, main=colnames(cards_som$data)[i], palette.name=topo.colors, heatkey = F,  ncolors = length(unique(cards_sub[i])[,1])), plot(cards_som, type = "property", property = unscaled, main=colnames(cards_som$data)[i], palette.name=topo.colors, heatkey = T, heatkeywidth = 0.5,  keepMargins = T, ncolors = length(unique(cards_sub[i])[,1])))
dev.off()
 }
 
 
 if(colnames(cards_som$data)[i] %in% z){
 plot(cards_som, type = "property", property = unscaled, main=colnames(cards_som$data)[i], palette.name=topo.colors, heatkey = F,  ncolors = length(unique(cards_sub[i])[,1]))
 }
 else{
 plot(cards_som, type = "property", property = unscaled, main=colnames(cards_som$data)[i], palette.name=topo.colors, heatkey = T, heatkeywidth = 0.5,  keepMargins = T, ncolors = length(unique(cards_sub[i])[,1]))
 }
 
 
 ############################################################################################################################33
 SEPERATE PLOTTING FOR CONTINOUS AND CATEGORICAL VARIABLES
 ###########################################################################################################################
 for(i in 1:ncol(cards_sub)){
mypath <- paste("C:/Users/bharath.goda/Desktop/som_plots/", i, ".png", sep = "")
png(file=mypath, width=2000,height=1200)
unscaled <- aggregate(as.numeric(cards_sub[,i]), by=list(cards_som$unit.classif), FUN=mean, simplify=TRUE)[,2]
 if(colnames(cards_som$data)[i] %in% z){
 plot(cards_som, type = "property", property = unscaled, main = colnames(cards_som$data)[i], palette.name=coolBlueHotRed, heatkey = F,  ncolors = length(unique(cards_sub[,i])), cex = 2, font.main= 4)
 legend("left", legend = levels(cards_sub2[,i]), fill = coolBlueHotRed(length(unique(cards_sub[,i]))), cex = 2,  bty = "n")
   title(colnames(cards_som$data)[i], cex.main = 3)
 }
 else{
 plot(cards_som, type = "property", property = unscaled, main=colnames(cards_som$data)[i], palette.name=coolBlueHotRed, heatkey = T, heatkeywidth = 0.6,  cex = 2, font.main= 4)
  title(colnames(cards_som$data)[i], cex.main = 3)
 }
dev.off()
 }
 


 
 
library(kohonen)
data <- knime.in
k ## columns which are actually numeric but are in character/factor
data[k]  <- lapply(data[k], as.character)
data[k]  <- lapply(data[k], as.numeric)
i <- sapply(data, is.factor)
data[i] <- sapply(data[i], as.numeric)
l ## columns which have to be considered
data_sub <- data[l]
data_scaled <- scale(data_sub)
z <- names(data[j])
knime.model <- som(data_scaled, grid = somgrid(xdim, ydim, topo = "hexagonal"), rlen = 100, alpha = c(0.05, 0.01),
    radius = quantile(nhbrdist, 0.67) * c(1, -1), init,
    toroidal = FALSE, n.hood, keep.data = TRUE)
 
 
 
 
####################################################################################
COOL BLUE HOT RED
#####################################################################################3
 coolBlueHotRed <- function(n, alpha = 1) {
  rainbow(n, end=4/6, alpha=alpha)[n:1]
}



####################################################################################
KNIME MODEL GENERAL
#####################################################################################3
unscaled <- aggregate(as.numeric(data[,i]), by=list(knime.model$unit.classif), FUN=mean, simplify=TRUE)[,2]
 if(colnames(knime.model$data)[i] %in% z){
 plot(knime.model, type = "property", property = unscaled, main = colnames(knime.model$data)[i], palette.name=coolBlueHotRed, heatkey = F,  ncolors = length(unique(data[,i])), cex = 2, font.main= 4)
 legend("left", legend = levels(data2[,i]), fill = coolBlueHotRed(length(unique(data[,i]))), cex = 2,  bty = "n")
 title(colnames(knime.model$data)[i], cex.main = 3)
 }
 else{
 plot(knime.model, type = "property", property = unscaled, main=colnames(knime.model$data)[i], palette.name=coolBlueHotRed, heatkey = T, heatkeywidth = 0.6,  cex = 2, font.main= 4)
  title(colnames(knime.model$data)[i], cex.main = 3)
 }
 
 
####################################################################################
KNIME MODEL MANUAL
#####################################################################################3
 unscaled <- aggregate(as.numeric(data[,1]), by=list(knime.model$unit.classif), FUN=mean, simplify=TRUE)[,2]
 if(colnames(knime.model$data)[1] %in% z){
 plot(knime.model, type = "property", property = unscaled, main = colnames(knime.model$data)[5], palette.name=coolBlueHotRed, heatkey = F,  ncolors = length(unique(data[,5])), cex = 2, font.main= 4)
 legend("left", legend = levels(data2[,5]), fill = coolBlueHotRed(length(unique(data[,5]))), cex = 2,  bty = "n")
 title(colnames(knime.model$data)[5], cex.main = 3)
 }
 else{
 plot(knime.model, type = "property", property = unscaled, main=colnames(knime.model$data)[5], palette.name=coolBlueHotRed, heatkey = T, heatkeywidth = 0.6,  cex = 2, font.main= 4)
  title(colnames(knime.model$data)[5], cex.main = 3)
 }
 

 
 
 
 
####################################################################################
mapping manual
#####################################################################################3
 
 coolBlueHotRed <- function(n, alpha = 1) {
  rainbow(n, end=4/6, alpha=alpha)[n:1]
}
plot(knime.model, type="mapping", 
     labels = factor(data[[3]]), col = as.numeric(factor(data[[3]])) + 1,
     main = "mapping plot", palette.name=coolBlueHotRed)
	 
	 
	  ####################################################################################
mapping general
#####################################################################################3
 
 coolBlueHotRed <- function(n, alpha = 1) {
  rainbow(n, end=4/6, alpha=alpha)[n:1]
}
plot(knime.model, type="mapping", 
     labels = factor(data[[colno]]), col = as.numeric(factor(data[[colno]])) + 1,
     main = "mapping plot", palette.name=coolBlueHotRed)
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
	 
z <- coolBlueHotRed(length(unique(cards_sub[,i])))	 
k <- function(x){
	return(z[x])}
	
	
	
	as.numeric(factor(data[[3]]))
	

for(i in 1:ncol(crime_data)){
mypath <- paste("C:/Users/bharath.goda/Desktop/som_plots/", colnames(cards_som$data)[i], ".png", sep = "")
png(file=mypath, width=2000,height=1200)
unscaled <- aggregate(as.numeric(crime_data[,i]), by=list(cards_som$unit.classif), FUN=mean, simplify=TRUE)[,2]
 if(colnames(cards_som$data)[i] %in% z){
 plot(cards_som, type = "property", property = unscaled, main = colnames(cards_som$data)[i], palette.name=coolBlueHotRed, heatkey = F,  ncolors = length(unique(crime_data[,i])), cex = 2, font.main= 4)
 legend("left", legend = levels(crime_data_2[,i]), fill = coolBlueHotRed(length(unique(crime_data[,i]))), cex = 2,  bty = "n")
   title(colnames(cards_som$data)[i], cex.main = 3)
 }
 else{
 plot(cards_som, type = "property", property = unscaled, main=colnames(cards_som$data)[i], palette.name=coolBlueHotRed, heatkey = T, heatkeywidth = 0.6,  cex = 2, font.main= 4)
  title(colnames(cards_som$data)[i], cex.main = 3)
 }
dev.off()
 }
 

	
	
	

	
	
	
	
############################################################################################################3
TA-FENG DATASET
#############################################################################################################
library(dplyr)
library(data.table)
library(ggplot2)
library(Hmisc)
library(kohonen)
setwd("D:/FRACTAL/SOM/DublinR_ClusteringMiniProject-master/data/TaFengDataSet")
colsdt <- data.table(names=c("trans_date","cust_id","age_band"
                             ,"res_area","prod_cat","prod_id"
                             ,"quantity","asset","price")
                     ,types=c("character","character","character"
                              ,"character","character","character"
                              ,"numeric","character","numeric"))
ta_1 <- fread("D01.csv", stringsAsFactors = F, colClasses = colsdt$types, col.names = colsdt$names)
ta_2 <- fread("D02.csv", stringsAsFactors = F, colClasses = colsdt$types, col.names = colsdt$names)
ta_3 <- fread("D11.csv", stringsAsFactors = F, colClasses = colsdt$types, col.names = colsdt$names)
ta_4 <- fread("D12.csv", stringsAsFactors = F, colClasses = colsdt$types, col.names = colsdt$names)
ta <- rbind(ta_3, ta_4, ta_1, ta_2)
ta$asset <- sapply(ta$asset, as.numeric)
ta$asset <- sapply(ta$asset, as.numeric)
ta <- ta[,c("total_no_trans", "total_no_products", "total_no_visits", "total_unique_products",
            "total_unique_subclass", "total_spend") := .(.N, sum(quantity), length(unique(trans_date)), length(unique(prod_id)), 
                                          length(unique(prod_cat)), sum(quantity*price)), by = cust_id]
ta <- ta[,c("no_trans_day", "no_products_day", "unique_products_day", "unique_subclass_day", "spend_day")
         := .(.N, sum(quantity), length(unique(prod_id)), length(unique(prod_cat)), quantity*price), by = .(cust_id, trans_date)]
ta <- ta[, ":=" (max_trans = max(no_products_day), min_trans = min(no_products_day), 
                 med_trans = median(no_products_day), max_spend = max(spend_day), min_spend = min(spend_day), 
                 med_spend = median(spend_day)), by = cust_id]
ta[, profit := .((price-asset)*quantity)]
ta[, profit_day := sum(profit), by = .(cust_id, trans_date)]
ta[, total_profit := sum(profit), by = .(cust_id)]
ta[, day := as.POSIXlt(trans_date)$wday]
ta[,weekend := ifelse(day %in% c(0,6), T, F)]
# ta[weekend == TRUE, weekend_number := (.N), by = cust_id]
 ta[weekend == TRUE, weekend_percent :=((.N)/total_no_trans), by = cust_id]
# ta[, weekend_percent :=(weekend_number/total_no_trans), by = cust_id]
ta[,nbask := .N,by=list(prod_cat,prod_id)]
ta[,product_class := LETTERS[as.numeric(cut2(nbask,g=10))]]
ta[, nbask := .N,by=list(prod_cat,prod_id)]

k <- function(x){
  d = 0
  if (x %in% c("F", "G", "H", "I", "J")) {d = 1}
  return(d)
}

ta[, temp := sapply(ta$product_class, k)]
ta[, premium_percent := .(sum(temp)/.N), by = cust_id]
ta[, budget_percent := 1 - premium_percent, by = cust_id]

na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}


select <- dplyr::select
ta_sub <- select(ta, c(2,3,4,10, 11, 4, 15, 21:26, 29, 36, 37))
ta_sub <- unique(ta_sub)
# ta$weekend_percent <- sapply(ta$weekend_percent, na.zero)

ta_sub <- select(ta_sub, -1)
zzz <- ta_sub
zzz <- data.frame(zzz)
ta_sub[, res_area := sapply(res_area, as.factor)]
ta_sub[, res_area := sapply(res_area, as.numeric)]
ta_sub[, age_band := sapply(age_band, as.factor)]
ta_sub[, age_band := sapply(age_band, as.numeric)]

i <- sapply(zzz, is.character)
z <- names(zzz[i])

ta_K <- setkey(ta, cust_id)

ta_z <- ta[,c("total_no_trans", "total_no_products", "total_spend") := .(.N, sum(quantity), sum(quantity*price)), by = cust_id]


ta_sub_scaled <- scale(ta_sub)
cards_scaled <- ta_sub_scaled
cards_som <- som(cards_scaled, grid = somgrid(20, 20, "hexagonal"), rlen = 150)




for(i in 1:ncol(ta_sub)){
  mypath <- paste("C:/Users/bharath.goda/Desktop/som_plots/tafeng/", colnames(cards_som$data)[i], ".png", sep = "")
  png(file=mypath, width=2000,height=1200)
  unscaled <- aggregate(as.numeric(ta_sub[,i]), by=list(cards_som$unit.classif), FUN=mean, simplify=TRUE)[,2]
  if(colnames(cards_som$data)[i] %in% z){
    plot(cards_som, type = "property", property = unscaled, main = "", palette.name=rainbow, heatkey = F,  ncolors = length(unique(ta_sub[,i])), cex = 2, font.main= 4)
    legend("left", legend = unique(zzz[,i]), fill = rainbow(length(unique(ta_sub[,i]))), cex = 2,  bty = "n")
    title(colnames(cards_som$data)[i], cex.main = 3)
  }
  else{
    plot(cards_som, type = "property", property = unscaled, main="", palette.name=rainbow, heatkey = T, heatkeywidth = 0.6,  cex = 2, font.main= 4)
    title(colnames(cards_som$data)[i], cex.main = 3)
  }
  dev.off()
}



capVector <- function(x, probs = c(0.02,0.98)){
  #remove things above and below the percentiles specified
  ranges <- quantile(x, probs=probs, na.rm=T)
  x[x < ranges[1]] <- ranges[1]
  x[x > ranges[2]] <- ranges[2]
  return(x)
}

ooo <- ta_sub
ooo <- sapply(ooo, capVector)

ta_sub_scaled <- scale(ooo)
cards_scaled <- ta_sub_scaled
cards_som <- som(cards_scaled, grid = somgrid(20, 20, "hexagonal"), rlen = 500)
ooo_k <- ooo


for(i in 1:ncol(ooo_k)){
  mypath <- paste("C:/Users/bharath.goda/Desktop/som_plots/tafeng_new3/", colnames(cards_som$data)[i], ".png", sep = "")
  png(file=mypath, width=2000,height=1200)
  unscaled <- aggregate(as.numeric(ooo_k[,i]), by=list(cards_som$unit.classif), FUN=mean, simplify=TRUE)[,2]
  if(colnames(cards_som$data)[i] %in% z){
    plot(cards_som, type = "property", property = unscaled, main = "", palette.name= colorRampPalette(brewer.pal(length(unique(ooo_k[,i])),"Set1")), heatkey = F,  ncolors = length(unique(ooo_k[,i])), cex = 2, font.main= 4)
    legend("left", legend = unique(zzz[,i]), fill = brewer.pal(length(unique(ooo_k[,i])),"Set1"), cex = 2,  bty = "n")
    title(colnames(cards_som$data)[i], cex.main = 3)
  }
  else{
    plot(cards_som, type = "property", property = unscaled, main="", palette.name=rainbow, heatkey = T, heatkeywidth = 0.6,  cex = 2, font.main= 4)
    title(colnames(cards_som$data)[i], cex.main = 3)
  }
  dev.off()
}



library(RColorBrewer)
purples_palette <- brewer.pal(9, "Purples")
pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2')

## som_cluster <- clusterSOM(ClusterCodebooks=cards_som$codes,
                          LengthClustering=nrow(cards_som$codes),
                          Mapping=cards_som$unit.classif,
                          MapRow=cards_som$grid$xdim,
                          MapColumn=cards_som$grid$ydim,
                          StoppingCluster=2 )[[5]]


"changes",  "codes", "counts", "dist.neighbours"
type = c("changes",  "codes","counts", "dist.neighbours")
for (i in 1: length(type)){
  mypath <- paste("C:/Users/bharath.goda/Desktop/som_plots/tafeng_new3/", type[i], ".png", sep = "")
  png(file=mypath, width=2000,height=1200) 
  plot(cards_som, type = type[i], main = "", palette.name= colorRampPalette(brewer.pal(7, "Blues")))
  title(type[i], cex.main = 3)
  dev.off()
}


mypath <- paste("C:/Users/bharath.goda/Desktop/som_plots/tafeng_new2/", clusters, ".png", sep = "")
png(file=mypath, width=2000,height=1200)

som_cluster <- cutree(hclust(dist(cards_som$codes)), 6)
plot(cards_som, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters")
add.cluster.boundaries(cards_som, som_cluster)
dev.off()





for(i in 1:ncol(cards_sub)){
  mypath <- paste("C:/Users/bharath.goda/Desktop/som_plots/hrdata2/", colnames(cards_som$data)[i], ".png", sep = "")
  png(file=mypath, width=2000,height=1200)
  unscaled <- aggregate(as.numeric(cards_sub[,i]), by=list(cards_som$unit.classif), FUN=mean, simplify=TRUE)[,2]
  if(colnames(cards_som$data)[i] %in% z){
    plot(cards_som, type = "property", property = unscaled, main = "", palette.name=coolBlueHotRed, heatkey = F,  ncolors = length(unique(cards_sub[,i])), cex = 2, font.main= 4)
    legend("left", legend = levels(cards_sub2[,i]), fill = coolBlueHotRed(length(unique(cards_sub[,i]))), cex = 2,  bty = "n")
    title(colnames(cards_som$data)[i], cex.main = 3)
  }
  else{
    plot(cards_som, type = "property", property = unscaled, main="", palette.name=coolBlueHotRed, heatkey = T, heatkeywidth = 0.6,  cex = 2, font.main= 4)
    title(colnames(cards_som$data)[i], cex.main = 3)
  }
  dev.off()
}




for(i in c(1,2,3,4,5,8)){
  k[,i] <- as.factor(k[,i])
}

data <- read.csv("C:/Users/bharath.goda/Downloads/Masked Employee Data for SOM csv.csv")
data_sub <- select(data, Band, Designation.Clubbed, Location, Client.Cleaned, Degree..cleaned., Fractal.Tenure, Total.Exp, Gender)

k <- data_sub
select <- dplyr::select
i <- sapply(k, is.factor)
kk <- k
k[i] <- sapply(k[i], as.character)

          ""              

k <- filter(k, Band != "")
k <- filter(k, Designation.Clubbed != "")
k <- filter(k, Degree..cleaned. != "~")
k <- filter(k, Location != "")
k <- filter(k, Client.Cleaned != "")
k <- filter(k, Degree..cleaned. != "")

kkk <- k
for(i in c(1,2,3,4,5,8)){
  k[,i] <- as.factor(k[,i])
}

i <- sapply(k, is.factor)
kkkk <- k

k[i] <- sapply(k[i], as.numeric)


cards_sub2 <- kkkk
cards_sub <- k

cards_scaled <- scale(cards_sub)
j <- sapply(cards_sub2, is.factor)
z <- names(cards_sub[j])

cards_som <- som(cards_scaled, grid = somgrid(6, 6, "hexagonal"), rlen = 1000)



for(i in 1:ncol(cards_sub)){
  mypath <- paste("C:/Users/bharath.goda/Desktop/som_plots/hrdata4/", colnames(cards_som$data)[i], ".png", sep = "")
  png(file=mypath, width=2000,height=1200)
  unscaled <- aggregate(as.numeric(cards_sub[,i]), by=list(cards_som$unit.classif), FUN=mean, simplify=TRUE)[,2]
  if(colnames(cards_som$data)[i] %in% z){
    plot(cards_som, type = "property", property = unscaled, main = "", palette.name=rainbow, heatkey = F,  ncolors = length(unique(cards_sub[,i])), cex = 2, font.main= 4)
    legend("left", legend = levels(cards_sub2[,i]), fill = rainbow(length(unique(cards_sub[,i]))), cex = 2,  bty = "n")
    title(colnames(cards_som$data)[i], cex.main = 3)
  }
  else{
    plot(cards_som, type = "property", property = unscaled, main="", palette.name=rainbow, heatkey = T, heatkeywidth = 0.6,  cex = 2, font.main= 4)
    title(colnames(cards_som$data)[i], cex.main = 3)
  }
  dev.off()
}




for(i in 1:ncol(cards_sub)){
  mypath <- paste("C:/Users/bharath.goda/Desktop/som_plots/hrdata5/", colnames(cards_som$data)[i], ".png", sep = "")
  png(file=mypath, width=2000,height=1200)
  unscaled <- aggregate(as.numeric(cards_sub[,i]), by=list(cards_som$unit.classif), FUN=mean, simplify=TRUE)[,2]
  if(colnames(cards_som$data)[i] %in% z){
    plot(cards_som, type = "property", property = unscaled, main = "", palette.name=rainbow, heatkey = T,  ncolors = length(unique(cards_sub[,i])), cex = 2, font.main= 4)
    
    title(colnames(cards_som$data)[i], cex.main = 3)
  }
  else{
    plot(cards_som, type = "property", property = unscaled, main="", palette.name=rainbow, heatkey = T, heatkeywidth = 0.6,  cex = 2, font.main= 4)
    title(colnames(cards_som$data)[i], cex.main = 3)
  }
  dev.off()
}



type = c("changes",  "codes", "counts", "dist.neighbours")
for (i in 1: length(type)){
  mypath <- paste("C:/Users/bharath.goda/Desktop/som_plots/tafeng_new3/", type[i], ".png", sep = "")
  png(file=mypath, width=2000,height=1200) 
  plot(cards_som, type = type[i], main = "", palette.name= colorRampPalette(brewer.pal(7, "Blues")))
  title(type[i], cex.main = 3)
  dev.off()
}

