##################################################################
#     name: Andrew.kim
#     description: Anormaly Detection method
#     method: Local Outlier Factor(LOF) Algorithm
#     date: 2014.05.09
#     package: DMwR
#     Using data:
#       1. hbk
#       2. bushfire
#       3. wine data
##################################################################

# LOF Algorithm Example
# install.packages("DMwR")
require(DMwR)

##################################################################
# Using data: hbk
##################################################################

### Data setting
data <- hbk[,1:3]


### Test
lof.scores <- lofactor(data, 3)   # LOF Algorithm
  
lof_list <- cbind(data, lof.scores)
lof_list$color <- ifelse(lof_list$lof.scores > 1.5, 2, 1)
  
outlier_rows <- which(lof_list$lof.scores > 1.5)
outlier_count <- length(outlier_rows)
  
plot(data, col = lof_list$color, pch=16)  



##################################################################
# Using data: bushfire data
# Outlier = 7, 8, 9, 10, 11, 32, 33, 34, 35, 36, 37, 38
# best k: 9
##################################################################

require(robustbase)
data <- bushfire[, 1:5]
outlier_data <- c(7:11, 32, 33:38)

bushfire$out <- 1
bushfire$out[outlier_data] <- 2
plot(bushfire[,1:5], col=bushfire$out)


### Init Setting
kmax <- 10  # k of K-NN

outlier_list <- vector('list', kmax)
names(outlier_list) <- paste0('k=', seq_along(outlier_list))


### Simulation
for(i in 3:kmax){
  print(i)
  
  lof.scores <- lofactor(data, i)   # LOF Algorithm
  
  lof_list <- cbind(data, lof.scores)
  lof_list$color <- ifelse(lof_list$lof.scores > 1.5, 2, 1)
  lof_list$color <- ifelse(is.na(lof_list$lof.scores), 1, lof_list$color)
  
  outlier_rows <- which(lof_list$lof.scores > 1.5)
  outlier_count <- length(outlier_rows)
  outlier_percent <- length(intersect(outlier_data, outlier_rows))/14
  
  outlier <- list(count = outlier_count, row_numbers = outlier_rows, percent = outlier_percent)
  outlier_list[[i]] <- outlier
  
  plot(lof_list[, 1:5], col = lof_list$color, pch=16)  
}


### plotting
ppp <- data.frame(matrix(rep(0, kmax*2), ncol=2))
colnames(ppp) <- c("k", "percent")
for (i in 3:kmax){
  ppp[i, 1] <- i
  ppp[i, 2] <- round(outlier_list[[i]]$percent*100, 3)
}

plot(ppp$percent, pch=16, type="o", ylab="%", xlab="k", ylim=c(0,100), xlim=c(3,10))



##################################################################
# Using data: Wine Quality data
# Data Normalization
##################################################################

require(DMwR)
require(clusterSim)

### data load
red_wine <- read.csv("C:/Users/Andrew/Desktop/winequality-red.csv", header=T, sep=";")
white_wine <- read.csv("C:/Users/Andrew/Desktop/winequality-white.csv", header=T, sep=";")
wine_data <- data.frame(rbind(red_wine, white_wine))
# plot(wine_data)

### Data setting
raw_data <- wine_data[1:100, c(5,10)] # variable: 2
data <- data.Normalization(raw_data, type="n1", normalization="column")

# data <- raw_data
# plot(data)

### Init Setting
kmax <- 10  # k of K-NN

outlier_list <- vector('list', kmax)
names(outlier_list) <- paste0('k=', seq_along(outlier_list))


### Simulation
par(mfrow=c(3,3))
for(i in 3:kmax){
  print(i)
  
  lof.scores <- lofactor(data, i)   # LOF Algorithm
  
  lof_list <- cbind(raw_data, lof.scores)
  lof_list$color <- ifelse(lof_list$lof.scores > 1.5, 2, 1)
  lof_list$color <- ifelse(is.na(lof_list$lof.scores), 1, lof_list$color)
  
  outlier_rows <- which(lof_list$lof.scores > 1.5)
  outlier_count <- length(outlier_rows)
  
  outlier <- list(count = outlier_count, row_numbers = outlier_rows)
  outlier_list[[i]] <- outlier
  
  plot_title <- paste("k = ", i, sep="")
  plot(lof_list[ ,1:2], col = lof_list$color, pch=16, main = plot_title)  
}


### Normaly Percent Plot
## Data-Set for NPP (Normaly Percent Plot)
ppp <- data.frame(rep(0, kmax), ncol=2)
colnames(ppp) <- c("k", "count")
for (i in 3:kmax){
  ppp[i, 1] <- i
  ppp[i, 2] <- outlier_list[[i]]$count
}

## Plotting
plot(1-(ppp$count/nrow(data)), type="l", ylab="Normaly (%)", xlab="k")

par(mfrow=c(1,1))




