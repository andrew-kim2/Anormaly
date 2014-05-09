##################################################################
#     name: Andrew.kim
#     description: Anormaly Detection method
#     method: Local Outlier Factor(LOF) Algorithm
#     date: 2014.05.09
#     package: DMwR
#     Using data:
#       1. iris
#       2. 99' KDD-CUP data
##################################################################

# LOF Algorithm Example
# install.packages("DMwR")
require(DMwR)

##################################################################
# Using data: iris
##################################################################

### Data setting
data(iris)
data <- iris[,1:4]
data[1,1] <- 10


### Init Setting
kmax <- 50  # k of K-NN

outlier_list <- vector('list', kmax)
names(outlier_list) <- paste0('k=', seq_along(outlier_list))


### Simulation
for(i in 1:kmax){
  lof.scores <- lofactor(data, i)   # LOF Algorithm
  
  lof_list <- cbind(iris[, 1:2], lof.scores)
  lof_list$color <- ifelse(lof_list$lof.scores < 1.5, 1, 2)
  
  outlier_rows <- which(lof_list$lof.scores > 1.5)
  outlier_count <- length(outlier_rows)
  
  outlier <- list(count = outlier_count, row_numbers = outlier_rows)
  outlier_list[[i]] <- outlier
  
  plot(data, col = lof_list$color, pch=16)  
}


### Normaly Percent Plot
## Data-Set for NPP (Normaly Percent Plot)
ppp <- data.frame(rep(0, kmax), ncol=2)
colnames(ppp) <- c("k", "count")
for (i in 1:kmax){
  ppp[i, 1] <- i
  ppp[i, 2] <- outlier_list[[i]]$count
}

## Plotting
plot(1-(ppp$count/nrow(data)), type="l", ylab="Normaly (%)", xlab="k")


##################################################################
# Using data: 99' KDD-CUP data
##################################################################



