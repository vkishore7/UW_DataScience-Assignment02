# KMeansNorm.R
# Copyright 2017 by Ernst Henle

source("/Users/vvemulapalli/Documents/Assignment/Assignment2/KMeansHelper.R") # ClusterPlot. Samples for observations, clusterCenters, and labels.

KMeansNorm <- function(observations = sampleObservations, clusterCenters = centersGuess, normD1 = F, normD2 = F)
{
  if (normD1)
  {
    # Determine mean and standard deviation of 1st dimension in observations
    observationsMean1 <- mean(observations[,1])
    observationsSd1 <- sd(observations[,1])
    # normalize 1st dimension of observations
    observations[,1] <- (observations[,1] - observationsMean1)/observationsSd1
    # normalize 1st dimension of clusterCenters
    clusterCenters[,1] <- (clusterCenters[,1] - observationsMean1)/observationsSd1
  }
  if (normD2)
  {
    # Determine mean and standard deviation of 2nd dimension in observations
    observationsMean2 <- mean(observations[,2])
    observationsSd2 <- sd(observations[,2])
    # normalize 2nd dimension of observations
    observations[,2] <- (observations[,2] - observationsMean2)/observationsSd2
    # normalize 2nd dimension of clusterCenters
    clusterCenters[,2] <- (clusterCenters[,2] - observationsMean2)/observationsSd2
  }
  #clusterCenters <- KMeans(observations, clusterCenters)
  KMeansResult <- kmeans(x=observations, centers=clusterCenters)
  clusterCenters <- KMeansResult$centers
  ClusterPlot(observations=observations, centers=clusterCenters, labels=KMeansResult$cluster)
  if (normD1)
  {
    # denormalize in first dimension
    clusterCenters[,1] <- clusterCenters[,1]*sd(observations[,1])+mean(observations[,1])
  } 
  if (normD2)
  {
    # denormalize in second dimension
    clusterCenters[,2] <- clusterCenters[,2]*sd(observations[,2])+mean(observations[,2])
  } 
  return(clusterCenters)
}

# Q4a
# 

# Q4b
# 
# 

# Q4c
# 

# Q4d
# 

# Q4e
# 

# Q5
# 

# Q6
# 

# Q7
# 
